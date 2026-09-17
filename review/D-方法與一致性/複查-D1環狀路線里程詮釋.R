# ============================================================
# D1／C5 複查（續四）：環狀路線負里程的可能情形，以及哪一種算式與票證時間戳相容
#
# 審查人 2026-09-14 提出的可能情形：
#   (1) 順向且未過終點 → 下車累積 − 上車累積 > 0，無爭議
#   (2) 順向但坐過終點再到下車站 → 上車站序 > 下車站序 → 差為負
#   (3) 到終點後改逆向行駛到下車站
# 本腳本檢驗這些情形，並加入第 (4) 種：距離表把同一實體站牌列兩次，
#   票證帶哪一次出現的代碼，決定了被定位到哪一段，與乘客實際怎麼搭無關。
#
# 關鍵檢驗：若情形 (2) 成立（真的坐過終點），閉合表上正確算式是
#     環繞 = 全長 − 上車累積 + 下車累積
#   而不是 abs(下車 − 上車)。兩者差一個「終點折返段」。
#   若情形 (4) 成立，正確值是同站名另一次出現所給的短距離。
#   → 先算出三種值，再用「刷卡上車／下車時間」的時距分辨哪一種與資料相容。
#     時間戳僅到小時（D2），但足以分辨 20 分鐘與 3 小時等級的差別。
#
# 另計算丁案（閉合表負值改用環繞公式）對月平均的影響，與甲／乙／丙並列。
#
# 輸出：output/D1-詮釋-閉合表清單.csv、output/D1-詮釋-負值票證分類.csv、
#       output/D1-詮釋-時距對照.csv、output/D1-詮釋-四案月平均.csv、同名 .log
# 耗時：約 4 分鐘
# ============================================================

suppressPackageStartupMessages({ library(data.table) })

this_file <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
if (length(this_file) == 0) this_file <- "複查-D1環狀路線里程詮釋.R"
code_dir  <- normalizePath(dirname(this_file))
REPO_ROOT <- normalizePath(file.path(code_dir, "..", ".."))
source(file.path(REPO_ROOT, "review", "A-資料缺漏與異常", "00-setup.R"))
out_dir <- file.path(code_dir, "output")
t0 <- Sys.time()

COLS <- c("搭乘路線名稱", "搭乘附屬路線名稱", "搭乘公車路線方向",
          "上車站牌代碼", "上車站牌名稱", "下車站牌代碼", "下車站牌名稱",
          "上車計費站序資料", "下車站牌站序", "刷卡上車時間", "刷卡下車時間")
KEY <- c("搭乘路線名稱", "搭乘附屬路線名稱", "搭乘公車路線方向")

read_dist <- function(f) {
  d <- fread(file.path(data_path, "公車站間距離資料", f), colClasses = list(character = 1:5))
  strip_bom(d)
  d[, 站序資料 := as.numeric(站序資料)]
  d[, 站間距離 := as.numeric(站間距離)]
  d[is.na(站間距離), 站間距離 := 0]
  setorder(d, 搭乘路線名稱, 搭乘附屬路線名稱, 搭乘公車路線方向, 站序資料)
  d[, 累積里程 := cumsum(站間距離), by = KEY]
  d[]
}
dists <- list(花蓮公路 = read_dist("花蓮縣公路客運站間距離資料.csv"),
              臺東公路 = read_dist("臺東縣公路客運站間距離資料.csv"),
              花蓮市區 = read_dist("花蓮縣市區客運站間距離資料.csv"),
              臺東市區 = read_dist("臺東縣市區客運站間距離資料.csv"))

prep <- function(d) {
  d <- in_window(d)
  d[, 搭乘路線名稱     := trimws(as.character(搭乘路線名稱))]
  d[, 搭乘附屬路線名稱 := trimws(as.character(搭乘附屬路線名稱))]
  d[, 搭乘公車路線方向 := as.character(搭乘公車路線方向)]
  d[, 上車碼 := suppressWarnings(as.character(as.numeric(gsub("[^0-9]", "", 上車站牌代碼))))]
  d[, 下車碼 := suppressWarnings(as.character(as.numeric(gsub("[^0-9]", "", 下車站牌代碼))))]
  d[, 上車站序 := suppressWarnings(as.numeric(上車計費站序資料))]
  d[, 下車站序 := suppressWarnings(as.numeric(下車站牌站序))]
  d[, 上車時 := as.POSIXct(刷卡上車時間, tz = "UTC")]
  d[, 下車時 := as.POSIXct(刷卡下車時間, tz = "UTC")]
  d[, 時距_小時 := as.numeric(difftime(下車時, 上車時, units = "hours"))]
  d[, 月份 := 民國月份]
  d[]
}
公路   <- prep(read_tickets("公路客運2024_to_202606.csv", COLS))
花蓮市 <- prep(read_tickets("花蓮縣公車.csv", COLS))
臺東市 <- prep(read_tickets("臺東縣公車.csv", COLS))
cat("讀取完成：", format(round(difftime(Sys.time(), t0, units = "secs"))), "\n")

sets <- list(
  花蓮公路 = 公路[搭乘路線名稱 %in% routes_hualien_THB],
  臺東公路 = 公路[搭乘路線名稱 %in% routes_taitung_THB],
  花蓮市區 = 花蓮市[!搭乘路線名稱 %in% hualien_abnormal_routes],
  臺東市區 = 臺東市[!(get(DATE_COL) %in% taitung_abnormal_dates) | 搭乘路線名稱 %in% taitung_normal_routes]
)

# ------------------------------------------------------------
# 站名對照（由票證取各代碼最常見的站名）
# ------------------------------------------------------------
name_map <- function(tk) {
  rbind(tk[, .(代碼 = 上車碼, 站名 = 上車站牌名稱)], tk[, .(代碼 = 下車碼, 站名 = 下車站牌名稱)])[
    !is.na(代碼) & 站名 != "", .N, by = .(代碼, 站名)][order(-N)][, .SD[1], by = 代碼][, .(代碼, 站名)]
}

# ------------------------------------------------------------
# 閉合判定：距離表首站與末站是否為同一實體站牌（同代碼或同站名）
# ------------------------------------------------------------
closure_rows <- list(); neg_rows <- list(); month_rows <- list()

match_code <- function(tk, cm) {
  side <- function(code_col, seq_col, pre) {
    c1 <- cm[, c(KEY, "站牌代碼", "站序資料", "累積里程"), with = FALSE]
    setnames(c1, c("站牌代碼", "站序資料", "累積里程"), c(code_col, "表站序", "候選"))
    t1 <- tk[!is.na(get(code_col)), c("rid", KEY, code_col, seq_col), with = FALSE]
    setnames(t1, seq_col, "票序")
    cand <- merge(t1, c1, by = c(KEY, code_col), allow.cartesian = TRUE, sort = FALSE)
    cand[, has_seq := !is.na(票序) & 票序 != -99]
    cand[, 站序差 := fifelse(has_seq & !is.na(表站序), abs(票序 - 表站序), NA_real_)]
    cand[, min_diff := suppressWarnings(min(站序差, na.rm = TRUE)), by = rid]
    sel <- cand[!has_seq | (!is.na(站序差) & 站序差 == min_diff)]
    r <- sel[, .(v = if (uniqueN(候選) == 1) 候選[1] else NA_real_,
                 s = if (uniqueN(候選) == 1) 表站序[1] else NA_real_), by = rid]
    setnames(r, c("v", "s"), paste0(pre, c("里程", "表站序")))
    r
  }
  x <- merge(tk, side("上車碼", "上車站序", "上車"), by = "rid", all.x = TRUE)
  x <- merge(x,  side("下車碼", "下車站序", "下車"), by = "rid", all.x = TRUE)
  x[!is.na(上車里程) & !is.na(下車里程)]
}

matched <- list()
for (ds in names(sets)) {
  cat("\n============================================================\n", ds,
      "\n============================================================\n", sep = "")
  tk <- copy(sets[[ds]]); tk[, rid := .I]
  cm <- dists[[ds]]
  nm <- name_map(tk)

  # 閉合資訊
  ends <- cm[, .(首代碼 = 站牌代碼[which.min(站序資料)], 末代碼 = 站牌代碼[which.max(站序資料)],
                 站數 = .N, 全長 = max(累積里程)), by = KEY]
  ends <- merge(ends, nm[, .(首代碼 = 代碼, 首站名 = 站名)], by = "首代碼", all.x = TRUE)
  ends <- merge(ends, nm[, .(末代碼 = 代碼, 末站名 = 站名)], by = "末代碼", all.x = TRUE)
  ends[, 閉合 := 首代碼 == 末代碼 | (!is.na(首站名) & !is.na(末站名) & 首站名 == 末站名)]
  cat("距離表共 ", nrow(ends), " 個「路線×附屬×方向」，其中首末站相同（閉合）", sum(ends$閉合, na.rm = TRUE), " 個\n", sep = "")
  print(ends[閉合 == TRUE, .(搭乘附屬路線名稱, 搭乘公車路線方向, 站數, 全長_公里 = round(全長 / 1000, 2), 首站名, 末站名)])
  ends[, 資料集 := ds]; closure_rows[[ds]] <- ends[閉合 == TRUE]

  x <- match_code(tk, cm)
  x <- merge(x, ends[, c(KEY, "閉合", "全長"), with = FALSE], by = KEY, all.x = TRUE)
  x[is.na(閉合), 閉合 := FALSE]
  x[, 差 := 下車里程 - 上車里程]
  x[, 環繞 := fifelse(閉合, 全長 - 上車里程 + 下車里程, NA_real_)]
  matched[[ds]] <- x

  # 負值票證分類
  neg <- x[差 < 0, .(負值票證 = .N,
                     abs平均_公里 = round(mean(abs(差)) / 1000, 2),
                     環繞平均_公里 = round(mean(環繞, na.rm = TRUE) / 1000, 2)), by = 閉合]
  cat("\n負值票證依距離表是否閉合：\n"); print(neg)
  cat(sprintf("全體代碼配到 %d 筆；負值 %d 筆（%.2f%%），其中落在閉合表者 %.1f%%\n",
              nrow(x), x[差 < 0, .N], 100 * x[, mean(差 < 0)],
              100 * x[差 < 0 & 閉合 == TRUE, .N] / max(1, x[差 < 0, .N])))
  neg[, 資料集 := ds]; neg_rows[[ds]] <- neg

  # 四案月平均
  m <- Reduce(function(a, b) merge(a, b, by = "月份"), list(
    x[差 != 0, .(甲_abs = round(sum(abs(差)) / .N / 1000, 2)), by = 月份],
    x[差 > 0,  .(乙_丟負值 = round(sum(差) / .N / 1000, 2)), by = 月份],
    x[差 != 0 & !閉合, .(丙_排除閉合表 = round(sum(abs(差)) / .N / 1000, 2)), by = 月份],
    x[差 != 0, .(丁_閉合用環繞 = round(sum(fifelse(差 < 0 & 閉合, 環繞, abs(差))) / .N / 1000, 2)), by = 月份]
  ))
  cat("\n四案月平均里程（公里）：\n"); print(m)
  cat(sprintf("甲−丁：平均 %.2f、最大 %.2f（%s）\n",
              mean(m$甲_abs - m$丁_閉合用環繞), max(m$甲_abs - m$丁_閉合用環繞),
              m[which.max(甲_abs - 丁_閉合用環繞), 月份]))
  m[, 資料集 := ds]; month_rows[[ds]] <- m
}

fwrite(rbindlist(closure_rows, fill = TRUE), file.path(out_dir, "D1-詮釋-閉合表清單.csv"), bom = TRUE)
fwrite(rbindlist(neg_rows,   fill = TRUE), file.path(out_dir, "D1-詮釋-負值票證分類.csv"), bom = TRUE)
fwrite(rbindlist(month_rows, fill = TRUE), file.path(out_dir, "D1-詮釋-四案月平均.csv"), bom = TRUE)

# ------------------------------------------------------------
# 時距檢驗：同一組起迄站名、依票證代碼拆成兩群，時距是否相同
# 若兩群時距分布相同，兩群是同一種實際搭乘，里程卻差一整圈
# ------------------------------------------------------------
cat("\n============================================================\n",
    "時距檢驗（刷卡下車時間 − 刷卡上車時間，小時；時間戳僅到小時）\n",
    "============================================================\n", sep = "")
probes <- list(
  list(ds = "臺東公路", sub = "8101A", dir = "0",
       pairs = list(c("台東轉運站", "小野柳"), c("台東火車站", "小野柳"),
                    c("阿美族民俗中心", "成功漁港"), c("成功漁港", "三仙台遊憩區"),
                    c("小野柳", "阿美族民俗中心"))),
  list(ds = "花蓮市區", sub = "309", dir = "0",
       pairs = list(c("玉里火車站", "奚卜蘭遊客中心"), c("金剛好事館", "玉里火車站"),
                    c("奚卜蘭遊客中心", "石梯坪"), c("石梯坪", "北回歸線")))
)
dur_rows <- list()
for (pb in probes) {
  x <- matched[[pb$ds]][搭乘附屬路線名稱 == pb$sub & 搭乘公車路線方向 == pb$dir]
  cat("\n----- ", pb$ds, " ", pb$sub, " 方向 ", pb$dir, " -----\n", sep = "")
  for (pr in pb$pairs) {
    y <- x[上車站牌名稱 == pr[1] & 下車站牌名稱 == pr[2]]
    if (nrow(y) == 0) next
    d <- y[, .(票證數 = .N,
               上車表站序 = 上車表站序[1], 下車表站序 = 下車表站序[1],
               表差_公里 = round(差[1] / 1000, 2),
               abs_公里 = round(abs(差[1]) / 1000, 2),
               環繞_公里 = round(環繞[1] / 1000, 2),
               時距中位_小時 = median(時距_小時, na.rm = TRUE),
               時距平均_小時 = round(mean(時距_小時, na.rm = TRUE), 2),
               時距0小時占比 = round(mean(時距_小時 == 0, na.rm = TRUE) * 100, 1),
               時距2小時以上占比 = round(mean(時距_小時 >= 2, na.rm = TRUE) * 100, 1)),
           by = .(上車碼, 下車碼)][order(-票證數)]
    d[, `:=`(起 = pr[1], 迄 = pr[2], 資料集 = pb$ds, 附屬路線 = pb$sub)]
    cat("\n", pr[1], " → ", pr[2], "\n", sep = "")
    print(d[, .(上車碼, 上車表站序, 下車表站序, 票證數, 表差_公里, abs_公里, 環繞_公里,
                時距中位_小時, 時距平均_小時, 時距0小時占比, 時距2小時以上占比)])
    dur_rows[[length(dur_rows) + 1]] <- d
  }
}
fwrite(rbindlist(dur_rows, fill = TRUE), file.path(out_dir, "D1-詮釋-時距對照.csv"), bom = TRUE)

cat("\n已寫出 output/D1-詮釋-{閉合表清單,負值票證分類,時距對照,四案月平均}.csv\n")
cat("耗時：", format(round(difftime(Sys.time(), t0, units = "mins"), 1)), "\n")
