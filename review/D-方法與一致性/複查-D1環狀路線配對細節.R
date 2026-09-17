# ============================================================
# D1／C5 複查（續三）：環狀路線的票證到底是怎麼被配對的
#
# 問題（審查人 2026-09-14）：整併版第一層規則「同一站牌代碼有多筆候選時，取票證站序最接近者」
# 看起來合理，為什麼算出來的里程會是 119 公里。
#
# 本腳本拆開兩件事：
#   1. 規則實際被用到的比例 —— 有多少票證的站牌代碼在該路線方向的距離表裡本來就只有一個候選，
#      根本輪不到「取最接近」這條規則。
#   2. 同一組起迄站名（如 台東轉運站→小野柳）的票證，依其攜帶的「上車站牌代碼」拆開來看，
#      是否分成兩群、各自對到距離表中同一實體站牌的兩次出現、因而得到兩個差很多的里程。
#   另外計算：距離表中最大單段（8101A 方向 0 的站序 2→3）被多少票證的里程區間涵蓋。
#
# 只描述距離表與票證本身的內容；不判定哪個里程接近真實（需 TDX 站序座標或業者里程表，本審查無）。
#
# 輸出：output/D1-環狀-距離表站牌重複.csv、output/D1-環狀-候選數分布.csv、
#       output/D1-環狀-同站名依代碼拆分.csv、同名 .log
# 耗時：約 2 分鐘
# ============================================================

suppressPackageStartupMessages({ library(data.table) })

this_file <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
if (length(this_file) == 0) this_file <- "複查-D1環狀路線配對細節.R"
code_dir  <- normalizePath(dirname(this_file))
REPO_ROOT <- normalizePath(file.path(code_dir, "..", ".."))
source(file.path(REPO_ROOT, "review", "A-資料缺漏與異常", "00-setup.R"))
out_dir <- file.path(code_dir, "output")
t0 <- Sys.time()

COLS <- c("搭乘路線名稱", "搭乘附屬路線名稱", "搭乘公車路線方向",
          "上車站牌代碼", "上車站牌名稱", "下車站牌代碼", "下車站牌名稱",
          "上車計費站序資料", "下車站牌站序")
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
dists <- list(臺東公路 = read_dist("臺東縣公路客運站間距離資料.csv"),
              花蓮市區 = read_dist("花蓮縣市區客運站間距離資料.csv"))

prep <- function(d) {
  d <- in_window(d)
  d[, 搭乘附屬路線名稱 := trimws(as.character(搭乘附屬路線名稱))]
  d[, 搭乘公車路線方向 := as.character(搭乘公車路線方向)]
  d[, 上車碼 := suppressWarnings(as.character(as.numeric(gsub("[^0-9]", "", 上車站牌代碼))))]
  d[, 下車碼 := suppressWarnings(as.character(as.numeric(gsub("[^0-9]", "", 下車站牌代碼))))]
  d[, 上車站序 := suppressWarnings(as.numeric(上車計費站序資料))]
  d[, 下車站序 := suppressWarnings(as.numeric(下車站牌站序))]
  d[]
}
tickets <- list(臺東公路 = prep(read_tickets("公路客運2024_to_202606.csv", COLS)),
                花蓮市區 = prep(read_tickets("花蓮縣公車.csv", COLS)))

cases <- list(list(ds = "臺東公路", sub = "8101A", dir = "0"),
              list(ds = "花蓮市區", sub = "309",   dir = "0"))

dup_rows <- list(); cn_rows <- list(); sp_rows <- list()

for (cs in cases) {
  tag <- paste0(cs$ds, " ", cs$sub, " 方向 ", cs$dir)
  cat("\n============================================================\n", tag,
      "\n============================================================\n", sep = "")

  cm <- dists[[cs$ds]][搭乘附屬路線名稱 == cs$sub & 搭乘公車路線方向 == cs$dir]
  tk <- tickets[[cs$ds]][搭乘附屬路線名稱 == cs$sub & 搭乘公車路線方向 == cs$dir]

  # ---- 1. 距離表中重複出現的站牌代碼 ----
  dup <- cm[, .(出現次數 = .N, 站序 = paste(站序資料, collapse = "、"),
                累積里程_公里 = paste(round(累積里程 / 1000, 2), collapse = "、")), by = 站牌代碼][出現次數 > 1]
  nm <- tk[, .N, by = .(站牌代碼 = 上車碼, 站名 = 上車站牌名稱)][order(-N)][, .SD[1], by = 站牌代碼]
  dup <- merge(dup, nm[, .(站牌代碼, 站名)], by = "站牌代碼", all.x = TRUE)
  cat("\n距離表共 ", nrow(cm), " 列、", uniqueN(cm$站牌代碼), " 個不重複代碼；重複出現的代碼：\n", sep = "")
  print(dup)

  # 同名不同代碼（同一實體站牌在表中以兩個代碼出現）
  tb <- merge(cm[, .(站序資料, 站牌代碼, 累積里程_公里 = round(累積里程 / 1000, 2))],
              nm[, .(站牌代碼, 站名)], by = "站牌代碼", all.x = TRUE)[order(站序資料)]
  same_name <- tb[, .(代碼數 = uniqueN(站牌代碼), 代碼 = paste(unique(站牌代碼), collapse = "、"),
                      站序 = paste(站序資料, collapse = "、"),
                      累積里程_公里 = paste(累積里程_公里, collapse = "、")), by = 站名][代碼數 > 1 | grepl("、", 站序)]
  cat("\n距離表中出現一次以上的站名（含同名不同代碼）：\n"); print(same_name)
  cat("\n該表單段距離（公里）由大到小：\n")
  print(cm[order(-站間距離), .(站序資料, 站牌代碼, 站間距離_公里 = round(站間距離 / 1000, 2))][1:min(5, nrow(cm))])
  dup[, `:=`(資料集 = cs$ds, 附屬路線 = cs$sub, 方向 = cs$dir)]; dup_rows[[tag]] <- dup

  # ---- 2. 套用整併版第一層規則，記錄候選數與選中站序 ----
  pick <- function(code_col, seq_col, pre) {
    c1 <- cm[, .(配對碼 = 站牌代碼, 表站序 = 站序資料, 候選 = 累積里程)]
    t1 <- tk[!is.na(get(code_col)), .(rid = .I[!is.na(get(code_col))], 配對碼 = get(code_col), 票序 = get(seq_col))]
    t1 <- tk[, .(rid = .I, 配對碼 = get(code_col), 票序 = get(seq_col))][!is.na(配對碼)]
    cand <- merge(t1, c1, by = "配對碼", allow.cartesian = TRUE, sort = FALSE)
    cand[, 候選數 := .N, by = rid]
    cand[, has_seq := !is.na(票序) & 票序 != -99]
    cand[, 站序差 := fifelse(has_seq, abs(票序 - 表站序), NA_real_)]
    cand[, min_diff := suppressWarnings(min(站序差, na.rm = TRUE)), by = rid]
    sel <- cand[!has_seq | (!is.na(站序差) & 站序差 == min_diff)]
    r <- sel[, .(候選數 = 候選數[1], 選中站序 = if (uniqueN(候選) == 1) 表站序[1] else NA_real_,
                 里程 = if (uniqueN(候選) == 1) 候選[1] else NA_real_), by = rid]
    setnames(r, c("候選數", "選中站序", "里程"), paste0(pre, c("候選數", "選中站序", "里程")))
    r
  }
  tk[, rid := .I]
  x <- merge(tk, pick("上車碼", "上車站序", "上車"), by = "rid", all.x = TRUE)
  x <- merge(x,  pick("下車碼", "下車站序", "下車"), by = "rid", all.x = TRUE)
  x <- x[!is.na(上車里程) & !is.na(下車里程)]
  x[, 差 := 下車里程 - 上車里程]

  cn <- x[, .(票證數 = .N, 反向數 = sum(差 < 0), 反向占比 = round(mean(差 < 0) * 100, 1),
              abs里程_公里 = round(mean(abs(差)) / 1000, 2)),
          by = .(上車候選數, 下車候選數)][order(-票證數)]
  cat("\n配對候選數分布（候選數 = 1 表示該代碼在此表只出現一次，「取站序最接近」規則用不到）：\n")
  print(cn)
  cat(sprintf("兩端候選數皆為 1 的票證占 %.1f%%\n", 100 * x[上車候選數 == 1 & 下車候選數 == 1, .N] / nrow(x)))
  cat(sprintf("票證自帶站序與選中之表站序相同者占 %.1f%%（上車）、%.1f%%（下車）\n",
              100 * x[!is.na(上車站序) & 上車站序 == 上車選中站序, .N] / nrow(x),
              100 * x[!is.na(下車站序) & 下車站序 == 下車選中站序, .N] / nrow(x)))
  cn[, `:=`(資料集 = cs$ds, 附屬路線 = cs$sub, 方向 = cs$dir)]; cn_rows[[tag]] <- cn

  # ---- 3. 同一組起迄「站名」，依上車代碼拆開 ----
  top <- x[, .N, by = .(上車站牌名稱, 下車站牌名稱)][order(-N)][1:4]
  sp <- x[top, on = .(上車站牌名稱, 下車站牌名稱)][
    , .(票證數 = .N, 上車選中站序 = 上車選中站序[1], 下車選中站序 = 下車選中站序[1],
        上車累積里程_公里 = round(上車里程[1] / 1000, 2), 下車累積里程_公里 = round(下車里程[1] / 1000, 2),
        差_公里 = round(差[1] / 1000, 2), 取abs後_公里 = round(abs(差[1]) / 1000, 2)),
    by = .(上車站牌名稱, 下車站牌名稱, 上車碼, 下車碼)][order(上車站牌名稱, 下車站牌名稱, -票證數)]
  cat("\n最常見的四組起迄站名，依票證攜帶的站牌代碼拆開：\n")
  print(sp)
  sp[, `:=`(資料集 = cs$ds, 附屬路線 = cs$sub, 方向 = cs$dir)]; sp_rows[[tag]] <- sp

  # ---- 4. 最大單段被多少票證的里程區間涵蓋 ----
  big <- cm[which.max(站間距離)]
  lo <- big$站序資料
  x[, 跨越最大段 := pmin(上車選中站序, 下車選中站序) < lo & pmax(上車選中站序, 下車選中站序) >= lo]
  cat(sprintf("\n該表最大單段位於站序 %d→%d（%.2f 公里）。里程區間涵蓋此段的票證：%d 筆（%.1f%%），平均 abs 里程 %.2f 公里；\n未涵蓋者 %d 筆，平均 %.2f 公里。\n",
              lo - 1, lo, big$站間距離 / 1000,
              x[跨越最大段 == TRUE, .N], 100 * x[, mean(跨越最大段)], x[跨越最大段 == TRUE, mean(abs(差))] / 1000,
              x[跨越最大段 == FALSE, .N], x[跨越最大段 == FALSE, mean(abs(差))] / 1000))
  cat(sprintf("反向票證中涵蓋此段者 %.1f%%；正向票證中 %.1f%%\n",
              100 * x[差 < 0, mean(跨越最大段)], 100 * x[差 > 0, mean(跨越最大段)]))
  tk[, rid := NULL]
}

fwrite(rbindlist(dup_rows, fill = TRUE), file.path(out_dir, "D1-環狀-距離表站牌重複.csv"), bom = TRUE)
fwrite(rbindlist(cn_rows,  fill = TRUE), file.path(out_dir, "D1-環狀-候選數分布.csv"), bom = TRUE)
fwrite(rbindlist(sp_rows,  fill = TRUE), file.path(out_dir, "D1-環狀-同站名依代碼拆分.csv"), bom = TRUE)
cat("\n已寫出 output/D1-環狀-{距離表站牌重複,候選數分布,同站名依代碼拆分}.csv\n")
cat("耗時：", format(round(difftime(Sys.time(), t0, units = "mins"), 1)), "\n")
