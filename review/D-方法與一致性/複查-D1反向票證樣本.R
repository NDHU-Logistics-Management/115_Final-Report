# ============================================================
# D1 複查（續二）：反向票證（下車累積里程 < 上車累積里程）是什麼
#
# 承 複查-D1公路站序補配與反向樣本.R：臺東公路的反向票證集中在 8101A–D、8168A 方向 0，
# 取絕對值後平均 148 公里，遠高於同路線正向的 47 公里。本腳本看四個資料集的反向票證
# 集中在哪些路線／方向，以及 8101A 方向 0 的距離表與反向票證的上下車站名，
# 判斷 abs() 給的里程是否合理。
#
# 輸出：output/D1-反向票證-四資料集依路線方向.csv、output/D1-反向票證-8101A方向0距離表.csv、
#       output/D1-反向票證-8101A方向0起迄站.csv、同名 .log
# 耗時：約 2.5 分鐘
# ============================================================

suppressPackageStartupMessages({ library(data.table) })

this_file <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
if (length(this_file) == 0) this_file <- "複查-D1反向票證樣本.R"
code_dir  <- normalizePath(dirname(this_file))
REPO_ROOT <- normalizePath(file.path(code_dir, "..", ".."))
source(file.path(REPO_ROOT, "review", "A-資料缺漏與異常", "00-setup.R"))
out_dir <- file.path(code_dir, "output")
t0 <- Sys.time()

COLS <- c("業者編號", "搭乘路線名稱", "搭乘附屬路線名稱", "搭乘公車路線方向",
          "上車站牌代碼", "上車站牌名稱", "下車站牌代碼", "下車站牌名稱",
          "上車計費站序資料", "下車站牌站序", "原始票證筆數")
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
cum <- list(花蓮公路 = read_dist("花蓮縣公路客運站間距離資料.csv"),
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
  d[, 月份 := 民國月份]
  d[, tid := .I]
  d[]
}
公路   <- prep(read_tickets("公路客運2024_to_202606.csv", COLS))
花蓮市 <- prep(read_tickets("花蓮縣公車.csv", COLS))
臺東市 <- prep(read_tickets("臺東縣公車.csv", COLS))

# 代碼配對，同站牌多筆候選依站序差最小（整併版第一層）
match_code <- function(tk, cm) {
  side <- function(code_col, seq_col, out_col) {
    c1 <- cm[, c(KEY, "站牌代碼", "站序資料", "累積里程"), with = FALSE]
    setnames(c1, c("站牌代碼", "站序資料", "累積里程"), c(code_col, "距離表站序", "候選"))
    t1 <- tk[!is.na(get(code_col)), c("tid", KEY, code_col, seq_col), with = FALSE]
    setnames(t1, seq_col, "票證站序")
    cand <- merge(t1, c1, by = c(KEY, code_col), allow.cartesian = TRUE, sort = FALSE)
    cand[, has_seq := !is.na(票證站序) & 票證站序 != -99]
    cand[, 站序差 := fifelse(has_seq & !is.na(距離表站序), abs(票證站序 - 距離表站序), NA_real_)]
    cand[, min_diff := suppressWarnings(min(站序差, na.rm = TRUE)), by = tid]
    sel <- cand[!has_seq | (!is.na(站序差) & 站序差 == min_diff)]
    r <- sel[, .(v = if (uniqueN(候選) == 1) 候選[1] else NA_real_, s = 距離表站序[1]), by = tid]
    setnames(r, c("v", "s"), c(out_col, sub("里程", "表站序", out_col)))
    r
  }
  x <- merge(tk, side("上車碼", "上車站序", "上車里程"), by = "tid")
  x <- merge(x,  side("下車碼", "下車站序", "下車里程"), by = "tid")
  x[!is.na(上車里程) & !is.na(下車里程)]
}

sets <- list(
  花蓮公路 = 公路[搭乘路線名稱 %in% routes_hualien_THB],
  臺東公路 = 公路[搭乘路線名稱 %in% routes_taitung_THB],
  花蓮市區 = 花蓮市[!搭乘路線名稱 %in% hualien_abnormal_routes],
  臺東市區 = 臺東市[!(get(DATE_COL) %in% taitung_abnormal_dates) | 搭乘路線名稱 %in% taitung_normal_routes]
)

rev_rows <- list(); matched <- list()
for (ds in names(sets)) {
  x <- match_code(sets[[ds]], cum[[ds]])
  x[, 差 := 下車里程 - 上車里程]
  matched[[ds]] <- x
  tot <- nrow(x); nrev <- x[差 < 0, .N]
  cat(sprintf("\n===== %s：代碼配到 %d 列，反向 %d 列（%.2f%%），零 %d 列；反向取 abs 後平均 %.2f 公里、正向平均 %.2f 公里\n",
              ds, tot, nrev, 100 * nrev / tot, x[差 == 0, .N],
              x[差 < 0, mean(abs(差))] / 1000, x[差 > 0, mean(差)] / 1000))
  # 反向票證納入（abs）與不納入的月平均差
  m <- merge(x[差 > 0, .(不含反向 = sum(差) / .N / 1000), by = 月份],
             x[差 != 0, .(含反向abs = sum(abs(差)) / .N / 1000), by = 月份], by = "月份")
  m[, 差異 := round(含反向abs - 不含反向, 2)]
  cat("反向票證對月平均的影響（含 abs − 不含）：最大 ", max(m$差異), " 公里（", m[which.max(差異), 月份], "），平均 ",
      round(mean(m$差異), 2), " 公里\n", sep = "")
  rv <- x[, .(票證數 = .N, 反向數 = sum(差 < 0), 反向占比 = round(mean(差 < 0) * 100, 1),
              正向平均里程_公里 = round(mean(差[差 > 0]) / 1000, 2),
              反向abs平均里程_公里 = round(mean(abs(差[差 < 0])) / 1000, 2),
              反向里程合計占全體_pct = round(sum(abs(差[差 < 0])) / sum(abs(x$差)) * 100, 2)),
          by = KEY][反向數 > 0][order(-反向數)]
  cat("反向票證依路線／方向（前 10）\n"); print(head(rv, 10))
  rv[, 資料集 := ds]; rev_rows[[ds]] <- rv
}
fwrite(rbindlist(rev_rows), file.path(out_dir, "D1-反向票證-四資料集依路線方向.csv"), bom = TRUE)

# ------------------------------------------------------------
# 反向票證集中的路線／方向：距離表與反向票證起迄站
# ------------------------------------------------------------
cases <- list(
  list(ds = "臺東公路", sub = "8101A",    dir = "0"),
  list(ds = "臺東公路", sub = "8101D",    dir = "0"),
  list(ds = "臺東公路", sub = "8168A",    dir = "0"),
  list(ds = "花蓮市區", sub = "309",      dir = "0"),
  list(ds = "花蓮市區", sub = "308",      dir = "1"),
  list(ds = "臺東市區", sub = "陸海空線C", dir = "0")
)
tb_rows <- list(); od_rows <- list()
for (cs in cases) {
  tag <- paste0(cs$ds, " ", cs$sub, " 方向 ", cs$dir)
  cat("\n========================================\n ", tag, " 距離表\n========================================\n", sep = "")
  tb <- cum[[cs$ds]][搭乘附屬路線名稱 == cs$sub & 搭乘公車路線方向 == cs$dir,
                     .(站序資料, 站牌代碼, 站間距離, 累積里程_公里 = round(累積里程 / 1000, 2))]
  # 站名：取票證中該站牌代碼最常見的上車站名
  nm <- sets[[cs$ds]][搭乘附屬路線名稱 == cs$sub, .N, by = .(站牌代碼 = 上車碼, 站名 = 上車站牌名稱)][order(-N)][, .SD[1], by = 站牌代碼]
  tb <- merge(tb, nm[, .(站牌代碼, 站名)], by = "站牌代碼", all.x = TRUE, sort = FALSE)[order(站序資料)]
  cat("站數 ", nrow(tb), "；全長 ", max(tb$累積里程_公里), " 公里；站序是否連續：", all(diff(tb$站序資料) == 1),
      "；站牌代碼重複數：", sum(duplicated(tb$站牌代碼)),
      "；最長單一站間距離 ", round(max(tb$站間距離) / 1000, 1), " 公里（站序 ",
      tb$站序資料[which.max(tb$站間距離)], "）\n", sep = "")
  print(tb, nrows = 200)
  tb[, `:=`(資料集 = cs$ds, 搭乘附屬路線名稱 = cs$sub, 搭乘公車路線方向 = cs$dir)]
  tb_rows[[tag]] <- tb

  y <- matched[[cs$ds]][搭乘附屬路線名稱 == cs$sub & 搭乘公車路線方向 == cs$dir]
  od <- y[, .(票證數 = .N, 反向數 = sum(差 < 0),
              上車表站序 = 上車表站序[1], 下車表站序 = 下車表站序[1],
              票證上車站序_中位 = median(上車站序), 票證下車站序_中位 = median(下車站序),
              abs里程_公里 = round(mean(abs(差)) / 1000, 2)),
          by = .(上車站牌名稱, 下車站牌名稱)][order(-反向數)]
  cat("\n", tag, " 反向票證最多的起迄站（前 12）\n", sep = ""); print(head(od[反向數 > 0], 12))
  cat("\n", tag, " 正向票證最多的起迄站（前 8，作對照）\n", sep = ""); print(head(od[反向數 == 0][order(-票證數)], 8))
  od[, `:=`(資料集 = cs$ds, 搭乘附屬路線名稱 = cs$sub, 搭乘公車路線方向 = cs$dir)]
  od_rows[[tag]] <- od
}
fwrite(rbindlist(tb_rows), file.path(out_dir, "D1-反向票證-集中路線距離表.csv"), bom = TRUE)
fwrite(rbindlist(od_rows), file.path(out_dir, "D1-反向票證-集中路線起迄站.csv"), bom = TRUE)

# ------------------------------------------------------------
# 三種處理方式下的月平均（代碼配對；與整併版實際值差 ≤0.6 公里，見 複查-D1公路站序補配與反向樣本.log）
#   甲 全部取 abs（≈ 整併版）
#   乙 反向票證丟棄（≈ 獨立腳本）
#   丙 環狀／距離表異常的附屬路線方向整組排除後取 abs
# ------------------------------------------------------------
excl <- list(
  臺東公路 = data.table(搭乘附屬路線名稱 = c("8101A", "8101B", "8101C", "8101D", "8168A"), 搭乘公車路線方向 = "0"),
  花蓮市區 = data.table(搭乘附屬路線名稱 = "309", 搭乘公車路線方向 = "0")
)
sc_rows <- list()
for (ds in names(excl)) {
  x <- matched[[ds]]
  x[, 排除 := paste(搭乘附屬路線名稱, 搭乘公車路線方向) %in% excl[[ds]][, paste(搭乘附屬路線名稱, 搭乘公車路線方向)]]
  s <- merge(merge(
    x[差 != 0, .(甲_全取abs = round(sum(abs(差)) / .N / 1000, 2), 人次_甲 = .N), by = 月份],
    x[差 > 0,  .(乙_丟反向 = round(sum(差) / .N / 1000, 2), 人次_乙 = .N), by = 月份], by = "月份"),
    x[差 != 0 & !排除, .(丙_排除異常路線 = round(sum(abs(差)) / .N / 1000, 2), 人次_丙 = .N), by = 月份], by = "月份")
  s[, `:=`(甲減丙 = round(甲_全取abs - 丙_排除異常路線, 2), 乙減丙 = round(乙_丟反向 - 丙_排除異常路線, 2))]
  cat("\n===== ", ds, "：三種處理方式的月平均里程（公里）；排除組 = ",
      excl[[ds]][, paste(paste(搭乘附屬路線名稱, "方向", 搭乘公車路線方向), collapse = "、")],
      "，占配對票證 ", round(x[, mean(排除)] * 100, 1), "%\n", sep = "")
  print(s)
  cat(sprintf("甲−丙：平均 %.2f、最大 %.2f（%s）；乙−丙：平均 %.2f、最大 %.2f（%s）\n",
              mean(s$甲減丙), max(s$甲減丙), s[which.max(甲減丙), 月份],
              mean(s$乙減丙), s[which.max(abs(乙減丙)), 乙減丙], s[which.max(abs(乙減丙)), 月份]))
  s[, 資料集 := ds]; sc_rows[[ds]] <- s
}
fwrite(rbindlist(sc_rows), file.path(out_dir, "D1-反向票證-三種處理方式月平均.csv"), bom = TRUE)

cat("\n耗時：", format(round(difftime(Sys.time(), t0, units = "mins"), 1)), "\n")
