# ============================================================
# D1 複查（續）：公路客運兩個最大差異來源的樣本檢視
#
# 承 複查-D1里程獨立腳本差異分解.R 的結果：
#   花蓮公路：整併版比獨立腳本高 1.17 公里，幾乎全來自「站序精確補配」多配到的票證（E − D2）
#   臺東公路：整併版比獨立腳本高 1.56 公里，幾乎全來自 abs(下車－上車) 把反向票證納入（C − B）
#
# 本腳本回答兩個問題：
#   1. 整併版目前的公路配對實際上是「只靠站序」（C1：站牌代碼前綴未去除，代碼配對 0%）。
#      只靠站序多配到的票證是哪些路線、為何站牌代碼配不到、它們的里程長多少、
#      站序配法與代碼配法在雙方都配得到的票證上是否一致。
#   2. 下車累積里程 < 上車累積里程（反向）的票證是哪些路線／方向、占多少、
#      取絕對值後的里程和同路線正向票證相比是否合理。
#
# 輸出：output/D1-公路-配對方法交叉.csv、output/D1-公路-只靠站序票證依路線.csv、
#       output/D1-公路-反向票證依路線方向.csv、同名 .log
# 耗時：約 2 分鐘
# ============================================================

suppressPackageStartupMessages({ library(data.table); library(openxlsx) })

this_file <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
if (length(this_file) == 0) this_file <- "複查-D1公路站序補配與反向樣本.R"
code_dir  <- normalizePath(dirname(this_file))
REPO_ROOT <- normalizePath(file.path(code_dir, "..", ".."))
source(file.path(REPO_ROOT, "review", "A-資料缺漏與異常", "00-setup.R"))
out_dir <- file.path(code_dir, "output")
t0 <- Sys.time()

COLS <- c("業者編號", "搭乘路線名稱", "搭乘附屬路線名稱", "搭乘公車路線方向",
          "上車站牌代碼", "下車站牌代碼", "上車計費站序資料", "下車站牌站序", "原始票證筆數")
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
            臺東公路 = read_dist("臺東縣公路客運站間距離資料.csv"))

公路 <- in_window(read_tickets("公路客運2024_to_202606.csv", COLS))
公路[, 搭乘路線名稱     := trimws(as.character(搭乘路線名稱))]
公路[, 搭乘附屬路線名稱 := trimws(as.character(搭乘附屬路線名稱))]
公路[, 搭乘公車路線方向 := as.character(搭乘公車路線方向)]
公路[, 上車碼 := suppressWarnings(as.character(as.numeric(gsub("[^0-9]", "", 上車站牌代碼))))]
公路[, 下車碼 := suppressWarnings(as.character(as.numeric(gsub("[^0-9]", "", 下車站牌代碼))))]
公路[, 上車站序 := suppressWarnings(as.numeric(上車計費站序資料))]
公路[, 下車站序 := suppressWarnings(as.numeric(下車站牌站序))]
公路[, 原始票證筆數 := as.numeric(原始票證筆數)]
公路[, 月份 := 民國月份]
公路 <- 公路[!is.na(原始票證筆數) & 原始票證筆數 > 0]
公路[, tid := .I]

# 代碼配對（整併版第一層：同站牌多筆候選依站序差最小；C1 修正後的行為）
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
    r <- sel[, .(v = if (uniqueN(候選) == 1) 候選[1] else NA_real_), by = tid]
    setnames(r, "v", out_col); r
  }
  x <- merge(tk[, .(tid)], side("上車碼", "上車站序", "上車里程_碼"), by = "tid", all.x = TRUE)
  x <- merge(x, side("下車碼", "下車站序", "下車里程_碼"), by = "tid", all.x = TRUE)
  x[, 里程_碼 := abs(下車里程_碼 - 上車里程_碼)]
  x[]
}

# 站序配對（整併版第二層：同路線／附屬／方向／站序，累積里程唯一才用）。
# 因 C1（前綴未去除），整併版的公路票證實際上全部走這一層。
match_seq <- function(tk, cm) {
  sq <- cm[!is.na(站序資料), .(n里程 = uniqueN(累積里程), 里程 = if (uniqueN(累積里程) == 1) 累積里程[1] else NA_real_),
           by = c(KEY, "站序資料")]
  side <- function(seq_col, out_col) {
    t1 <- tk[!is.na(get(seq_col)) & get(seq_col) != -99, c("tid", KEY, seq_col), with = FALSE]
    setnames(t1, seq_col, "站序資料")
    r <- merge(t1, sq[, c(KEY, "站序資料", "里程"), with = FALSE], by = c(KEY, "站序資料"), sort = FALSE)
    r <- r[!is.na(里程), .(tid, 里程)]
    setnames(r, "里程", out_col); r
  }
  x <- merge(tk[, .(tid)], side("上車站序", "上車里程_序"), by = "tid", all.x = TRUE)
  x <- merge(x, side("下車站序", "下車里程_序"), by = "tid", all.x = TRUE)
  x[, 里程_序 := abs(下車里程_序 - 上車里程_序)]
  x[]
}

xlsx_new <- file.path(REPO_ROOT, "revise", "output", "tbl", "月平均搭乘里程.xlsx")
cross_rows <- list(); seq_only_rows <- list(); rev_rows <- list()

for (ds in c("花蓮公路", "臺東公路")) {
  cat("\n========================================\n", ds, "\n========================================\n")
  routes <- if (ds == "花蓮公路") routes_hualien_THB else routes_taitung_THB
  tk <- 公路[搭乘路線名稱 %in% routes]
  cm <- cum[[ds]]

  x <- merge(tk, match_code(tk, cm), by = "tid")
  x <- merge(x,  match_seq(tk, cm),  by = "tid")

  # 0. 確認「只靠站序」= 整併版 xlsx
  E  <- as.data.table(read.xlsx(xlsx_new, sheet = ds))
  m  <- x[!is.na(里程_序), .(人次_序 = .N, 里程_序 = round(sum(里程_序) / .N / 1000, 2)), by = 月份]
  chk <- merge(E[, .(月份, 人次_E = 搭乘人次, 里程_E = 平均搭乘里程_公里)], m, by = "月份")
  cat(sprintf("站序配法重現整併版 xlsx：人次最大差 %d、里程最大差 %.2f 公里\n",
              max(abs(chk$人次_E - chk$人次_序)), max(abs(chk$里程_E - chk$里程_序))))

  # 1. 兩種配法交叉
  x[, 配法 := fcase(!is.na(里程_碼) & !is.na(里程_序), "代碼與站序皆配到",
                   !is.na(里程_碼), "只有代碼",
                   !is.na(里程_序), "只有站序",
                   default = "皆未配到")]
  cr <- x[, .(票證數 = .N, 占比 = round(.N / nrow(x) * 100, 2),
              代碼配法平均里程 = round(mean(里程_碼, na.rm = TRUE) / 1000, 2),
              站序配法平均里程 = round(mean(里程_序, na.rm = TRUE) / 1000, 2)), by = 配法][order(-票證數)]
  both <- x[配法 == "代碼與站序皆配到"]
  agree <- both[, .(一致率 = round(mean(abs(里程_碼 - 里程_序) < 1) * 100, 2),
                    平均絕對差_公尺 = round(mean(abs(里程_碼 - 里程_序)), 1))]
  cat("\n兩種配法交叉：\n"); print(cr)
  cat(sprintf("皆配到的票證：兩法里程一致率 %.2f%%，平均絕對差 %.1f 公尺\n", agree$一致率, agree$平均絕對差_公尺))
  cr[, 資料集 := ds]; cross_rows[[ds]] <- cr

  # 2. 只靠站序配到的票證：路線、代碼配不到的原因、里程
  so <- x[配法 == "只有站序"]
  so[, 代碼配不到原因 := fcase(
    is.na(上車碼) & is.na(下車碼), "上下車代碼皆空",
    is.na(上車碼) | is.na(下車碼), "單側代碼空",
    is.na(上車里程_碼) & is.na(下車里程_碼), "上下車代碼皆不在該路線距離表",
    is.na(上車里程_碼), "上車代碼不在該路線距離表",
    is.na(下車里程_碼), "下車代碼不在該路線距離表",
    default = "其他")]
  # 代碼是否存在於距離表任何地方（其他路線／方向）
  all_codes <- unique(cm$站牌代碼)
  so[, 代碼在距離表他處 := fifelse(上車碼 %in% all_codes & 下車碼 %in% all_codes, "上下車皆在他處",
                                  fifelse(上車碼 %in% all_codes | 下車碼 %in% all_codes, "單側在他處", "皆不在"))]
  cat("\n只靠站序配到的票證：代碼配不到的原因\n")
  print(so[, .(票證數 = .N, 平均里程_公里 = round(mean(里程_序) / 1000, 2)), by = .(代碼配不到原因, 代碼在距離表他處)][order(-票證數)])
  by_route <- so[, .(票證數 = .N, 平均里程_公里 = round(mean(里程_序) / 1000, 2),
                     站序中位數_上 = median(上車站序), 站序中位數_下 = median(下車站序)),
                 by = .(搭乘路線名稱, 搭乘附屬路線名稱, 搭乘公車路線方向, 業者編號)][order(-票證數)]
  # 同路線／方向「皆配到」票證的平均里程作對照
  ref <- both[, .(皆配到票證數 = .N, 皆配到平均里程_公里 = round(mean(里程_序) / 1000, 2)),
              by = .(搭乘路線名稱, 搭乘附屬路線名稱, 搭乘公車路線方向)]
  by_route <- merge(by_route, ref, by = c("搭乘路線名稱", "搭乘附屬路線名稱", "搭乘公車路線方向"), all.x = TRUE)[order(-票證數)]
  cat("\n只靠站序配到的票證：依路線／方向／業者（前 15）\n"); print(head(by_route, 15))
  by_route[, 資料集 := ds]; seq_only_rows[[ds]] <- by_route
  cat("\n只靠站序配到的票證：里程分布（公里）\n")
  print(round(quantile(so$里程_序 / 1000, c(0, .1, .25, .5, .75, .9, 1)), 1))
  cat(sprintf("只靠站序票證逐月占整併版人次：最低 %.1f%%、最高 %.1f%%\n",
              100 * min(so[, .N, by = 月份][chk, on = "月份"][, N / 人次_E]),
              100 * max(so[, .N, by = 月份][chk, on = "月份"][, N / 人次_E])))

  # 3. 反向票證（代碼配法：下車累積里程 < 上車累積里程）
  y <- x[!is.na(上車里程_碼) & !is.na(下車里程_碼)]
  y[, 方向別 := fcase(下車里程_碼 > 上車里程_碼, "正向", 下車里程_碼 < 上車里程_碼, "反向", default = "零")]
  cat("\n代碼配到的票證：正向／反向／零\n")
  print(y[, .(票證數 = .N, 占比 = round(.N / nrow(y) * 100, 2), 平均里程_abs_公里 = round(mean(里程_碼) / 1000, 2)), by = 方向別])
  rv <- y[, .(票證數 = .N, 反向數 = sum(方向別 == "反向"), 反向占比 = round(mean(方向別 == "反向") * 100, 1),
              正向平均里程_公里 = round(mean(里程_碼[方向別 == "正向"]) / 1000, 2),
              反向平均里程_公里 = round(mean(里程_碼[方向別 == "反向"]) / 1000, 2)),
          by = .(搭乘路線名稱, 搭乘附屬路線名稱, 搭乘公車路線方向)][反向數 > 0][order(-反向數)]
  cat("\n反向票證依路線／方向（前 15）\n"); print(head(rv, 15))
  rv[, 資料集 := ds]; rev_rows[[ds]] <- rv
  cat(sprintf("反向票證逐月占代碼配到票證：最低 %.1f%%、最高 %.1f%%（最高月份 %s）\n",
              100 * min(y[, mean(方向別 == "反向"), by = 月份]$V1),
              100 * max(y[, mean(方向別 == "反向"), by = 月份]$V1),
              y[, mean(方向別 == "反向"), by = 月份][which.max(V1), 月份]))
}

fwrite(rbindlist(cross_rows),    file.path(out_dir, "D1-公路-配對方法交叉.csv"), bom = TRUE)
fwrite(rbindlist(seq_only_rows), file.path(out_dir, "D1-公路-只靠站序票證依路線.csv"), bom = TRUE)
fwrite(rbindlist(rev_rows),      file.path(out_dir, "D1-公路-反向票證依路線方向.csv"), bom = TRUE)
cat("\n已寫出 output/D1-公路-{配對方法交叉,只靠站序票證依路線,反向票證依路線方向}.csv\n")
cat("耗時：", format(round(difftime(Sys.time(), t0, units = "mins"), 1)), "\n")
