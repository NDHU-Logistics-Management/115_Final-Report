# ============================================================
# D1 複查：月平均搭乘里程 —— 獨立腳本 vs 整併版 Rmd 差異分解
#
# 問題：`月平均搭乘里程/圖片/*.png`（組員獨立腳本產出，2025-08-25）與
#       `revise/output/tbl/月平均搭乘里程.xlsx`（整併版 115期末.Rmd 重出）逐月數值不同，
#       公路差約 1–3 公里、市區差約 0.5–1.5 公里，整併版一律偏高。
#
# 做法：在同一份 data/ 上，從獨立腳本的算法出發，一次只改一項規則，逐步走到整併版的規則，
#       看每一步讓 25 個月的平均里程移動多少。
#
#   A   獨立腳本原法：路線範圍依附屬路線清單（公路）／距離表有的路線（市區）；
#       站牌代碼去字母；距離表同鍵多筆取站序最小者（slice(1)）；市區配對鍵不含搭乘路線名稱；
#       里程 = 下車－上車，只留 > 0；人次 = 列數
#   B   A ＋ 票證範圍改用整併版：公路 搭乘路線名稱 %in% routes_*_THB；市區 _clean 清理規則；原始票證筆數 > 0
#   C   B ＋ 里程改 abs(下車－上車)，含 0（整併版第 7 條規則）
#   D   C ＋ 配對鍵加入搭乘路線名稱、距離表只用本縣（市區才有差；公路 D = C）
#   D2  D ＋ 同站牌多筆候選改依「票證站序差最小」選點（整併版第一層規則，站牌代碼前綴 C1 修正後的行為）
#   E   整併版實際結果（revise/output/tbl/月平均搭乘里程.xlsx）
#       E − D2 = 站序精確補配（第二層 fallback）＋ C1 前綴未修正致公路／臺東市區全走 fallback
#
# 輸出：output/D1-里程差異分解-逐月.csv      四個資料集 × 25 個月，各步驟平均里程與人次，另附圖片標示值
#       output/D1-里程差異分解-摘要.csv      每一步的平均移動量、最大移動量與月份
#       output/D1-里程差異分解-路線範圍.csv  兩者納入的路線／附屬路線差異與人次
# 耗時：約 3 分鐘
# ============================================================

suppressPackageStartupMessages({ library(data.table); library(openxlsx) })

this_file <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
if (length(this_file) == 0) this_file <- "複查-D1里程獨立腳本差異分解.R"
code_dir  <- normalizePath(dirname(this_file))
REPO_ROOT <- normalizePath(file.path(code_dir, "..", ".."))

source(file.path(REPO_ROOT, "review", "A-資料缺漏與異常", "00-setup.R"))
out_dir <- file.path(code_dir, "output")
dir.create(out_dir, showWarnings = FALSE)
t0 <- Sys.time()

# ------------------------------------------------------------
# 獨立腳本的路線清單（逐字抄自 月平均搭乘里程/115期末報告-公路客運平均搭乘里程.R）
# 以「搭乘附屬路線名稱」inner_join，未列者不納入
# ------------------------------------------------------------
tm_taitung_sub <- c(
  "1145", "8101", "8101A", "8101B", "8101C", "8101D", "8102", "8103", "8105", "8107", "8109",
  "8119", "8120", "8122", "8125",
  "8117", "8161", "8163", "8163A", "8163B", "8165", "8165A", "8166", "8166A",
  "8167", "8167A", "8167B", "8168", "8168A", "8168B", "8170", "8170A",
  "8171", "8171A", "8171B", "8172", "8173", "8178",
  "8181",
  "8132", "8135", "8136", "8137", "8138", "8150", "8151", "8151A", "8152A", "8156", "8157", "8158",
  "8113", "8115", "8128", "8129", "8129A", "8130", "8130A", "8131", "8131A", "8153"
)
tm_hualien_sub <- c(
  "1129", "11290", "1132", "1132A", "1136", "1140", "1145", "8119",
  "1121", "11210", "1122", "11220", "1128", "1130", "1135", "1135A", "1137",
  "1139", "1139B", "1139C", "1142", "11420", "1143", "8161", "8173",
  "1125", "1133", "1141", "11410", "1141A", "8181"
)

# 圖片上的數值標籤（1 位小數），逐字抄自 月平均搭乘里程/圖片/*.png，用來確認步驟 A 有重現獨立腳本
png_label <- list(
  花蓮公路 = c(22.4, 23.6, 23.4, 21.7, 21.1, 20.7, 21.7, 22.6, 22.5, 21.5, 21.6, 21.4, 22.3,
               23.3, 23.3, 21.0, 20.8, 20.6, 21.0, 20.6, 23.0, 20.2, 21.1, 20.5, 21.2),
  臺東公路 = c(23.2, 25.4, 26.3, 23.4, 23.2, 22.9, 22.5, 23.5, 23.0, 22.2, 22.1, 22.3, 23.2,
               24.8, 25.5, 22.1, 22.1, 29.1, 21.4, 21.3, 23.3, 21.2, 21.5, 21.8, 22.1),
  花蓮市區 = c(16.3, 16.3, 16.6, 16.1, 16.3, 14.2, 15.1, 12.6, 12.8, 12.7, 12.8, 12.1, 12.7,
               12.0, 12.5, 12.8, 12.6, 11.4, 11.5, 10.8, 11.1, 11.7, 11.5, 9.2, 8.1),
  臺東市區 = c(8.1, 8.1, 8.2, 7.9, 7.8, 7.4, 7.3, 7.2, 7.4, 7.6, 8.0, 8.0, 8.1,
               8.0, 8.2, 8.0, 8.0, 7.6, 7.2, 7.3, 7.6, 7.5, 7.9, 7.9, 7.9)
)

# ------------------------------------------------------------
# 讀取
# ------------------------------------------------------------
COLS <- c("搭乘路線名稱", "搭乘附屬路線名稱", "搭乘公車路線方向",
          "上車站牌代碼", "下車站牌代碼", "上車計費站序資料", "下車站牌站序", "原始票證筆數")

read_dist <- function(f) {
  d <- fread(file.path(data_path, "公車站間距離資料", f), colClasses = list(character = 1:5))
  strip_bom(d)
  d[, 站序資料 := as.numeric(站序資料)]
  d[, 站間距離 := as.numeric(站間距離)]
  d[]
}
dist <- list(
  花蓮公路 = read_dist("花蓮縣公路客運站間距離資料.csv"),
  臺東公路 = read_dist("臺東縣公路客運站間距離資料.csv"),
  花蓮市區 = read_dist("花蓮縣市區客運站間距離資料.csv"),
  臺東市區 = read_dist("臺東縣市區客運站間距離資料.csv")
)

prep_tickets <- function(d) {
  d <- in_window(d)
  d[, 搭乘路線名稱     := trimws(as.character(搭乘路線名稱))]
  d[, 搭乘附屬路線名稱 := trimws(as.character(搭乘附屬路線名稱))]
  d[, 搭乘公車路線方向 := as.character(搭乘公車路線方向)]
  # 獨立腳本的站牌代碼寫法：去掉所有非數字
  d[, 上車碼 := suppressWarnings(as.character(as.numeric(gsub("[^0-9]", "", 上車站牌代碼))))]
  d[, 下車碼 := suppressWarnings(as.character(as.numeric(gsub("[^0-9]", "", 下車站牌代碼))))]
  d[, 上車站序 := suppressWarnings(as.numeric(上車計費站序資料))]
  d[, 下車站序 := suppressWarnings(as.numeric(下車站牌站序))]
  d[, 原始票證筆數 := as.numeric(原始票證筆數)]
  d[, 月份 := 民國月份]
  d[, tid := .I]
  d[]
}

公路   <- prep_tickets(read_tickets("公路客運2024_to_202606.csv", COLS))
花蓮市 <- prep_tickets(read_tickets("花蓮縣公車.csv", COLS))
臺東市 <- prep_tickets(read_tickets("臺東縣公車.csv", COLS))
cat("讀取完成：", format(round(difftime(Sys.time(), t0, units = "secs"))), "\n\n")

# ------------------------------------------------------------
# 票證範圍
# ------------------------------------------------------------
scope_tm <- list(
  花蓮公路 = function() 公路[搭乘附屬路線名稱 %in% tm_hualien_sub],
  臺東公路 = function() 公路[搭乘附屬路線名稱 %in% tm_taitung_sub],
  # 市區獨立腳本：搭乘路線名稱 %in% 距離表的路線（不做 _clean 清理）
  花蓮市區 = function() 花蓮市[搭乘路線名稱 %in% unique(dist$花蓮市區$搭乘路線名稱)],
  臺東市區 = function() 臺東市[搭乘路線名稱 %in% unique(dist$臺東市區$搭乘路線名稱)]
)
scope_rmd <- list(
  花蓮公路 = function() 公路[搭乘路線名稱 %in% routes_hualien_THB & !is.na(原始票證筆數) & 原始票證筆數 > 0],
  臺東公路 = function() 公路[搭乘路線名稱 %in% routes_taitung_THB & !is.na(原始票證筆數) & 原始票證筆數 > 0],
  花蓮市區 = function() 花蓮市[!搭乘路線名稱 %in% hualien_abnormal_routes & !is.na(原始票證筆數) & 原始票證筆數 > 0],
  臺東市區 = function() 臺東市[(!(get(DATE_COL) %in% taitung_abnormal_dates) | 搭乘路線名稱 %in% taitung_normal_routes) &
                                 !is.na(原始票證筆數) & 原始票證筆數 > 0]
)

# ------------------------------------------------------------
# 距離表累積里程（兩邊做法相同：同路線／附屬／方向依站序累加，NA 站間距離視為 0）
# ------------------------------------------------------------
build_cum <- function(d) {
  d <- copy(d)
  d[is.na(站間距離), 站間距離 := 0]
  setorder(d, 搭乘路線名稱, 搭乘附屬路線名稱, 搭乘公車路線方向, 站序資料)
  d[, 累積里程 := cumsum(站間距離), by = .(搭乘路線名稱, 搭乘附屬路線名稱, 搭乘公車路線方向)]
  d[]
}
cum <- lapply(dist, build_cum)
cum_city_both <- rbind(cum$花蓮市區, cum$臺東市區)   # 市區獨立腳本把兩縣距離表合併後才去重

KEY_FULL <- c("搭乘路線名稱", "搭乘附屬路線名稱", "搭乘公車路線方向")
KEY_SUB  <- c("搭乘附屬路線名稱", "搭乘公車路線方向")

# 獨立腳本配對：同鍵多筆取站序最小者，兩端各 left join 一次
match_first <- function(tk, cm, key_cols) {
  idx <- unique(cm, by = c(key_cols, "站牌代碼"))
  on  <- idx[, c(key_cols, "站牌代碼", "累積里程"), with = FALSE]
  setnames(on, c("站牌代碼", "累積里程"), c("上車碼", "上車里程"))
  off <- idx[, c(key_cols, "站牌代碼", "累積里程"), with = FALSE]
  setnames(off, c("站牌代碼", "累積里程"), c("下車碼", "下車里程"))
  x <- tk[!is.na(上車碼) & !is.na(下車碼)]
  x <- merge(x, on,  by = c(key_cols, "上車碼"), all.x = TRUE, sort = FALSE)
  x <- merge(x, off, by = c(key_cols, "下車碼"), all.x = TRUE, sort = FALSE)
  x[, 差 := 下車里程 - 上車里程]
  x[]
}

# 整併版第一層規則：同站牌多筆候選時取票證站序差最小者；站序 -99／NA 時須唯一才用；歧義不猜
match_seq <- function(tk, cm, key_cols) {
  x <- tk[!is.na(上車碼) & !is.na(下車碼)]
  side <- function(code_col, seq_col, out_col) {
    c1 <- cm[, c(key_cols, "站牌代碼", "站序資料", "累積里程"), with = FALSE]
    setnames(c1, c("站牌代碼", "站序資料", "累積里程"), c(code_col, "距離表站序", "候選"))
    t1 <- x[, c("tid", key_cols, code_col, seq_col), with = FALSE]
    setnames(t1, seq_col, "票證站序")
    cand <- merge(t1, c1, by = c(key_cols, code_col), allow.cartesian = TRUE, sort = FALSE)
    cand[, has_seq := !is.na(票證站序) & 票證站序 != -99]
    cand[, 站序差 := fifelse(has_seq & !is.na(距離表站序), abs(票證站序 - 距離表站序), NA_real_)]
    cand[, min_diff := suppressWarnings(min(站序差, na.rm = TRUE)), by = tid]
    sel <- cand[!has_seq | (!is.na(站序差) & 站序差 == min_diff)]
    r <- sel[, .(v = if (uniqueN(候選) == 1) 候選[1] else NA_real_), by = tid]
    setnames(r, "v", out_col)
    r
  }
  x <- merge(x, side("上車碼", "上車站序", "上車里程"), by = "tid", all.x = TRUE, sort = FALSE)
  x <- merge(x, side("下車碼", "下車站序", "下車里程"), by = "tid", all.x = TRUE, sort = FALSE)
  x[, 差 := 下車里程 - 上車里程]
  x[]
}

monthly <- function(x, rule) {
  y <- if (rule == "pos") x[!is.na(差) & 差 > 0] else copy(x[!is.na(差)])[, 差 := abs(差)]
  y[, .(人次 = .N, 平均 = round(sum(差) / .N / 1000, 2)), by = 月份][order(月份)]
}

# ------------------------------------------------------------
# 逐資料集分解
# ------------------------------------------------------------
xlsx_new <- file.path(REPO_ROOT, "revise", "output", "tbl", "月平均搭乘里程.xlsx")
month_all <- data.table(月份 = sort(unique(公路$月份)))

rows_month <- list(); rows_sum <- list(); rows_scope <- list()

for (ds in c("花蓮公路", "臺東公路", "花蓮市區", "臺東市區")) {
  cat("\n========================================\n", ds, "\n========================================\n")
  is_city <- grepl("市區", ds)
  tk_tm  <- scope_tm[[ds]]()
  tk_rmd <- scope_rmd[[ds]]()
  cat("票證列數（分析期間內）：獨立腳本範圍", nrow(tk_tm), "／整併版範圍", nrow(tk_rmd), "\n")

  # 路線範圍差異
  base <- if (is_city) (if (ds == "花蓮市區") 花蓮市 else 臺東市) else 公路
  sc <- base[, .(人次 = .N), by = .(搭乘路線名稱, 搭乘附屬路線名稱)]
  sc[, 獨立腳本 := if (is_city) 搭乘路線名稱 %in% unique(dist[[ds]]$搭乘路線名稱)
                   else 搭乘附屬路線名稱 %in% (if (ds == "花蓮公路") tm_hualien_sub else tm_taitung_sub)]
  sc[, 整併版 := if (ds == "花蓮公路") 搭乘路線名稱 %in% routes_hualien_THB
                 else if (ds == "臺東公路") 搭乘路線名稱 %in% routes_taitung_THB
                 else if (ds == "花蓮市區") !搭乘路線名稱 %in% hualien_abnormal_routes
                 else TRUE]
  sc <- sc[獨立腳本 != 整併版][order(-人次)]
  if (nrow(sc) > 0) { sc[, 資料集 := ds]; rows_scope[[ds]] <- sc }
  cat("只在一邊納入的路線：\n"); print(sc[, .(搭乘路線名稱, 搭乘附屬路線名稱, 人次, 獨立腳本, 整併版)])

  # A：獨立腳本原法
  cm_tm  <- if (is_city) cum_city_both else cum[[ds]]
  key_tm <- if (is_city) KEY_SUB else KEY_FULL
  mA <- match_first(tk_tm, cm_tm, key_tm)
  A  <- monthly(mA, "pos")
  cat(sprintf("A  配對成功 %d 列；其中 差<0 %d 列、差=0 %d 列（獨立腳本丟棄）\n",
              mA[!is.na(差), .N], mA[!is.na(差) & 差 < 0, .N], mA[!is.na(差) & 差 == 0, .N]))

  # B：整併版票證範圍
  mB <- match_first(tk_rmd, cm_tm, key_tm)
  B  <- monthly(mB, "pos")
  cat(sprintf("B  配對成功 %d 列；差<0 %d 列、差=0 %d 列\n",
              mB[!is.na(差), .N], mB[!is.na(差) & 差 < 0, .N], mB[!is.na(差) & 差 == 0, .N]))

  # C：abs 含 0
  C <- monthly(mB, "abs")

  # D：完整鍵、本縣距離表
  mD <- match_first(tk_rmd, cum[[ds]], KEY_FULL)
  D  <- monthly(mD, "abs")
  cat(sprintf("D  配對成功 %d 列\n", mD[!is.na(差), .N]))

  # D2：站序差最小選點
  mD2 <- match_seq(tk_rmd, cum[[ds]], KEY_FULL)
  D2  <- monthly(mD2, "abs")
  cat(sprintf("D2 配對成功 %d 列\n", mD2[!is.na(差), .N]))

  # E：整併版 xlsx
  E <- as.data.table(read.xlsx(xlsx_new, sheet = ds))[, .(月份, 人次 = 搭乘人次, 平均 = 平均搭乘里程_公里)]

  tab <- copy(month_all)
  add <- function(tab, m, nm) {
    m <- copy(m); setnames(m, c("人次", "平均"), paste0(c("人次_", "里程_"), nm))
    merge(tab, m, by = "月份", all.x = TRUE)
  }
  tab <- add(tab, A, "A"); tab <- add(tab, B, "B"); tab <- add(tab, C, "C")
  tab <- add(tab, D, "D"); tab <- add(tab, D2, "D2"); tab <- add(tab, E, "E")
  tab[, 圖片標示 := png_label[[ds]]]
  tab[, 資料集 := ds]
  setcolorder(tab, c("資料集", "月份", "圖片標示", paste0("里程_", c("A", "B", "C", "D", "D2", "E")),
                     paste0("人次_", c("A", "B", "C", "D", "D2", "E"))))
  rows_month[[ds]] <- tab

  cat("\n逐月平均里程（公里）：\n")
  print(tab[, .(月份, 圖片標示, A = 里程_A, B = 里程_B, C = 里程_C, D = 里程_D, D2 = 里程_D2, E = 里程_E)])

  steps <- list(
    c("圖片標示", "里程_A", "A 重現獨立腳本（圖片為 1 位小數）"),
    c("里程_A",  "里程_B",  "B 票證範圍改整併版"),
    c("里程_B",  "里程_C",  "C 里程改 abs 含 0"),
    c("里程_C",  "里程_D",  "D 配對鍵加路線名稱、本縣距離表"),
    c("里程_D",  "里程_D2", "D2 同站牌依站序差最小選點"),
    c("里程_D2", "里程_E",  "E 整併版實際（站序精確補配、C1 前綴）"),
    c("里程_A",  "里程_E",  "合計 A→E")
  )
  for (s in steps) {
    d <- tab[[s[2]]] - tab[[s[1]]]
    if (s[1] == "圖片標示") d <- round(tab[[s[2]]], 1) - tab[[s[1]]]
    i <- which.max(abs(d))
    rows_sum[[length(rows_sum) + 1]] <- data.table(
      資料集 = ds, 步驟 = s[3],
      平均移動_公里 = round(mean(d, na.rm = TRUE), 2),
      最大移動_公里 = round(d[i], 2), 最大移動月份 = tab$月份[i],
      人次_前 = if (s[1] == "圖片標示") NA_integer_ else sum(tab[[sub("里程_", "人次_", s[1])]], na.rm = TRUE),
      人次_後 = sum(tab[[sub("里程_", "人次_", s[2])]], na.rm = TRUE)
    )
  }
}

month_tab <- rbindlist(rows_month)
sum_tab   <- rbindlist(rows_sum)
scope_tab <- rbindlist(rows_scope, fill = TRUE)

fwrite(month_tab, file.path(out_dir, "D1-里程差異分解-逐月.csv"), bom = TRUE)
fwrite(sum_tab,   file.path(out_dir, "D1-里程差異分解-摘要.csv"), bom = TRUE)
fwrite(scope_tab[, .(資料集, 搭乘路線名稱, 搭乘附屬路線名稱, 人次, 獨立腳本, 整併版)],
       file.path(out_dir, "D1-里程差異分解-路線範圍.csv"), bom = TRUE)

cat("\n========================================\n摘要：每一步讓 25 個月平均里程移動多少\n========================================\n")
print(sum_tab)
cat("\n已寫出 output/D1-里程差異分解-{逐月,摘要,路線範圍}.csv\n")
cat("耗時：", format(round(difftime(Sys.time(), t0, units = "mins"), 1)), "\n")
