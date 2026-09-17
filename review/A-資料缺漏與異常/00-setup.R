# ============================================================
# A 類稽核：共用設定
#
# 用法：
#   其餘 A*.R 腳本開頭皆會 source 本檔。
#   若你的 repo 不在預設路徑，請在 source 之前先指定：
#       REPO_ROOT <- "你的路徑/115_Final-Report"
#
# 設計原則：
#   1. 只讀取稽核需要的欄位（fread select），避免載入完整 1.3GB
#   2. 所有輸出同時 print 到 console 並寫入 output/
#   3. 清理規則與 實作測試/115期末.Rmd 保持一致，以確保稽核對象
#      就是報告實際使用的資料
# ============================================================

library(data.table)


# ------------------------------------------------------------
# 路徑
# ------------------------------------------------------------

if (!exists("REPO_ROOT")) {
  REPO_ROOT <- "d:/Github/115_Final-Report"
}

data_path <- file.path(REPO_ROOT, "data")
out_dir   <- file.path(REPO_ROOT, "review", "A-資料缺漏與異常", "output")

if (!dir.exists(data_path)) {
  stop(
    "找不到資料夾：", data_path,
    "\n請在 source 本檔前指定 REPO_ROOT <- \"你的路徑/115_Final-Report\""
  )
}

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)


# ------------------------------------------------------------
# 分析期間（113/06 ～ 115/06）
# ------------------------------------------------------------

start_date <- as.Date("2024-06-01")
end_date   <- as.Date("2026-06-30")


# ------------------------------------------------------------
# 路線定義（與 115期末.Rmd 相同）
# ------------------------------------------------------------

routes_hualien_THB <- c(
  "1121", "1122", "1123", "1125", "1126", "1128", "1129", "1130",
  "1132", "1133", "1135", "1136", "1137", "1139", "1140", "1141",
  "1142", "1143", "1145", "8119", "8161", "8173", "8181"
)

routes_taitung_THB <- c(
  "1145", "309", "8101", "8102", "8103", "8105", "8107", "8109",
  "8110", "8111", "8113", "8115", "8117", "8119", "8120", "8122",
  "8125", "8128", "8129", "8130", "8131", "8132", "8135", "8136",
  "8137", "8138", "8150", "8151", "8152", "8153", "8156", "8157",
  "8158", "8161", "8163", "8165", "8166", "8167", "8168", "8170",
  "8171", "8172", "8173", "8178", "8180", "8181"
)

# 市區客運異常資料清理規則（與 115期末.Rmd 相同）
hualien_abnormal_routes <- c(
  "50", "51", "52", "53", "71", "72", "73", "81", "83", "綠線"
)

taitung_abnormal_dates <- as.Date(c("2026-05-24", "2026-05-25"))
taitung_normal_routes  <- c("101", "201", "202", "203")


# ------------------------------------------------------------
# 工具函數
# ------------------------------------------------------------

DATE_COL <- "資料代表日期(yyyy-MM-dd)"

# CSV 首欄名稱帶 UTF-8 BOM，統一去除
strip_bom <- function(d) {
  setnames(d, names(d), sub("^\uFEFF", "", names(d)))
  invisible(d)
}

# 代碼類欄位一律以字串讀入，避免 0816、0412 這類前導零被吃掉
ID_COLS <- c(
  "業者編號", "卡號", "搭乘路線名稱", "搭乘附屬路線名稱",
  "搭乘路線代碼", "搭乘附屬路線代碼", "票種類型", "票種次類型",
  "持卡身分", "上車站牌代碼", "下車站牌代碼", "轉乘代碼紀錄"
)

# 讀取票證資料：只取指定欄位，並轉好日期與分析期間
read_tickets <- function(file, cols) {

  cols <- unique(c(cols, DATE_COL))

  cat("讀取：", basename(file), " 欄位：", paste(cols, collapse = ", "), "\n")

  d <- fread(
    file.path(data_path, file),
    select = cols,
    colClasses = list(character = intersect(cols, ID_COLS)),
    showProgress = FALSE
  )

  strip_bom(d)

  d[, (DATE_COL) := as.Date(get(DATE_COL))]
  d <- d[!is.na(get(DATE_COL))]

  d[, 西元年月 := format(get(DATE_COL), "%Y-%m")]
  d[, 民國月份 := sprintf(
    "%03d/%02d",
    as.integer(format(get(DATE_COL), "%Y")) - 1911,
    as.integer(format(get(DATE_COL), "%m"))
  )]

  d[]
}

# 限縮到分析期間
in_window <- function(d) {
  d[get(DATE_COL) >= start_date & get(DATE_COL) <= end_date]
}

# 站牌代碼正規化（C1 的修正版）
#
# 票證側代碼帶縣市前綴（HUA298878／TTT295312／THB96102），
# 距離表側為純數字（298878／295300／96102）。
# 115期末.Rmd:915 只去除 ^HUA，導致公路客運與臺東市區的
# 站牌代碼配對率為 0%。此處採用獨立腳本原本的正確寫法。
normalize_stop_code_fixed <- function(x) {
  x <- trimws(toupper(as.character(x)))
  x <- sub("^(HUA|TTT|THB|TPE|HSZ)", "", x)
  x[x %in% c("", "NA", "N/A", "NULL", "-99")] <- NA_character_
  x
}

# 同時輸出到 console 與 output/
emit <- function(dt, filename, title) {
  cat("\n========================================\n")
  cat(title, "\n")
  cat("========================================\n")
  print(dt)
  fwrite(dt, file.path(out_dir, filename), bom = TRUE)
  cat("\n已寫入：output/", filename, "\n", sep = "")
  invisible(dt)
}

# ------------------------------------------------------------
# 載入報告本身的里程配對函數
#
# 直接從 實作測試/115期末.Rmd 擷取「月平均搭乘里程」的函數定義
# （站牌代碼標準化 → 站距索引 → 單端點配對 → 月平均主函數），
# 不重寫演算法，以確保稽核結果與已交付數字可對帳。
#
# 以區塊標題定位而非行號，Rmd 內容小幅變動仍可運作。
# 只擷取「函數定義」部分，不含第 7 節之後依賴外部變數的呼叫。
# ------------------------------------------------------------

load_mileage_functions <- function(
  rmd = file.path(REPO_ROOT, "實作測試", "115期末.Rmd")
) {

  if (!file.exists(rmd)) stop("找不到 Rmd：", rmd)

  lines <- readLines(rmd, encoding = "UTF-8", warn = FALSE)

  i1 <- grep("# 1. 站牌代碼標準化",            lines, fixed = TRUE)
  i3 <- grep("# 3. 建立四組站距索引",          lines, fixed = TRUE)
  i4 <- grep("# 4. 建立「精確站序 fallback」表", lines, fixed = TRUE)
  i7 <- grep("# 7. 四種運具計算",              lines, fixed = TRUE)

  if (any(lengths(list(i1, i3, i4, i7)) != 1)) {
    stop(
      "無法在 Rmd 中定位里程函數區塊（各標記命中數：",
      paste(lengths(list(i1, i3, i4, i7)), collapse = "/"),
      "）。Rmd 結構可能已變更，請改以人工指定行號。"
    )
  }

  # 跳過第 3 節：它是對四組站距資料的實際呼叫，依賴本腳本沒有載入的變數。
  # 只保留純函數定義（第 1、2、4、5、6 節）。
  block <- lines[c(i1:(i3 - 2), (i4 - 1):(i7 - 2))]

  tmp <- tempfile(fileext = ".R")
  writeLines(block, tmp, useBytes = TRUE)
  source(tmp, local = globalenv(), encoding = "UTF-8")
  unlink(tmp)

  cat("[setup] 已從 115期末.Rmd 載入里程配對函數（第",
      i1, "~", i7 - 2, "行）\n")

  invisible(TRUE)
}


# ------------------------------------------------------------
# 原始票證筆數稽核（A3、A4 共用）
#
# 【2026-09-11 更新：欄位定義經電話向票證協作平臺（林羽晧先生）確認】
#
# `原始票證筆數` 是「平臺處理及合併資料時產生的重複值數量」，
# **並非實際搭乘次數**。凡其他欄位完全相同的重複紀錄會被壓成
# 一列，本欄記錄壓了幾筆重複；這些重複是平臺端處理產物，
# 不是乘客搭了幾次。因此：
#   - 人次類指標必須以列數計算（.N），**不可**加總本欄
#   - 115期末.Rmd 各處 sum(原始票證筆數) 均屬誤用，見 C4
#   - 本欄 > 1 的月份代表平臺端曾發生重複合併，而非運量倍增
#
# 下列 weight_audit()／weight_dist()／flag_weight_anomaly()
# 保留作為「平臺端重複事件」的偵測工具：正常月份本欄應全為 1，
# 平均值偏離 1 即代表該月交付資料含重複。
# （函數與欄位名稱沿用「權重」一詞以維持 output/ 檔名穩定。）
# ------------------------------------------------------------

weight_audit <- function(d, weight_col = "原始票證筆數") {
  d[
    ,
    .(
      列數     = .N,
      人次     = sum(get(weight_col), na.rm = TRUE),
      最小值   = min(get(weight_col), na.rm = TRUE),
      中位數   = as.numeric(median(get(weight_col), na.rm = TRUE)),
      平均權重 = round(mean(get(weight_col), na.rm = TRUE), 3),
      最大值   = max(get(weight_col), na.rm = TRUE),
      權重非1列數 = sum(get(weight_col) != 1, na.rm = TRUE)
    ),
    by = .(西元年月, 民國月份)
  ][order(西元年月)][
    , `:=`(
      虛增人次 = 人次 - 列數,
      `權重非1占比%` = round(100 * 權重非1列數 / 列數, 1)
    )
  ][]
}

# 權重值 × 月份 交叉表
weight_dist <- function(d, weight_col = "原始票證筆數") {
  dcast(
    d[, .(列數 = .N), by = .(西元年月, 權重 = get(weight_col))],
    西元年月 ~ 權重,
    value.var = "列數",
    fill = 0L
  )
}

# 依門檻標記異常月份
flag_weight_anomaly <- function(audit, tol = 0.05) {
  audit[abs(平均權重 - 1) > tol,
        .(西元年月, 民國月份, 列數, 人次, 平均權重, 虛增人次, `權重非1占比%`)]
}

cat("\n[00-setup] 完成。資料路徑：", data_path, "\n")
cat("[00-setup] 輸出路徑：", out_dir, "\n\n")
