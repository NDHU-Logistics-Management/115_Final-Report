# ============================================================
# 資料清理：公路拆分、市區異常值清理、A7 去重
#
# 產物：revise/cache/clean_data.RData（不進 git），內含
#   花蓮公路、臺東公路、花蓮市區_clean、臺東市區_clean、四組站距表
#
# 快取失效規則：data/ 下任一 CSV、02_route_list.R、本檔比 RData 新，就重讀 CSV 重建；
# 否則直接 load()。各章檔開頭呼叫 ensure_clean_data() 時會進到這裡。
# ============================================================

source(file.path(if (exists("CODE_DIR")) CODE_DIR else ".", "00_設定.R"), encoding = "UTF-8")
source(file.path(CODE_DIR, "02_route_list.R"), encoding = "UTF-8")

cache_inputs <- c(
  list.files(data_path, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE),
  file.path(CODE_DIR, c("02_route_list.R", "03_clean_data.R"))
)
cache_fresh <- file.exists(CLEAN_RDATA) &&
  file.mtime(CLEAN_RDATA) > max(file.mtime(cache_inputs))

if (cache_fresh) {

  cat("[03] 讀取快取：", CLEAN_RDATA, "\n")
  load(CLEAN_RDATA, envir = globalenv())

} else {

  cat("[03] 快取不存在或已過期，重讀 CSV 並清理\n")
  source(file.path(CODE_DIR, "01_load_data.R"), encoding = "UTF-8")

# =========================
# 公路客運拆分
# =========================

# 統一路線欄位格式
公路客運$搭乘路線名稱 <- trimws(
  as.character(
    公路客運$搭乘路線名稱
  )
)

# 花蓮公路客運
花蓮公路 <- 公路客運[
  搭乘路線名稱 %in% routes_hualien_THB
]

# 臺東公路客運
臺東公路 <- 公路客運[
  搭乘路線名稱 %in% routes_taitung_THB
]

# =========================
# 檢查
# =========================

nrow(花蓮公路)
nrow(臺東公路)

sort(unique(花蓮公路$搭乘路線名稱))
sort(unique(臺東公路$搭乘路線名稱))
# =========================
# 市區客運異常資料清理
# =========================

library(data.table)

# 日期格式
花蓮市區[, `資料代表日期(yyyy-MM-dd)` :=
  as.Date(`資料代表日期(yyyy-MM-dd)`)]

臺東市區[, `資料代表日期(yyyy-MM-dd)` :=
  as.Date(`資料代表日期(yyyy-MM-dd)`)]

# 路線名稱格式
花蓮市區[, 搭乘路線名稱 :=
  trimws(as.character(搭乘路線名稱))]

臺東市區[, 搭乘路線名稱 :=
  trimws(as.character(搭乘路線名稱))]

花蓮市區_clean <- 花蓮市區[
  !搭乘路線名稱 %in% hualien_abnormal_routes
]

臺東市區_clean <- 臺東市區[
  !(`資料代表日期(yyyy-MM-dd)` %in% taitung_abnormal_dates) |
    搭乘路線名稱 %in% taitung_normal_routes
]


# =========================
# 檢查清理結果
# =========================

cat(
  "花蓮市區原始筆數：", nrow(花蓮市區), "\n",
  "花蓮市區清理後筆數：", nrow(花蓮市區_clean), "\n",
  "花蓮市區排除筆數：",
  nrow(花蓮市區) - nrow(花蓮市區_clean),
  "\n\n"
)

cat(
  "臺東市區原始筆數：", nrow(臺東市區), "\n",
  "臺東市區清理後筆數：", nrow(臺東市區_clean), "\n",
  "臺東市區排除筆數：",
  nrow(臺東市區) - nrow(臺東市區_clean),
  "\n"
)

# =========================
# [修訂 A7] TPASS 重複列去除
# =========================
# 重複列：「是否包含異常值」= 1 且 刷卡下車時間 為 1970-01-01（未刷下車）的列，
# 若另有一筆旗標為 0 的正常列與其 卡號｜搭乘路線代碼｜上車站牌代碼｜刷卡上車時間 相同，
# 即為同一趟車的重複紀錄，刪除該旗標列；正常列保留，使用人數不變。
# 規則對四組資料全期間一律套用；實際只有 花蓮市區 114/08–114/10、臺東市區 114/09–114/10 被實質改變。

remove_duplicate_rows <- function(x, label) {
  x <- copy(x)
  flag_col <- grep("^是否包含異常值", names(x), value = TRUE)
  stopifnot(length(flag_col) == 1)
  x[, A7_key := paste(卡號, 搭乘路線代碼, 上車站牌代碼, as.character(刷卡上車時間), sep = "|")]
  ok_keys <- x[get(flag_col) == 0, unique(A7_key)]
  x[, A7_dup :=
      !is.na(get(flag_col)) & get(flag_col) == 1 &
      substr(as.character(刷卡下車時間), 1, 4) == "1970" &
      !is.na(卡號) & 卡號 != "" &
      A7_key %chin% ok_keys]
  by_month <- x[A7_dup == TRUE, .N, by = .(月 = format(as.Date(`資料代表日期(yyyy-MM-dd)`), "%Y-%m"))][order(月)]
  cat("[A7] ", label, "：去除重複列 ", sum(x$A7_dup), " 筆（", nrow(x), " → ", nrow(x) - sum(x$A7_dup), "）\n", sep = "")
  if (nrow(by_month) > 0) print(by_month[N >= 10])
  x <- x[A7_dup == FALSE]
  x[, c("A7_key", "A7_dup") := NULL]
  x[]
}

花蓮公路      <- remove_duplicate_rows(花蓮公路,      "花蓮公路")
臺東公路      <- remove_duplicate_rows(臺東公路,      "臺東公路")
花蓮市區_clean <- remove_duplicate_rows(花蓮市區_clean, "花蓮市區")
臺東市區_clean <- remove_duplicate_rows(臺東市區_clean, "臺東市區")

  save(list = CLEAN_OBJECTS, file = CLEAN_RDATA)
  cat("[03] 已寫出快取：", CLEAN_RDATA, "\n")
  rm(公路客運, 花蓮市區, 臺東市區)

}
