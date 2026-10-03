# ============================================================
# 讀取資料
#
# 票證 CSV 三份（約 1.3 GB）與站間距離 CSV 四份。
# 一般不直接執行本檔：03_clean_data.R 在快取失效時才會 source 它。
# ============================================================

source(file.path(if (exists("CODE_DIR")) CODE_DIR else ".", "00_設定.R"), encoding = "UTF-8")

# =========================
# 1. 票證資料
# =========================

ticket_files <- list.files(
  path = data_path,
  pattern = "\\.csv$",
  full.names = TRUE
)

ticket_data <- lapply(
  ticket_files,
  fread
)

names(ticket_data) <- tools::file_path_sans_ext(
  basename(ticket_files)
)

# =========================
# 2. 站間距離資料
# =========================

distance_files <- list.files(
  path = distance_path,
  pattern = "\\.csv$",
  full.names = TRUE
)

distance_data <- lapply(
  distance_files,
  fread
)

names(distance_data) <- tools::file_path_sans_ext(
  basename(distance_files)
)

# =========================
# 檢查讀到哪些檔案
# =========================

names(ticket_data)

names(distance_data)

# =========================
# 細分資料
# =========================

公路客運 <- ticket_data[["公路客運2024_to_202606"]]
花蓮市區 <- ticket_data[["花蓮縣公車"]]
臺東市區 <- ticket_data[["臺東縣公車"]]

花蓮公路站距 <- distance_data[["花蓮縣公路客運站間距離資料"]]
花蓮市區站距 <- distance_data[["花蓮縣市區客運站間距離資料"]]
臺東公路站距 <- distance_data[["臺東縣公路客運站間距離資料"]]
臺東市區站距 <- distance_data[["臺東縣市區客運站間距離資料"]]

rm(ticket_data, distance_data)
