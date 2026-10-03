# ============================================================
# 共用設定
#
# 套件、路徑、分析期間、TPASS 票種、中文字型、圖上標記函數。
# 各章與前處理檔開頭都會 source 本檔，重複 source 無副作用。
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)   # [修訂] days_in_month() 需要，原檔未載入
  library(openxlsx)
})

# =========================
# 路徑
# =========================

# [修訂] 路徑改為相對於 repo 根目錄。run-all.R 會先定義 CODE_DIR；
# 在 RStudio 逐檔執行時，工作目錄為 repo 根目錄或 revise/code 皆可。
if (!exists("CODE_DIR")) {
  CODE_DIR <- if (file.exists("00_設定.R")) getwd() else file.path(getwd(), "revise", "code")
}
CODE_DIR <- normalizePath(CODE_DIR)

if (!exists("REPO_ROOT")) REPO_ROOT <- normalizePath(file.path(CODE_DIR, "..", ".."))
if (!exists("FIG_DIR"))   FIG_DIR   <- file.path(REPO_ROOT, "revise", "output", "fig")
if (!exists("TBL_DIR"))   TBL_DIR   <- file.path(REPO_ROOT, "revise", "output", "tbl")
if (!exists("CACHE_DIR")) CACHE_DIR <- file.path(REPO_ROOT, "revise", "cache")
dir.create(FIG_DIR,   showWarnings = FALSE, recursive = TRUE)
dir.create(TBL_DIR,   showWarnings = FALSE, recursive = TRUE)
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

data_path     <- file.path(REPO_ROOT, "data")
distance_path <- file.path(data_path, "公車站間距離資料")

# 03_clean_data.R 的產物；各章需要的資料物件
CLEAN_RDATA   <- file.path(CACHE_DIR, "clean_data.RData")
CLEAN_OBJECTS <- c(
  "花蓮公路", "臺東公路", "花蓮市區_clean", "臺東市區_clean",
  "花蓮公路站距", "花蓮市區站距", "臺東公路站距", "臺東市區站距"
)

# 各章開頭呼叫：資料已在記憶體就沿用，否則由 03_clean_data.R 讀快取或重建
ensure_clean_data <- function() {
  if (!all(vapply(CLEAN_OBJECTS, exists, logical(1), envir = globalenv()))) {
    source(file.path(CODE_DIR, "03_clean_data.R"), encoding = "UTF-8")
  }
  invisible(NULL)
}

# =========================
# 分析期間與 TPASS 定義
# =========================

start_date = as.Date("2024-06-01")
end_date = as.Date("2026-06-30")

# TPASS 正式定義
tpass_types = c(
  "#HUA-199",
  "#HUA-399",
  "#TTT-299"
)

# =========================
# 中文字型
# =========================

windowsFonts(
  msjh = windowsFont("Microsoft JhengHei")
)

# =========================
# 圖上標記
# =========================

# [修訂 A1/A2] 業者未提交票證資料之月份：圖上該月資料點改為空心黑邊三角形
FLAG_MONTHS <- list(
  臺東公路 = "114/11",                        # A1 東台灣客運（0816）
  花蓮市區 = c("115/04", "115/05", "115/06")  # A2 太魯閣客運（0412）
)

# [修訂 A6] 花蓮市區 1123 路線改號為 311／311A 之時點：圖 2.1.1 加垂直虛線
VLINE_MONTHS <- list(
  花蓮市區 = "113/11"
)

# 在既有折線上覆蓋空心三角形。先鋪一個白色圓點蓋住原本的實心點，再畫三角形。
flag_points <- function(x, y, months, flag, cex = 1.3) {
  if (is.null(flag)) return(invisible(NULL))
  idx <- which(months %in% flag)
  if (length(idx) == 0) return(invisible(NULL))
  points(x[idx], y[idx], pch = 21, col = "white", bg = "white", cex = cex * 0.85)
  points(x[idx], y[idx], pch = 24, col = "black", bg = "white", cex = cex, lwd = 1.3)
  invisible(idx)
}

flag_vline <- function(months, vline) {
  if (is.null(vline)) return(invisible(NULL))
  idx <- which(months %in% vline)
  if (length(idx) == 0) return(invisible(NULL))
  abline(v = idx, lty = 2, lwd = 1.2, col = "grey30")
  invisible(idx)
}
