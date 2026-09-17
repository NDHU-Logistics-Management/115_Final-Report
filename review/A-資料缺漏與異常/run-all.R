# ============================================================
# A 類稽核：一次執行全部
#
# 用法：
#   Rscript run-all.R
#   或在 RStudio 中 source 本檔
#
# 若 repo 不在預設路徑，先設定：
#   REPO_ROOT <- "你的路徑/115_Final-Report"
#
# 執行時間：約 3~4 分鐘（需讀取約 3 GB 的 CSV；A6 含完整里程配對，最耗時）
# 輸出：全部寫入 output/，並在最後列出摘要
# ============================================================

# ------------------------------------------------------------
# 定位本腳本所在資料夾（支援 Rscript 與 source 兩種呼叫方式）
# ------------------------------------------------------------

this_file <- function() {
  a <- commandArgs(trailingOnly = FALSE)
  m <- grep("^--file=", a, value = TRUE)
  if (length(m) > 0) return(normalizePath(sub("^--file=", "", m[1])))
  of <- tryCatch(sys.frames()[[1]]$ofile, error = function(e) NULL)
  if (!is.null(of)) return(normalizePath(of))
  NA_character_
}

script_dir <- tryCatch(dirname(this_file()), error = function(e) NA_character_)

if (!is.na(script_dir) && dir.exists(script_dir)) {
  setwd(script_dir)
}

cat("工作目錄：", getwd(), "\n\n")


# ------------------------------------------------------------
# 共用設定
# ------------------------------------------------------------

source("00-setup.R")


# ------------------------------------------------------------
# 依序執行各項稽核
#
# 每支腳本各自獨立可執行；此處統一呼叫並記錄耗時。
# 任一支失敗不中斷其餘項目，錯誤訊息集中於最後回報。
# ------------------------------------------------------------

scripts <- c(
  A1 = "A1-臺東公路業者資料覆蓋.R",
  A2 = "A2-花蓮市區業者資料覆蓋.R",
  A3 = "A3-臺東市區權重異常.R",
  A4 = "A4-花蓮115年4月權重異常.R",
  A3A4 = "A3A4-原始票證筆數跨資料集業者月份分布.R",
  A5 = "A5-花蓮市區搭乘次數分布斷裂.R",
  A6 = "A6-路線改制里程組成斷點.R",
  A6b = "A6-1123回填可行性.R"
)

results <- data.table::data.table(
  項目   = names(scripts),
  腳本   = unname(scripts),
  狀態   = NA_character_,
  秒數   = NA_real_,
  訊息   = NA_character_
)

for (i in seq_along(scripts)) {

  cat("\n\n############################################\n")
  cat("#  ", names(scripts)[i], " ", scripts[i], "\n")
  cat("############################################\n\n")

  t0 <- Sys.time()

  ok <- tryCatch({
    source(scripts[i], local = new.env(), encoding = "UTF-8")
    TRUE
  }, error = function(e) {
    results[i, 訊息 := conditionMessage(e)]
    FALSE
  })

  results[i, `:=`(
    狀態 = if (isTRUE(ok)) "成功" else "失敗",
    秒數 = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  )]
}


# ------------------------------------------------------------
# 摘要
# ------------------------------------------------------------

cat("\n\n############################################\n")
cat("#   執行摘要\n")
cat("############################################\n\n")

print(results)

cat("\n輸出檔案（", out_dir, "）：\n", sep = "")
print(sort(basename(list.files(out_dir, pattern = "csv$"))))

if (any(results$狀態 == "失敗")) {
  cat("\n有項目執行失敗，詳見上表『訊息』欄。\n")
} else {
  cat("\n全部項目執行完成。\n")
}
