# ============================================================
# 一鍵重出 revise/output 下的全部圖表
#
# 用法（在任何工作目錄）：
#   Rscript "revise/code/run-all.R"
#
# 做法：把 115期末-修訂.Rmd 以 knitr::purl() 抽成純 R 檔後 source。
# 全部輸出寫到 revise/output/fig（png）與 revise/output/tbl（xlsx），
# 實作測試/ 內的原始輸出不動，留作對帳基準。
# ============================================================

this_file <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
if (length(this_file) == 0) this_file <- "run-all.R"   # 在 RStudio 內以 source() 執行時
code_dir  <- normalizePath(dirname(this_file))

REPO_ROOT <- normalizePath(file.path(code_dir, "..", ".."))
FIG_DIR   <- file.path(REPO_ROOT, "revise", "output", "fig")
TBL_DIR   <- file.path(REPO_ROOT, "revise", "output", "tbl")

cat("REPO_ROOT:", REPO_ROOT, "\nFIG_DIR:  ", FIG_DIR, "\nTBL_DIR:  ", TBL_DIR, "\n\n")

# 前置檢查：輸出 xlsx 若正被 Excel 開啟（會留下 ~$ 開頭的鎖定檔），write.xlsx 只會警告不會停，
# 結果是舊檔留在原地。這裡直接中止，避免圖新表舊。
locks <- list.files(TBL_DIR, pattern = "^~[$]", all.files = TRUE)
if (length(locks) > 0) {
  stop("請先關閉 Excel 中開啟的檔案再執行：", paste(sub("^~[$]", "", locks), collapse = "、"))
}

rmd <- file.path(code_dir, "115期末-修訂.Rmd")
r   <- file.path(code_dir, "115期末-修訂.R")

knitr::purl(rmd, output = r, documentation = 0, quiet = TRUE)
cat("已抽出：", r, "\n")

t0 <- Sys.time()
setwd(code_dir)
source(r, encoding = "UTF-8", echo = FALSE)

cat("\n============================================================\n")
cat("完成。耗時：", format(round(difftime(Sys.time(), t0, units = "mins"), 1)), "\n")
cat("fig：", length(list.files(FIG_DIR, pattern = "[.]png$")), "張\n")
cat("tbl：", length(list.files(TBL_DIR, pattern = "[.]xlsx$")), "份\n")
cat("============================================================\n")
