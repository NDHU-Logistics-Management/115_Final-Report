# ============================================================
# 一鍵重出 revise/output 下的全部圖表，並對帳
#
# 用法（在任何工作目錄）：
#   Rscript "revise/code/run-all.R"
#
# 步驟：
#   1. 依序 source 四個章節檔，輸出到 revise/output/fig（22 張 png）與
#      revise/output/tbl（4 份 xlsx）。資料清理結果快取在 revise/cache/clean_data.RData，
#      CSV 或清單未變時直接讀快取（約 1 分鐘）；否則重讀 CSV（約 5 分鐘）。
#      實作測試/ 內的原始輸出不動，留作對帳基準。
#   2. 對帳：99_對帳.R（寫 數值對帳.csv、數值對帳-摘要.csv，並與 review 稽核重算交叉比對）。
#
# 檔案：
#   00_設定.R        套件、路徑、期間、TPASS 票種、字型、圖上標記
#   01_load_data.R   讀 CSV（由 03 在快取失效時呼叫）
#   02_route_list.R  路線與異常值清單
#   03_clean_data.R  拆分、清理、A7 去重 → cache/clean_data.RData
#   1-TPASS使用比例.R ～ 4-平均轉乘次數.R   一章一檔，可各自單獨執行
#   99_對帳.R
#   檢查/            不影響輸出的檢查段
# ============================================================

this_file <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
if (length(this_file) == 0) this_file <- "run-all.R"   # 在 RStudio 內以 source() 執行時
CODE_DIR  <- normalizePath(dirname(this_file))

source(file.path(CODE_DIR, "00_設定.R"), encoding = "UTF-8")
cat("REPO_ROOT:", REPO_ROOT, "\nFIG_DIR:  ", FIG_DIR, "\nTBL_DIR:  ", TBL_DIR, "\n\n")

# ------------------------------------------------------------
# 1. 重出
# ------------------------------------------------------------
# 輸出 xlsx 若正被 Excel 開啟（會留下 ~$ 開頭的鎖定檔），write.xlsx 只會警告不會停，
# 結果是舊檔留在原地。這裡直接中止，避免圖新表舊。
locks <- list.files(TBL_DIR, pattern = "^~[$]", all.files = TRUE)
if (length(locks) > 0) {
  stop("請先關閉 Excel 中開啟的檔案再執行：", paste(sub("^~[$]", "", locks), collapse = "、"))
}

chapters <- c(
  "1-TPASS使用比例.R",
  "2-平均搭乘里程.R",
  "3-平均每日使用次數及其族群分布.R",
  "4-平均轉乘次數.R"
)

t0 <- Sys.time()
source(file.path(CODE_DIR, "03_clean_data.R"), encoding = "UTF-8")
for (ch in chapters) {
  cat("\n>>>>>", ch, "\n")
  source(file.path(CODE_DIR, ch), encoding = "UTF-8", echo = FALSE)
}

cat("\n============================================================\n")
cat("重出完成。耗時：", format(round(difftime(Sys.time(), t0, units = "mins"), 1)), "\n")
cat("fig：", length(list.files(FIG_DIR, pattern = "[.]png$")), "張\n")
cat("tbl：", length(list.files(TBL_DIR, pattern = "[.]xlsx$")), "份\n")
cat("============================================================\n")

# ------------------------------------------------------------
# 2. 對帳
# ------------------------------------------------------------
source(file.path(CODE_DIR, "99_對帳.R"), encoding = "UTF-8")
