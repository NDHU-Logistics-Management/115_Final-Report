suppressPackageStartupMessages({library(openxlsx); library(data.table)})
options(width = 220)
o <- as.data.table(read.xlsx("d:/Github/115_Final-Report/實作測試/平均每日使用次數及其族群分布.xlsx", sheet = "全部乘客_臺東市區"))
n <- as.data.table(read.xlsx("d:/Github/115_Final-Report/revise/output/tbl/平均每日使用次數及其族群分布.xlsx", sheet = "全部乘客_臺東市區"))
cat("舊檔欄位：", paste(names(o), collapse = " / "), "\n新檔欄位：", paste(names(n), collapse = " / "), "\n\n")
cols <- setdiff(names(o), "月份")
diff_months <- o$月份[rowSums(o[, ..cols] != n[, ..cols]) > 0]
cat("有差異的月份：", paste(diff_months, collapse = "、"), "\n\n")
for (m in diff_months) {
  cat("=== ", m, " ===\n")
  print(rbind(cbind(版本 = "舊（實作測試）", o[月份 == m]), cbind(版本 = "新（revise）", n[月份 == m])))
}
cat("\n其餘", 25 - length(diff_months), "個月份每一格完全相同。\n")
