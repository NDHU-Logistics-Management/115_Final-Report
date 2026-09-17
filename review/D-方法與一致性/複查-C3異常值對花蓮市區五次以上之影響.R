# 關鍵檢查：花蓮市區「五次以上」占比（全部乘客），含異常 vs 刪異常
# A5 的核心現象是 114/11 起五次以上驟降。若刪掉旗標列後前期也降下來，
# 則該「斷點」有一部分來自旗標列本身，而非 114/11 才發生的變化。
# 同時檢查臺東市區作為對照。
suppressPackageStartupMessages(library(data.table))
options(width = 200)
dp <- "d:/Github/115_Final-Report/data"
start <- as.Date("2024-06-01"); end <- as.Date("2026-06-30")
hua_bad <- c("50","51","52","53","71","72","73","81","83","綠線")
ttt_bad_d <- as.Date(c("2026-05-24","2026-05-25")); ttt_ok <- c("101","201","202","203")

rd <- function(f) {
  d <- fread(file.path(dp, f), select = c(2, 8, 26, 30),
             colClasses = list(character = c(1, 2, 3)), showProgress = FALSE)
  setnames(d, c("卡號", "路線", "旗標", "日期"))
  d[, 日期 := as.Date(日期)]
  d[, `:=`(路線 = trimws(路線), 旗標 = trimws(旗標))]
  d <- d[日期 >= start & 日期 <= end & !is.na(卡號) & 卡號 != ""]
  d[, 月 := sprintf("%03d/%02d", as.integer(format(日期, "%Y")) - 1911, as.integer(format(日期, "%m")))]
  d
}
band <- function(x) {
  pm <- x[, .(n = .N), by = .(月, 卡號)]
  pm[, g := fcase(n == 1, "僅一次", n <= 4, "二至四次", default = "五次以上")]
  pm[, .(人數 = .N, 五次以上 = sum(g == "五次以上"),
         `五次以上%` = round(100 * mean(g == "五次以上"), 2)), by = 月][order(月)]
}
cmp <- function(d, label) {
  a <- band(d); b <- band(d[旗標 == "0"])
  m <- merge(a, b, by = "月", suffixes = c("_含", "_刪"))
  m[, 旗標占比 := round(100 * d[, mean(旗標 == "1"), by = 月][order(月)]$V1, 2)]
  cat("\n==========", label, "==========\n")
  print(m[, .(月, 旗標占比,
              人數_含, `五次以上%_含`, 人數_刪, `五次以上%_刪`,
              差 = round(`五次以上%_刪` - `五次以上%_含`, 2))])
}

hc <- rd("花蓮縣公車.csv"); cmp(hc[!路線 %chin% hua_bad], "花蓮市區　全部乘客"); rm(hc); invisible(gc())
tc <- rd("臺東縣公車.csv"); cmp(tc[!(日期 %in% ttt_bad_d) | 路線 %chin% ttt_ok], "臺東市區　全部乘客（對照）")
