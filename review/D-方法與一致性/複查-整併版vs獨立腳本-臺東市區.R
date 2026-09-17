# 量化：整併版 Rmd 與 臺東獨立腳本 對「臺東市區客運」的資料範圍差異
# 整併版：臺東縣公車.csv 全部，僅清掉 2026-05-24/25 非 101/201/202/203 的列
# 獨立版：業者=1202 且 搭乘路線代碼 ∈ {TTT0981,0982,0984,0985}，且 異常值旗標=="0"
suppressPackageStartupMessages(library(data.table))
options(width = 200)
p <- "d:/Github/115_Final-Report/data/臺東縣公車.csv"
d <- fread(p, select = c(1, 2, 7, 8, 26, 29, 30),
           colClasses = list(character = c(1, 2, 7, 8, 26)), showProgress = FALSE)
setnames(d, c("業者", "卡號", "路線代碼", "路線名稱", "異常旗標", "票證筆數", "日期"))
d[, 日期 := as.Date(日期)]
d <- d[日期 >= as.Date("2024-06-01") & 日期 <= as.Date("2026-06-30")]
d[, `:=`(業者 = trimws(業者), 路線代碼 = toupper(trimws(路線代碼)),
         路線名稱 = trimws(路線名稱), 異常旗標 = trimws(異常旗標))]
d[, 月 := sprintf("%03d/%02d", as.integer(format(日期, "%Y")) - 1911, as.integer(format(日期, "%m")))]

bad <- as.Date(c("2026-05-24", "2026-05-25")); ok <- c("101", "201", "202", "203")
d[, 整併版 := !(日期 %in% bad) | 路線名稱 %in% ok]
codes <- c("TTT0981", "TTT0982", "TTT0984", "TTT0985")
d[, 獨立版 := 業者 == "1202" & 路線代碼 %chin% codes & 異常旗標 == "0"]

cat("=== 全期列數 ===\n")
cat("整併版納入：", format(d[整併版 == TRUE, .N], big.mark = ","),
    "  獨立版納入：", format(d[獨立版 == TRUE, .N], big.mark = ","), "\n\n")

cat("=== 整併版納入、獨立版排除的列，按排除原因拆解 ===\n")
x <- d[整併版 == TRUE & 獨立版 == FALSE]
x[, 原因 := fcase(業者 != "1202", "業者非 1202",
                  !路線代碼 %chin% codes, "路線代碼不在市區白名單",
                  異常旗標 != "0", "異常值旗標 ≠ 0", default = "其他")]
print(x[, .(列數 = .N), by = 原因][order(-列數)])
cat("\n-- 業者非 1202 者的業者與路線 --\n")
print(x[業者 != "1202", .(列數 = .N), by = .(業者, 路線名稱)][order(-列數)][1:10])
cat("\n-- 1202 業者但路線代碼不在白名單者 --\n")
print(x[業者 == "1202" & !路線代碼 %chin% codes, .(列數 = .N), by = .(路線代碼, 路線名稱)][order(-列數)][1:12])
cat("\n-- 異常值旗標≠0（其餘條件都符合）--\n")
print(x[業者 == "1202" & 路線代碼 %chin% codes & 異常旗標 != "0", .(列數 = .N), by = 異常旗標])

cat("\n=== 獨立版納入、整併版排除的列 ===\n")
y <- d[整併版 == FALSE & 獨立版 == TRUE]
cat("列數：", nrow(y), "\n")
if (nrow(y) > 0) print(y[, .(列數 = .N), by = .(日期, 路線名稱)][order(-列數)][1:10])

cat("\n=== 逐月：兩版納入列數與使用人數 ===\n")
a <- d[整併版 == TRUE, .(整併_列數 = .N, 整併_人數 = uniqueN(卡號[卡號 != ""]),
                        整併_加總人次 = sum(票證筆數, na.rm = TRUE)), by = 月]
b <- d[獨立版 == TRUE, .(獨立_列數 = .N, 獨立_人數 = uniqueN(卡號[卡號 != ""]),
                        獨立_加總人次 = sum(票證筆數, na.rm = TRUE)), by = 月]
m <- merge(a, b, by = "月")[order(月)]
m[, `:=`(列數差 = 整併_列數 - 獨立_列數, 人數差 = 整併_人數 - 獨立_人數)]
print(m)
