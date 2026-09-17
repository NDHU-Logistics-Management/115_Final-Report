# 臺東市區 TPASS 114/09～114/10「僅一次」占比驟降之成因：
# 異常旗標列（未刷下車）與同卡同路線同上車站同上車時間之正常列重複。
#
# 輸出：
#   output/C3-臺東市區TPASS重複列-逐月.csv   逐月：搭乘次數區間、旗標列、重複列、去重後重算
#   output/C3-臺東市區TPASS重複列-逐日.csv   114/09～114/10 逐日旗標列占比
#   output/C3-臺東市區TPASS重複列-依路線.csv 114/09～114/10 旗標列依路線
#   output/C3-臺東市區TPASS重複列-樣本.csv   重複列樣本（卡號取前 8 碼）
#
# 資料範圍與整併版 Rmd 相同：資料代表日期 113/06～115/06，
# 2026-05-24、2026-05-25 僅保留 101／201／202／203；TPASS = 票種次類型 #HUA-199／#HUA-399／#TTT-299。
# 搭乘次數區間與 Rmd 相同：每卡每月列數 1／2／3–4／5+。
suppressPackageStartupMessages(library(data.table))
options(width = 200)
dp <- "d:/Github/115_Final-Report/data"
od <- "d:/Github/115_Final-Report/review/D-方法與一致性/output"
start <- as.Date("2024-06-01"); end <- as.Date("2026-06-30")
ttt_bad_d <- as.Date(c("2026-05-24", "2026-05-25")); ttt_ok <- c("101", "201", "202", "203")
tpass_types <- c("#HUA-199", "#HUA-399", "#TTT-299")

d <- fread(file.path(dp, "臺東縣公車.csv"),
           select = c(2, 6, 7, 8, 13, 14, 16, 17, 18, 20, 21, 26, 30),
           colClasses = "character", showProgress = FALSE)
setnames(d, c("卡號", "票種次類型", "路線代碼", "路線", "上車站代碼", "上車站", "上車時間",
              "下車站代碼", "下車站", "下車時間", "收費價格", "旗標", "日期"))
d[, 日期 := as.Date(日期)]
d <- d[日期 >= start & 日期 <= end]
d <- d[!(日期 %in% ttt_bad_d) | 路線 %chin% ttt_ok]
d[, 月 := sprintf("%03d/%02d", as.integer(format(日期, "%Y")) - 1911, as.integer(format(日期, "%m")))]
d[, TPASS := 票種次類型 %chin% tpass_types]
d[, 未刷下車 := 旗標 == "1" & grepl("^1970", 下車時間)]
d[, 未刷上車 := 旗標 == "1" & grepl("^1970", 上車時間)]
# 重複判定鍵：同卡號、同路線、同上車站、同上車時間（小時級）
d[, key := paste(卡號, 路線代碼, 上車站代碼, 上車時間, sep = "|")]
ok_keys <- d[旗標 == "0", unique(key)]
d[, 重複 := 未刷下車 & key %chin% ok_keys]

# ---------- 逐月 ----------
tp <- d[TPASS == TRUE]
bucket <- function(x) {
  pc <- x[, .(n = .N), by = .(月, 卡號)]
  pc[, .(人數 = .N, 僅一次 = sum(n == 1), 二次 = sum(n == 2), 三四次 = sum(n %in% 3:4),
         五次以上 = sum(n >= 5), 人次 = sum(n)), by = 月]
}
orig <- bucket(tp)
orig[, `僅一次%` := round(100 * 僅一次 / 人數, 1)]
orig[, `五次以上%` := round(100 * 五次以上 / 人數, 1)]
dedup <- bucket(tp[重複 == FALSE])[, .(月, 去重後人次 = 人次,
                                        `去重後僅一次%` = round(100 * 僅一次 / 人數, 1),
                                        `去重後五次以上%` = round(100 * 五次以上 / 人數, 1))]
flag <- tp[, .(旗標列 = sum(旗標 == "1"), `旗標%` = round(100 * mean(旗標 == "1"), 1),
               旗標未刷下車列 = sum(未刷下車), 旗標未刷上車列 = sum(未刷上車),
               與正常列重複 = sum(重複),
               `重複率%` = round(100 * sum(重複) / max(sum(未刷下車), 1), 1)), by = 月]
non <- d[TPASS == FALSE, .(非TPASS旗標未刷下車列 = sum(未刷下車),
                            `非TPASS重複率%` = round(100 * sum(重複) / max(sum(未刷下車), 1), 1)), by = 月]
monthly <- Reduce(function(a, b) merge(a, b, by = "月", all.x = TRUE), list(orig, flag, dedup, non))[order(月)]
setcolorder(monthly, c("月", "人數", "僅一次", "二次", "三四次", "五次以上", "僅一次%", "五次以上%", "人次",
                       "旗標列", "旗標%", "旗標未刷下車列", "旗標未刷上車列", "與正常列重複", "重複率%",
                       "去重後人次", "去重後僅一次%", "去重後五次以上%",
                       "非TPASS旗標未刷下車列", "非TPASS重複率%"))
cat("=== 臺東市區 TPASS 逐月：搭乘次數區間、旗標列、重複列、去重後重算 ===\n")
print(monthly, nrow = 100)
fwrite(monthly, file.path(od, "C3-臺東市區TPASS重複列-逐月.csv"), bom = TRUE)

# ---------- 114/09～114/10 逐日 ----------
m2 <- c("114/09", "114/10")
daily <- tp[月 %chin% m2, .(TPASS列數 = .N, 旗標列 = sum(旗標 == "1"),
                            `旗標%` = round(100 * mean(旗標 == "1"), 1),
                            重複列 = sum(重複), 卡數 = uniqueN(卡號)), by = .(月, 日期)][order(日期)]
cat("\n=== 114/09～114/10 逐日（TPASS）===\n")
print(daily, nrow = 100)
fwrite(daily, file.path(od, "C3-臺東市區TPASS重複列-逐日.csv"), bom = TRUE)

# ---------- 依路線 ----------
byroute <- tp[月 %chin% m2, .(TPASS列數 = .N, 旗標列 = sum(旗標 == "1"), 重複列 = sum(重複),
                              `旗標%` = round(100 * mean(旗標 == "1"), 1)), by = .(月, 路線)][order(月, -旗標列)]
cat("\n=== 114/09～114/10 依路線（TPASS）===\n")
print(byroute)
fwrite(byroute, file.path(od, "C3-臺東市區TPASS重複列-依路線.csv"), bom = TRUE)

# ---------- 人數與卡數對照 ----------
cards <- tp[月 %chin% c("114/08", m2, "114/11"),
            .(卡數 = uniqueN(卡號), 含旗標列之卡數 = uniqueN(卡號[旗標 == "1"]),
              含重複列之卡數 = uniqueN(卡號[重複 == TRUE])), by = 月][order(月)]
cat("\n=== 114/08～114/11 卡數對照（TPASS）===\n")
print(cards)

# ---------- 樣本 ----------
ex_cards <- tp[月 == "114/09" & 重複 == TRUE, unique(卡號)][1:3]
sample <- tp[卡號 %chin% ex_cards & 月 == "114/09",
             .(卡號 = substr(卡號, 1, 8), 路線, 上車站, 上車時間, 下車站, 下車時間, 收費價格, 旗標, 重複)][order(卡號, 上車時間, 旗標)]
cat("\n=== 重複列樣本（114/09，三張卡）===\n")
print(sample, nrow = 200)
fwrite(sample, file.path(od, "C3-臺東市區TPASS重複列-樣本.csv"), bom = TRUE)

cat("\n完成\n")
