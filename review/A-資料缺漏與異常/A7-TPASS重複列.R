# ============================================================
# A7：TPASS 票證同一趟車被記成兩列（花蓮市區 114/08–114/10、臺東市區 114/09–114/10）
#
# 重複列定義：
#   「是否包含異常值」= 1 且 刷卡下車時間 = 1970-01-01 00:00:00（未刷下車）的列，
#   若另有一筆旗標為 0 的正常列與其 卡號、搭乘路線代碼、上車站牌代碼、刷卡上車時間 完全相同，
#   該旗標列即為重複列。
#
# 本腳本回答：
#   1. 四組資料逐月：旗標列、未刷下車列、重複列、重複率；TPASS 與非 TPASS 分開
#   2. 重複列的實際樣貌（樣本）
#   3. 去除重複列後，各指標逐月變動：TPASS 使用比例、平均每日使用次數／單日平均使用次數、
#      全部乘客與 TPASS 搭乘次數區間、平均轉乘次數（指標定義照抄 revise/code/115期末-修訂.Rmd）
#   平均搭乘里程不受影響：重複列下車站序為 -99，配對必然失敗，本來就不在分子分母。
#
# 資料範圍與整併版 Rmd 相同：資料代表日期 113/06～115/06；花蓮市區排除 hua_bad 路線；
# 臺東市區 2026-05-24／25 只保留 101／201／202／203；公路依路線清單分花蓮／臺東。
#
# 輸出（output/）：
#   A7-重複率逐月.csv、A7-重複列樣本.csv、A7-指標影響-使用次數與區間.csv、A7-指標影響-轉乘.csv
# 耗時約 4 分鐘。
# ============================================================
suppressPackageStartupMessages(library(data.table))
options(width = 220)
if (!exists("REPO_ROOT")) REPO_ROOT <- "d:/Github/115_Final-Report"
dp <- file.path(REPO_ROOT, "data")
od <- file.path(REPO_ROOT, "review", "A-資料缺漏與異常", "output")
start <- as.Date("2024-06-01"); end <- as.Date("2026-06-30")
routes_hua <- c("1121","1122","1123","1125","1126","1128","1129","1130","1132","1133","1135","1136","1137","1139","1140","1141","1142","1143","1145","8119","8161","8173","8181")
routes_ttt <- c("1145","309","8101","8102","8103","8105","8107","8109","8110","8111","8113","8115","8117","8119","8120","8122","8125","8128","8129","8130","8131","8132","8135","8136","8137","8138","8150","8151","8152","8153","8156","8157","8158","8161","8163","8165","8166","8167","8168","8170","8171","8172","8173","8178","8180","8181")
hua_bad <- c("50","51","52","53","71","72","73","81","83","綠線")
ttt_bad_d <- as.Date(c("2026-05-24","2026-05-25")); ttt_ok <- c("101","201","202","203")
tpass_types <- c("#HUA-199", "#HUA-399", "#TTT-299")
transfer_limit <- 120

rd <- function(f) {
  d <- fread(file.path(dp, f), select = c(2, 6, 7, 8, 13, 14, 16, 17, 18, 20, 26, 30),
             colClasses = "character", showProgress = FALSE)
  setnames(d, c("卡號", "票種次類型", "路線代碼", "路線", "上車站代碼", "上車站", "上車時間",
                "下車站代碼", "下車站", "下車時間", "旗標", "日期"))
  d[, 日期 := as.Date(日期)]
  d[, `:=`(路線 = trimws(路線), 旗標 = trimws(旗標), 卡號 = trimws(卡號))]
  d[日期 >= start & 日期 <= end & !is.na(卡號) & 卡號 != ""]
}

mark <- function(d, label) {
  d[, 資料 := label]
  d[, 月 := sprintf("%03d/%02d", as.integer(format(日期, "%Y")) - 1911, as.integer(format(日期, "%m")))]
  d[, TPASS := 票種次類型 %chin% tpass_types]
  d[, 未刷下車 := 旗標 == "1" & grepl("^1970", 下車時間)]
  d[, key := paste(卡號, 路線代碼, 上車站代碼, 上車時間, sep = "|")]
  ok_keys <- d[旗標 == "0", unique(key)]
  d[, 重複 := 未刷下車 & key %chin% ok_keys]
  d[, key := NULL]
  d
}

# ---------- 1. 逐月重複率 ----------
rate <- function(d) {
  d[, .(列數 = .N, 旗標列 = sum(旗標 == "1"), 未刷下車列 = sum(未刷下車), 重複列 = sum(重複),
        `重複列占未刷下車%` = round(100 * sum(重複) / max(sum(未刷下車), 1), 1),
        `重複列占全月%` = round(100 * sum(重複) / .N, 2),
        TPASS列數 = sum(TPASS), TPASS重複列 = sum(重複 & TPASS),
        `TPASS重複列占TPASS%` = round(100 * sum(重複 & TPASS) / max(sum(TPASS), 1), 1),
        非TPASS重複列 = sum(重複 & !TPASS)), by = .(資料, 月)][order(資料, 月)]
}

# ---------- 3a. 使用次數與區間（照 Rmd 定義）----------
days_in <- function(ym) {
  y <- as.integer(substr(ym, 1, 3)) + 1911; m <- as.integer(substr(ym, 5, 6))
  first <- as.Date(sprintf("%d-%02d-01", y, m)); as.integer(format(seq(first, by = "month", length.out = 2)[2] - 1, "%d"))
}
usage <- function(x, label) {
  # 平均每日使用次數 = 總使用次數 / (使用人數 × 天數)；花蓮市區改單日平均 = 總使用次數 / 使用人合計日數
  m <- x[, .(總使用次數 = .N, 使用人數 = uniqueN(卡號), 使用人合計日數 = uniqueN(paste(卡號, 日期))), by = 月]
  m[, 天數 := sapply(月, days_in)]
  m[, 平均每日使用次數 := round(總使用次數 / (使用人數 * 天數), 3)]
  m[, 單日平均使用次數 := round(總使用次數 / 使用人合計日數, 2)]
  band <- function(z, pre) {
    pm <- z[, .(n = .N), by = .(月, 卡號)]
    b <- pm[, .(僅一次 = sum(n == 1), 二至四次 = sum(n >= 2 & n <= 4), 五次以上 = sum(n >= 5)), by = 月]
    b[, `僅一次%` := round(100 * 僅一次 / (僅一次 + 二至四次 + 五次以上), 1)]
    b[, `五次以上%` := round(100 * 五次以上 / (僅一次 + 二至四次 + 五次以上), 1)]
    setnames(b, setdiff(names(b), "月"), paste0(pre, setdiff(names(b), "月"))); b
  }
  tpr <- x[, .(TPASS人次 = sum(TPASS), `TPASS使用比例%` = round(100 * mean(TPASS), 2)), by = 月]
  out <- Reduce(function(a, b) merge(a, b, by = "月"), list(tpr, m, band(x, "全部_"), band(x[TPASS == TRUE], "TPASS_")))
  out[, 版本 := label]; out
}
usage_compare <- function(d) {
  a <- usage(d, "原"); b <- usage(d[重複 == FALSE], "去重")
  r <- rbind(a, b)[order(月, -版本)]
  r[, 資料 := d$資料[1]]; setcolorder(r, c("資料", "月", "版本")); r
}

# ---------- 3b. 轉乘（照 Rmd：同卡同日相鄰、前趟下車→本趟上車 0～120 分、路線不同）----------
transfer <- function(x, label) {
  z <- x[!is.na(路線) & 路線 != "", .(日期, 卡號, 路線, TPASS, 月,
                                    上 = as.POSIXct(上車時間, format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Taipei"),
                                    下 = as.POSIXct(下車時間, format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Taipei"))]
  z <- z[!is.na(上) & !is.na(下)]
  setorder(z, 日期, 卡號, 上, 下)
  z[, `:=`(前下 = shift(下), 前路線 = shift(路線)), by = .(日期, 卡號)]
  z[, 間隔 := as.numeric(difftime(上, 前下, units = "mins"))]
  z[, 轉乘 := !is.na(間隔) & 間隔 >= 0 & 間隔 <= transfer_limit & !is.na(前路線) & 路線 != 前路線]
  r <- z[, .(總搭乘紀錄數 = .N, 總轉乘次數 = sum(轉乘), 平均轉乘次數 = round(sum(轉乘) / .N, 3),
             TPASS搭乘紀錄數 = sum(TPASS), TPASS轉乘次數 = sum(轉乘 & TPASS),
             TPASS平均轉乘次數 = round(sum(轉乘 & TPASS) / max(sum(TPASS), 1), 3)), by = 月][order(月)]
  r[, 版本 := label]; r
}
transfer_compare <- function(region, name) {
  a <- transfer(region, "原"); b <- transfer(region[重複 == FALSE], "去重")
  r <- rbind(a, b)[order(月, -版本)]; r[, 地區 := name]; setcolorder(r, c("地區", "月", "版本")); r
}

# ---------- 執行 ----------
hua_c <- mark(rd("花蓮縣公車.csv")[!路線 %chin% hua_bad], "花蓮市區")
ttt_c <- mark(rd("臺東縣公車.csv")[!(日期 %in% ttt_bad_d) | 路線 %chin% ttt_ok], "臺東市區")
thb <- rd("公路客運2024_to_202606.csv")
hua_h <- mark(thb[路線 %chin% routes_hua], "花蓮公路")
ttt_h <- mark(thb[路線 %chin% routes_ttt], "臺東公路"); rm(thb); invisible(gc())

rates <- rbindlist(lapply(list(hua_c, ttt_c, hua_h, ttt_h), rate))
cat("=== 1. 逐月重複率（只列有重複列的 資料×月）===\n")
print(rates[重複列 > 0], nrow = 200)
cat("\n=== 四組全期間合計 ===\n")
print(rates[, .(列數 = sum(列數), 旗標列 = sum(旗標列), 未刷下車列 = sum(未刷下車列), 重複列 = sum(重複列),
                TPASS重複列 = sum(TPASS重複列), 非TPASS重複列 = sum(非TPASS重複列)), by = 資料])
fwrite(rates, file.path(od, "A7-重複率逐月.csv"), bom = TRUE)

# ---------- 2. 樣本 ----------
pick <- function(d, m, k = 2) {
  cards <- d[月 == m & 重複 == TRUE, unique(卡號)][seq_len(k)]
  d[卡號 %chin% cards & 月 == m,
    .(資料, 卡號 = substr(卡號, 1, 8), 票種次類型, 路線, 上車站, 上車時間, 下車站, 下車時間, 旗標, 重複列 = 重複)][order(卡號, 上車時間, 旗標)]
}
sample <- rbind(pick(ttt_c, "114/09"), pick(hua_c, "114/09"))
cat("\n=== 2. 重複列樣本（臺東市區、花蓮市區 114/09 各兩張卡）===\n")
print(sample, nrow = 200)
fwrite(sample, file.path(od, "A7-重複列樣本.csv"), bom = TRUE)

# ---------- 3a ----------
usage_all <- rbindlist(lapply(list(hua_c, ttt_c, hua_h, ttt_h), usage_compare))
changed <- usage_all[, .(diff = 總使用次數[版本 == "原"] - 總使用次數[版本 == "去重"]), by = .(資料, 月)][diff > 0]
cat("\n=== 3a. 使用次數與區間：去重前後（只列總使用次數有變動的 資料×月）===\n")
print(usage_all[changed, on = .(資料, 月)][, .(資料, 月, 版本, `TPASS使用比例%`, 總使用次數, 使用人數, 使用人合計日數,
                                              平均每日使用次數, 單日平均使用次數,
                                              全部_僅一次, 全部_二至四次, 全部_五次以上, `全部_僅一次%`, `全部_五次以上%`,
                                              TPASS_僅一次, TPASS_二至四次, TPASS_五次以上, `TPASS_僅一次%`, `TPASS_五次以上%`)], nrow = 300)
fwrite(usage_all, file.path(od, "A7-指標影響-使用次數與區間.csv"), bom = TRUE)

# ---------- 3b ----------
花蓮地區 <- rbind(hua_c, hua_h); 臺東地區 <- rbind(ttt_c, ttt_h)
tr <- rbind(transfer_compare(花蓮地區, "花蓮地區"), transfer_compare(臺東地區, "臺東地區"))
chg <- tr[, .(diff = 總搭乘紀錄數[版本 == "原"] - 總搭乘紀錄數[版本 == "去重"]), by = .(地區, 月)][diff > 0]
cat("\n=== 3b. 平均轉乘次數：去重前後（只列有變動的 地區×月）===\n")
print(tr[chg, on = .(地區, 月)], nrow = 200)
fwrite(tr, file.path(od, "A7-指標影響-轉乘.csv"), bom = TRUE)

cat("\n完成\n")
