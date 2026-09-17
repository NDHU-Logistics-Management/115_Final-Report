# E1 交通部三題：花蓮公路 TPASS 乘客「五次以上」與「僅一次」族群之月內側寫
# 月內可算（公路卡號逐月穩定）；跨月不追蹤。
suppressPackageStartupMessages(library(data.table))
data_path <- "d:/Github/115_Final-Report/data"
out <- "."
start_date <- as.Date("2024-06-01"); end_date <- as.Date("2026-06-30")
routes_hualien_THB <- c("1121","1122","1123","1125","1126","1128","1129","1130","1132","1133","1135","1136","1137","1139","1140","1141","1142","1143","1145","8119","8161","8173","8181")
tpass_types <- c("#HUA-199", "#HUA-399", "#TTT-299")

# 欄位：2 卡號 4 持卡身分 6 票種次類型 8 搭乘路線名稱 14 上車站牌名稱 16 刷卡上車時間 30 日期
d <- fread(file.path(data_path, "公路客運2024_to_202606.csv"), select = c(2, 4, 6, 8, 14, 16, 30),
           colClasses = list(character = c(2, 4, 6, 8, 14)), showProgress = FALSE)
setnames(d, c("卡號", "持卡身分", "票種", "路線", "上車站", "上車時間", "日期"))
d[, 日期 := as.Date(日期)]
d <- d[日期 >= start_date & 日期 <= end_date & trimws(路線) %in% routes_hualien_THB & !is.na(卡號) & 卡號 != ""]
d[, `:=`(票種 = trimws(票種), 持卡身分 = trimws(持卡身分), 路線 = trimws(路線))]
d[, 西元年月 := format(日期, "%Y-%m")]
d[, 民國月份 := sprintf("%03d/%02d", as.integer(format(日期, "%Y")) - 1911, as.integer(format(日期, "%m")))]
d[, 小時 := as.integer(substr(上車時間, 12, 13))]
d[, 假日 := format(日期, "%u") %in% c("6", "7")]
tp <- d[票種 %in% tpass_types]
cat("花蓮公路 TPASS 列數：", nrow(tp), "\n")

# 每卡每月次數 → 族群
pm <- tp[, .(次數 = .N, 天數 = uniqueN(日期)), by = .(民國月份, 卡號)]
pm[, 族群 := fcase(次數 == 1, "僅一次",次數 <= 4, "二至四次", default = "五次以上")]
tp <- merge(tp, pm[, .(民國月份, 卡號, 族群, 次數, 天數)], by = c("民國月份", "卡號"))

sink(file.path(out, "E1-族群側寫.txt"))
cat("===== A. 各族群每月人數（核對表 3.2.3）=====\n")
print(dcast(pm[, .N, by = .(民國月份, 族群)], 民國月份 ~ 族群, value.var = "N"))

cat("\n===== B. 五次以上族群：每人每月平均次數、平均使用天數、每使用日次數（全期）=====\n")
print(pm[, .(人月數 = .N, 平均次數 = round(mean(次數), 2), 中位數次數 = as.numeric(median(次數)),
            平均使用天數 = round(mean(天數), 2), 每使用日次數 = round(mean(次數 / 天數), 2)), by = 族群])

cat("\n===== C. 持卡身分分布（全期，以人月計）=====\n")
ident <- unique(tp[, .(民國月份, 卡號, 族群, 持卡身分)])[, .N, by = .(族群, 持卡身分)]
ident[, `占比%` := round(100 * N / sum(N), 1), by = 族群]
print(dcast(ident, 持卡身分 ~ 族群, value.var = "占比%", fill = 0)[order(-`五次以上`)])

cat("\n===== D. 主要路線（以人次計，全期）=====\n")
rt <- tp[, .N, by = .(族群, 路線)][, `占比%` := round(100 * N / sum(N), 1), by = 族群][order(族群, -N)]
print(rt[, head(.SD, 8), by = 族群])

cat("\n===== E. 平日／假日人次占比 =====\n")
print(tp[, .(假日人次占比 = round(100 * mean(假日), 1)), by = 族群])

cat("\n===== F. 上車時段分布（人次占比%）=====\n")
hr <- tp[, .N, by = .(族群, 時段 = fcase(小時 < 6, "00-05", 小時 < 9, "06-08", 小時 < 12, "09-11", 小時 < 15, "12-14",
                                       小時 < 18, "15-17", 小時 < 21, "18-20", default = "21-23"))]
hr[, `占比%` := round(100 * N / sum(N), 1), by = 族群]
print(dcast(hr, 時段 ~ 族群, value.var = "占比%", fill = 0))

cat("\n===== G. 五次以上族群：路線集中度（每人每月使用不同路線數）=====\n")
print(tp[族群 == "五次以上", .(路線數 = uniqueN(路線)), by = .(民國月份, 卡號)][, .(
  `僅1條路線%` = round(100 * mean(路線數 == 1), 1), `2條%` = round(100 * mean(路線數 == 2), 1),
  `3條以上%` = round(100 * mean(路線數 >= 3), 1))])

cat("\n===== H. 114/12 該月（交通部引用月）各族群持卡身分 =====\n")
i12 <- unique(tp[民國月份 == "114/12", .(卡號, 族群, 持卡身分)])[, .N, by = .(族群, 持卡身分)]
i12[, `占比%` := round(100 * N / sum(N), 1), by = 族群]
print(dcast(i12, 持卡身分 ~ 族群, value.var = "占比%", fill = 0))

cat("\n===== I. 僅一次族群：全期人月數、其中假日搭乘占比、主要上車站（前 8）=====\n")
print(tp[族群 == "僅一次", .(人月數 = .N, 假日占比 = round(100 * mean(假日), 1))])
print(tp[族群 == "僅一次", .N, by = 上車站][order(-N)][1:8])
sink()
cat("完成\n")
