# 四組資料：異常值旗標為 1 的逐月列數與占比
suppressPackageStartupMessages(library(data.table))
options(width = 200)
dp <- "d:/Github/115_Final-Report/data"
start <- as.Date("2024-06-01"); end <- as.Date("2026-06-30")
routes_hua <- c("1121","1122","1123","1125","1126","1128","1129","1130","1132","1133","1135","1136","1137","1139","1140","1141","1142","1143","1145","8119","8161","8173","8181")
routes_ttt <- c("1145","309","8101","8102","8103","8105","8107","8109","8110","8111","8113","8115","8117","8119","8120","8122","8125","8128","8129","8130","8131","8132","8135","8136","8137","8138","8150","8151","8152","8153","8156","8157","8158","8161","8163","8165","8166","8167","8168","8170","8171","8172","8173","8178","8180","8181")
hua_bad <- c("50","51","52","53","71","72","73","81","83","綠線")
ttt_bad_d <- as.Date(c("2026-05-24","2026-05-25")); ttt_ok <- c("101","201","202","203")

rd <- function(f) {
  d <- fread(file.path(dp, f), select = c(8, 16, 20, 26, 30),
             colClasses = list(character = c(1, 4)), showProgress = FALSE)
  setnames(d, c("路線", "上車時間", "下車時間", "旗標", "日期"))
  d[, 日期 := as.Date(日期)]
  d[, `:=`(路線 = trimws(路線), 旗標 = trimws(旗標))]
  d[日期 >= start & 日期 <= end]
}

bymonth <- function(d, label) {
  d[, 月 := sprintf("%03d/%02d", as.integer(format(日期, "%Y")) - 1911, as.integer(format(日期, "%m")))]
  d[, .(列數 = .N,
        異常列數 = sum(旗標 == "1"),
        `占比%` = round(100 * mean(旗標 == "1"), 2),
        未刷下車 = sum(旗標 == "1" & grepl("^1970", 下車時間)),
        未刷上車 = sum(旗標 == "1" & grepl("^1970", 上車時間))), by = 月][order(月)][, 資料 := label][]
}

hua_c <- rd("花蓮縣公車.csv"); r1 <- bymonth(hua_c[!路線 %chin% hua_bad], "花蓮市區"); rm(hua_c); invisible(gc())
ttt_c <- rd("臺東縣公車.csv"); r2 <- bymonth(ttt_c[!(日期 %in% ttt_bad_d) | 路線 %chin% ttt_ok], "臺東市區"); rm(ttt_c); invisible(gc())
thb <- rd("公路客運2024_to_202606.csv")
r3 <- bymonth(thb[路線 %chin% routes_hua], "花蓮公路")
r4 <- bymonth(thb[路線 %chin% routes_ttt], "臺東公路"); rm(thb); invisible(gc())

all <- rbindlist(list(r1, r2, r3, r4))
cat("=== 異常值列數（依資料代表日期歸月）===\n")
print(dcast(all, 月 ~ 資料, value.var = "異常列數"))
cat("\n=== 異常值占該月列數之百分比 ===\n")
print(dcast(all, 月 ~ 資料, value.var = "占比%"))
cat("\n=== 四組合計 ===\n")
print(all[, .(總列數 = sum(列數), 異常列數 = sum(異常列數),
              `占比%` = round(100 * sum(異常列數) / sum(列數), 2),
              未刷下車 = sum(未刷下車), 未刷上車 = sum(未刷上車)), by = 資料])
fwrite(all, "C:/Users/Yi-Xuan/AppData/Local/Temp/claude/d--Github-115-Midterm-Report/774db9ef-9c39-4588-b377-da3b5d381b09/scratchpad/flag_by_month.csv", bom = TRUE)
cat("\n完成\n")
