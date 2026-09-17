# B–E 複查用：C3 資料品質欄位分布、D3 轉乘代碼紀錄分布、D5 跨縣路線占比
# 只讀需要的欄位（以欄位索引選取，避免超長欄名）
suppressPackageStartupMessages(library(data.table))
data_path <- "d:/Github/115_Final-Report/data"
out <- "."
start_date <- as.Date("2024-06-01"); end_date <- as.Date("2026-06-30")

routes_hualien_THB <- c("1121","1122","1123","1125","1126","1128","1129","1130","1132","1133","1135","1136","1137","1139","1140","1141","1142","1143","1145","8119","8161","8173","8181")
routes_taitung_THB <- c("1145","309","8101","8102","8103","8105","8107","8109","8110","8111","8113","8115","8117","8119","8120","8122","8125","8128","8129","8130","8131","8132","8135","8136","8137","8138","8150","8151","8152","8153","8156","8157","8158","8161","8163","8165","8166","8167","8168","8170","8171","8172","8173","8178","8180","8181")
hualien_abnormal_routes <- c("50","51","52","53","71","72","73","81","83","綠線")
taitung_abnormal_dates <- as.Date(c("2026-05-24","2026-05-25")); taitung_normal_routes <- c("101","201","202","203")

rd <- function(f) {
  d <- fread(file.path(data_path, f), select = c(8, 20, 23, 26, 27, 28, 30),
             colClasses = list(character = c(8, 23)), showProgress = FALSE)
  setnames(d, c("路線", "下車時間", "轉乘代碼", "含異常值", "檢核錯誤代碼", "檢核結果", "日期"))
  d[, 日期 := as.Date(日期)]
  d[, 路線 := trimws(路線)]
  d[日期 >= start_date & 日期 <= end_date]
}

summarise_quality <- function(d, label) {
  n <- nrow(d)
  cat("\n=====", label, "  列數", format(n, big.mark = ","), "=====\n")
  cat("-- 檢核結果 --\n"); print(d[, .(列數 = .N, `占比%` = round(100 * .N / n, 2)), by = 檢核結果][order(檢核結果)])
  cat("-- 檢核錯誤代碼 --\n"); print(d[, .(列數 = .N, `占比%` = round(100 * .N / n, 2)), by = 檢核錯誤代碼][order(檢核錯誤代碼)])
  cat("-- 是否包含異常值 --\n"); print(d[, .(列數 = .N, `占比%` = round(100 * .N / n, 2)), by = 含異常值])
  cat("-- 下車時間為 1970 --\n"); print(d[, .(列數 = sum(grepl("^1970", 下車時間)), `占比%` = round(100 * mean(grepl("^1970", 下車時間)), 2))])
  cat("-- 轉乘代碼紀錄（D3）--\n")
  d[, 轉乘代碼 := trimws(轉乘代碼)]
  cat("有值列數：", format(sum(!is.na(d$轉乘代碼) & d$轉乘代碼 != ""), big.mark = ","),
      "  占比", round(100 * mean(!is.na(d$轉乘代碼) & d$轉乘代碼 != ""), 2), "%\n")
  print(d[!is.na(轉乘代碼) & 轉乘代碼 != "", .(列數 = .N), by = 轉乘代碼][order(-列數)][1:12])
  invisible(NULL)
}

thb <- rd("公路客運2024_to_202606.csv")
hua_thb <- thb[路線 %in% routes_hualien_THB]; ttt_thb <- thb[路線 %in% routes_taitung_THB]
summarise_quality(hua_thb, "花蓮公路"); summarise_quality(ttt_thb, "臺東公路")

cat("\n===== D5 跨縣路線占比 =====\n")
cross <- c("1145", "8119", "8161", "8173", "8181")
d5 <- rbind(
  hua_thb[, .(資料 = "花蓮公路", 總列數 = .N, 跨縣列數 = sum(路線 %in% cross))],
  ttt_thb[, .(資料 = "臺東公路", 總列數 = .N, 跨縣列數 = sum(路線 %in% cross))]
)[, `占比%` := round(100 * 跨縣列數 / 總列數, 2)][]
print(d5)
print(thb[路線 %in% cross, .(列數 = .N), by = 路線][order(-列數)])
rm(thb, hua_thb, ttt_thb); invisible(gc())

hua <- rd("花蓮縣公車.csv")[!路線 %in% hualien_abnormal_routes]
summarise_quality(hua, "花蓮市區"); rm(hua); invisible(gc())

ttt <- rd("臺東縣公車.csv")
ttt <- ttt[!(日期 %in% taitung_abnormal_dates) | 路線 %in% taitung_normal_routes]
summarise_quality(ttt, "臺東市區"); rm(ttt); invisible(gc())

cat("\n完成\n")
