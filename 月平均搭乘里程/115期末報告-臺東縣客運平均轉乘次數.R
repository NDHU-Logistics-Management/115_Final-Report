# library
install.packages("openxlsx")
install.packages("pbapply")
library(readr)
library(data.table)
library(dplyr)
library(lubridate)
library(tidyr)
library(purrr)
library(ggplot2)
library(readxl)

library(pbapply)
library(stringr)

library(openxlsx)
library(hms)



windowsFonts(kai = windowsFont("Microsoft JhengHei"))
setwd("C:/Users/gr704/OneDrive/桌面/運籌計畫")
# 讀入資料
data1 <- fread("C:/Users/gr704/OneDrive/桌面/115_Final-Report/data/公路客運2024_to_202606.csv")
data2 <- fread("C:/Users/gr704/OneDrive/桌面/115_Final-Report/data/花蓮縣公車.csv")
data3 <- fread("C:/Users/gr704/OneDrive/桌面/115_Final-Report/data/臺東縣公車.csv")

data5 <- fread("C:/Users/gr704/OneDrive/桌面/115_Final-Report/data/公車站間距離資料/花蓮縣公路客運站間距離資料.csv")
data6 <- fread("C:/Users/gr704/OneDrive/桌面/115_Final-Report/data/公車站間距離資料/花蓮縣市區客運站間距離資料.csv")
data7 <- fread("C:/Users/gr704/OneDrive/桌面/115_Final-Report/data/公車站間距離資料/臺東縣公路客運站間距離資料.csv")
data8 <- fread("C:/Users/gr704/OneDrive/桌面/115_Final-Report/data/公車站間距離資料/臺東縣市區客運站間距離資料.csv")


# ── 台東路線定義 ──────────────────────────────────────────────
route_type_taitung <- list()
route_type_taitung$coast  <- c(1145, 8101, 8102, 8103, 8105, 8107, 8109, 8119, 8120, 8122, 8125)
route_type_taitung$valley <- c(8117, 8161, 8163, 8165, 8166, 8167, 8168, 8170, 8171, 8172, 8173, 8178)
route_type_taitung$cross  <- c(8181)          
route_type_taitung$south  <- c(8132, 8135, 8136, 8137, 8138, 8150, 8151, 8152, 8156, 8157, 8158)
route_type_taitung$zhiben <- c(8113, 8115, 8128, 8129, 8130, 8131, 8153)

route_map_taitung <- data.frame(
  搭乘路線名稱 = as.character(c(
    route_type_taitung$coast,
    route_type_taitung$valley,
    route_type_taitung$cross,
    route_type_taitung$south,
    route_type_taitung$zhiben
  )),
  路線類別 = c(
    rep("海岸線", length(route_type_taitung$coast)),
    rep("縱谷線", length(route_type_taitung$valley)),
    rep("山海線", length(route_type_taitung$cross)),
    rep("南迴線", length(route_type_taitung$south)),
    rep("知本線", length(route_type_taitung$zhiben))
  ),
  stringsAsFactors = FALSE
)
# ── 台東市區：只保留 data8 有的路線 ──────────────────────────
valid_routes_ttt_city <- as.character(unique(data8$搭乘路線名稱))
# "101", "201", "202", "203", "市區循環", "陸海空線"

data_ttt_city <- data3 %>%
  filter(搭乘路線名稱 %in% valid_routes_ttt_city)

#  過濾台東公路客運
data1$搭乘路線名稱 <- as.character(data1$搭乘路線名稱)
data_taitung <- data1 %>%
  inner_join(route_map_taitung, by = "搭乘路線名稱")


# 合併台東公路+市區
data_taitung_all <- bind_rows(
  data_taitung %>% mutate(客運類型 = "公路客運"),
  data_ttt_city %>% mutate(客運類型 = "市區客運")
)

# 確認合併後筆數
nrow(data_taitung_all)
# 應等於
nrow(data_taitung) + nrow(data_ttt_city)

# 確認時間欄位格式正確
head(data_taitung_all$刷卡上車時間)
head(data_taitung_all$刷卡下車時間)

year.from  <- 2024
month.from <- 6
year.to    <- 2026
month.to   <- 6

data_transfer <- data_taitung_all %>%
  mutate(
    資料日期 = as.Date(`資料代表日期(yyyy-MM-dd)`),
    上車時間 = as.POSIXct(刷卡上車時間),
    年月     = paste0(year(資料日期) - 1911,
                    sprintf("%02d", month(資料日期))),
    票種分類 = ifelse(票種類型 == 4, "TPASS", "其他票種")  
  ) %>%
  filter(
    資料日期 >= as.Date(paste0(year.from, "-", sprintf("%02d", month.from), "-01")),
    資料日期 <= as.Date(paste0(year.to,   "-", sprintf("%02d", month.to),   "-01")) +
      months(1) - days(1),
    !is.na(卡號),
    !is.na(上車時間)
  ) %>%
  select(卡號, 業者編號, 資料日期, 上車時間, 年月, 票種分類)  

# ── 判斷轉乘 ──────────────────────────────────────────────────
day_data <- data_transfer %>%
  group_by(資料日期, 卡號, 業者編號) %>%
  arrange(上車時間, .by_group = TRUE) %>%
  mutate(
    前一筆上車時間 = lag(上車時間),
    間隔分鐘       = as.numeric(difftime(上車時間, 前一筆上車時間, units = "mins")),
    是否轉乘       = if_else(!is.na(間隔分鐘) & 間隔分鐘 <= 120, 1, 0)
  ) %>%
  summarise(
    轉乘次數       = sum(是否轉乘),
    TPASS轉乘次數  = sum(是否轉乘 == 1 & 票種分類 == "TPASS"),  
    .groups = "drop"
  ) %>%
  mutate(其他票種轉乘次數 = 轉乘次數 - TPASS轉乘次數)  

# ── 月總人次 ──────────────────────────────────────────────────
population_data <- data_transfer %>%
  group_by(年月) %>%
  summarise(
    人次          = n(),
    TPASS人次     = sum(票種分類 == "TPASS", na.rm = TRUE),  
    .groups = "drop"
  ) %>%
  mutate(其他票種人次 = 人次 - TPASS人次)  
# ── 月轉乘次數 ────────────────────────────────────────────────
month_data <- day_data %>%
  mutate(年月 = paste0(year(資料日期) - 1911,
                     sprintf("%02d", month(資料日期)))) %>%
  group_by(年月) %>%
  summarise(
    月轉乘次數      = sum(轉乘次數),
    TPASS月轉乘次數 = sum(TPASS轉乘次數),  
    .groups = "drop"
  ) %>%
  mutate(其他票種月轉乘次數 = 月轉乘次數 - TPASS月轉乘次數)  

# ── 計算平均轉乘次數 ──────────────────────────────────────────
result <- full_join(population_data, month_data, by = "年月") %>%
  mutate(
    平均轉乘次數             = 月轉乘次數 / 人次,
    平均轉乘次數百分比       = sprintf("%.2f%%", 平均轉乘次數 * 100),
    TPASS平均轉乘次數        = TPASS月轉乘次數 / TPASS人次,           
    TPASS平均轉乘次數百分比  = sprintf("%.2f%%", TPASS平均轉乘次數 * 100),  
    其他票種平均轉乘次數     = 其他票種月轉乘次數 / 其他票種人次,    
    其他票種平均轉乘次數百分比 = sprintf("%.2f%%", 其他票種平均轉乘次數 * 100)  
  ) %>%
  arrange(年月)

print(result)

# ── 繪圖函數 ──────────────────────────────────────────────────
bus_average_transfer_plot <- function(result, title_main) {
  color <- gray.colors(3)  
  
  par(family = "kai", mar = c(5, 6, 4, 10))
  
  # 計算 ylim 包含三條線
  all_vals <- c(result$平均轉乘次數,
                result$TPASS平均轉乘次數,
                result$其他票種平均轉乘次數)
  
  plot(x    = 1:nrow(result),
       y    = result$平均轉乘次數,
       type = "o", lwd = 2, pch = 16,
       col  = color[1],
       xlab = "月份", ylab = "",
       ylim = c(min(all_vals, na.rm = TRUE) * 0.9,
                max(all_vals, na.rm = TRUE) * 1.2),
       cex.main = 2, cex.lab = 2, cex.axis = 1.5, cex = 1.5,
       xaxt = "n", yaxt = "n", bty = "n")
  
  # TPASS 折線
  lines(x = 1:nrow(result), y = result$TPASS平均轉乘次數,
        type = "o", lwd = 2, pch = 16, cex = 1.5, col = color[2])
  
  # 其他票種折線
  lines(x = 1:nrow(result), y = result$其他票種平均轉乘次數,
        type = "o", lwd = 2, pch = 16, cex = 1.5, col = color[3])
  
  title(main = title_main, cex.main = 2, adj = 0)
  
  axis(side = 1, at = 1:nrow(result),
       labels = result$年月, cex.axis = 1.5)
  axis(side = 2, las = 1, cex.axis = 1.5, line = -1.5)
  
  grid()
  
  # 圖例
  legend("topright",
         legend = c("平均轉乘次數", "TPASS", "其他票種"),
         col    = color[1:3],
         lwd = 2, pch = 16, bty = "n",
         inset = c(-0.15, 0), xpd = TRUE, cex = 1.5)
}
# ── 定義 check_path 函數 ──────────────────────────────────────
check_path <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}
# ── 儲存圖片 ──────────────────────────────────────────────────
yr_label <- paste0(year.from - 1911, "年", month.from, "月至",
                   year.to   - 1911, "年", month.to,   "月")
path <- "img/line_plot/"
check_path(path)

png(paste0(path, yr_label, "臺東縣客運平均轉乘次數折線圖.png"),
    width = 15, height = 5, units = "in", res = 300, family = "kai")
bus_average_transfer_plot(
  result,
  paste0(yr_label, "臺東縣客運平均轉乘次數折線圖"))
dev.off()

# ── 存 Excel ──────────────────────────────────────────────────
wb <- createWorkbook()
addWorksheet(wb, "平均轉乘次數")
addWorksheet(wb, "日與卡號轉乘次數")
writeData(wb, sheet = "平均轉乘次數",     x = result)
writeData(wb, sheet = "日與卡號轉乘次數", x = day_data)

check_path("output_tables/")
saveWorkbook(wb,
             file = paste0("output_tables/", yr_label,
                           "臺東縣客運平均轉乘次數統計表.xlsx"),
             overwrite = TRUE)

message("✅ 轉乘分析完成！")
