# ==============================================================================
# 專案名稱：花東地區市區客運平均搭乘里程分析
# 描述：計算並繪製花蓮縣與臺東縣市區客運之月平均搭乘里程折線圖及統計表
# ==============================================================================

# ==============================================================================
# 1. 載入所需套件
# ==============================================================================
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

# ==============================================================================
# 2. 全域變數與環境設定
# ==============================================================================
# 設定 Mac 繪圖字體，避免中文亂碼 (使用黑體-繁，貼近微軟正黑體視覺)
mac_font <- "Heiti TC" 
par(family = mac_font)

# 設定分析的年月範圍 (2024年6月至2026年6月)
year.from <- 2024
month.from <- 6
year.to <- 2026
month.to <- 6

# 定義圖表標題與檔名格式
title_time_label <- paste0(year.from - 1911, "年", month.from, "月至", year.to - 1911, "年", month.to, "月")
file_time_label <- paste0(year.from - 1911, sprintf("%02d", month.from), "-", year.to - 1911, sprintf("%02d", month.to))

# 設定輸出資料夾 (建立於桌面)
out_base_dir <- "/Users/chungemma/Desktop/115_Final-Report_Output"
img_out_dir <- file.path(out_base_dir, "images")
tbl_out_dir <- file.path(out_base_dir, "output_tables")

if(!dir.exists(img_out_dir)) dir.create(img_out_dir, recursive = TRUE)
if(!dir.exists(tbl_out_dir)) dir.create(tbl_out_dir, recursive = TRUE)

# ==============================================================================
# 3. 讀取原始資料
# ==============================================================================
# 花蓮縣刷卡與里程資料
data2 <- fread("/Users/chungemma/Library/Mobile Documents/com~apple~CloudDocs/東華大學/NDHU-Logistics-Management/115_Final-Report/市區客運平均搭乘里程/花蓮縣/花蓮縣公車.csv")
data6 <- fread("/Users/chungemma/Library/Mobile Documents/com~apple~CloudDocs/東華大學/NDHU-Logistics-Management/115_Final-Report/市區客運平均搭乘里程/花蓮縣/花蓮縣市區客運站間距離資料.csv")

# 臺東縣刷卡與里程資料
data3 <- fread("/Users/chungemma/Library/Mobile Documents/com~apple~CloudDocs/東華大學/NDHU-Logistics-Management/115_Final-Report/市區客運平均搭乘里程/台東縣/臺東縣公車.csv")
data8 <- fread("/Users/chungemma/Library/Mobile Documents/com~apple~CloudDocs/東華大學/NDHU-Logistics-Management/115_Final-Report/市區客運平均搭乘里程/台東縣/臺東縣市區客運站間距離資料.csv")

# ==============================================================================
# 4. 建立市區客運站牌「累積里程」對照表
# ==============================================================================
city_mileage_raw <- rbind(data6, data8)

city_mileage_cum <- city_mileage_raw %>%
  group_by(搭乘路線名稱, 搭乘附屬路線名稱, 搭乘公車路線方向) %>%
  arrange(站序資料, .by_group = TRUE) %>%
  mutate(
    站間距離 = replace_na(站間距離, 0),
    累積里程 = cumsum(站間距離)
  ) %>%
  ungroup() %>%
  mutate(
    搭乘路線名稱     = as.character(搭乘路線名稱),
    搭乘附屬路線名稱 = as.character(搭乘附屬路線名稱),
    站牌代碼         = as.character(站牌代碼)
  ) %>%
  # 去重：同一附屬路線、同一方向的同一站牌僅保留首筆紀錄
  group_by(搭乘附屬路線名稱, 搭乘公車路線方向, 站牌代碼) %>%
  slice(1) %>%
  ungroup()

# ==============================================================================
# 5. 定義共用函數：計算實際搭乘里程
# ==============================================================================
calculate_actual_mileage <- function(df_bus, df_dist, valid_routes, y_from, m_from, y_to, m_to) {
  start_date <- as.Date(paste0(y_from, "-", sprintf("%02d", m_from), "-01"))
  end_date <- as.Date(paste0(y_to, "-", sprintf("%02d", m_to), "-01")) %m+% months(1) - days(1)
  
  df_bus %>%
    mutate(搭乘路線名稱 = as.character(搭乘路線名稱)) %>%
    filter(搭乘路線名稱 %in% valid_routes) %>%
    mutate(
      搭乘附屬路線名稱 = as.character(搭乘附屬路線名稱),
      資料日期         = as.Date(`資料代表日期(yyyy-MM-dd)`),
      # 擷取站牌代碼中的純數字部分以利關聯
      上車站牌代碼     = as.character(as.numeric(gsub("[^0-9]", "", 上車站牌代碼))),
      下車站牌代碼     = as.character(as.numeric(gsub("[^0-9]", "", 下車站牌代碼)))
    ) %>%
    filter(
      資料日期 >= start_date,
      資料日期 <= end_date,
      !is.na(上車站牌代碼), 上車站牌代碼 != "",
      !is.na(下車站牌代碼), 下車站牌代碼 != ""
    ) %>%
    mutate(年月 = paste0(year(資料日期) - 1911, sprintf("%02d", month(資料日期)))) %>%
    # 合併上車與下車站的累積里程
    left_join(
      df_dist %>% select(搭乘附屬路線名稱, 搭乘公車路線方向, 站牌代碼, 累積里程) %>% rename(上車里程 = 累積里程),
      by = c("搭乘附屬路線名稱", "搭乘公車路線方向", "上車站牌代碼" = "站牌代碼")
    ) %>%
    left_join(
      df_dist %>% select(搭乘附屬路線名稱, 搭乘公車路線方向, 站牌代碼, 累積里程) %>% rename(下車里程 = 累積里程),
      by = c("搭乘附屬路線名稱", "搭乘公車路線方向", "下車站牌代碼" = "站牌代碼")
    ) %>%
    mutate(
      上車里程 = as.numeric(上車里程),
      下車里程 = as.numeric(下車里程),
      # 計算搭乘里程，並排除異常資料 (負數或零)
      里程 = 下車里程 - 上車里程,
      里程 = ifelse(里程 > 0, 里程, NA_real_)
    ) %>%
    filter(!is.na(里程))
}

# 執行計算：花蓮縣與臺東縣市區客運
valid_routes_hua <- as.character(unique(data6$搭乘路線名稱))
df_mileage_hua_city <- calculate_actual_mileage(data2, city_mileage_cum, valid_routes_hua, year.from, month.from, year.to, month.to)

valid_routes_ttt <- as.character(unique(data8$搭乘路線名稱))
df_mileage_ttt_city <- calculate_actual_mileage(data3, city_mileage_cum, valid_routes_ttt, year.from, month.from, year.to, month.to)

# ==============================================================================
# 6. 定義共用函數：繪圖與輸出
# ==============================================================================
city_bus_monthly_average_mileage_plot <- function(df, title_main, save_path) {
  line_color <- "#555555" 
  
  data <- df %>%
    group_by(年月) %>%
    summarise(
      人次        = n(),
      總里程_公尺 = sum(里程),
      .groups     = "drop"
    ) %>%
    mutate(平均里程_公里 = 總里程_公尺 / 人次 / 1000) %>%
    arrange(年月)
  
  # 啟動繪圖設備
  png(save_path, width = 15, height = 5, units = "in", res = 300, family = mac_font)
  par(family = mac_font, mar = c(5, 6, 4, 10))
  
  # 繪製主圖
  plot(x    = 1:nrow(data),
       y    = data$平均里程_公里,
       type = "o", lwd = 2, pch = 16, col = line_color,
       xlab = "月份", ylab = "",
       ylim = c(min(data$平均里程_公里) * 0.9, max(data$平均里程_公里) * 1.2),
       cex.main = 2, cex.lab = 2, cex.axis = 1.5, cex = 1.5,
       xaxt = "n", yaxt = "n", bty = "n")
  
  # 加入淺灰色虛線格線
  grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
  
  # 繪製標題與座標軸
  title(main = title_main, cex.main = 2.5, adj = 0)
  
  # X軸標籤每 3 個月顯示一次
  x_labels <- rep("", nrow(data))
  label_idx <- seq(1, nrow(data), by = 3) 
  x_labels[label_idx] <- data$年月[label_idx]
  axis(side = 1, at = 1:nrow(data), labels = x_labels, cex.axis = 1.5)
  
  # 繪製數值標籤
  text(x = 1:nrow(data), 
       y = data$平均里程_公里 + max(data$平均里程_公里) * 0.03,
       labels = round(data$平均里程_公里, 1), 
       pos = 3, cex = 1.1, col = "black")
  
  # 繪製圖例
  legend("topright", legend = c("公里"), col = line_color, lwd = 2, pch = 16, bty = "n", inset = c(-0.15, 0), xpd = TRUE, cex = 1.5)
  
  dev.off()
}

# ==============================================================================
# 7. 執行匯出作業
# ==============================================================================
# 匯出折線圖
city_bus_monthly_average_mileage_plot(
  df_mileage_ttt_city, 
  title_main = paste0(title_time_label, "臺東縣市區客運平均搭乘里程折線圖"),
  save_path  = file.path(img_out_dir, paste0(file_time_label, "臺東縣市區客運平均搭乘里程折線圖.png"))
)

city_bus_monthly_average_mileage_plot(
  df_mileage_hua_city, 
  title_main = paste0(title_time_label, "花蓮縣市區客運平均搭乘里程折線圖"),
  save_path  = file.path(img_out_dir, paste0(file_time_label, "花蓮縣市區客運平均搭乘里程折線圖.png"))
)

# 整理統計表 (對齊指定的 Excel 格式)
monthly_ttt_city <- df_mileage_ttt_city %>%
  group_by(年月) %>%
  summarise(
    搭乘人次 = n(),
    `平均里程(公里)` = round(sum(里程) / n() / 1000, 1),
    .groups = "drop"
  ) %>% 
  arrange(年月) %>%
  mutate(
    月份 = paste0(substr(年月, 1, 3), "年", as.numeric(substr(年月, 4, 5)), "月")
  ) %>%
  select(月份, 搭乘人次, `平均里程(公里)`)

monthly_hua_city <- df_mileage_hua_city %>%
  group_by(年月) %>%
  summarise(
    搭乘人次 = n(),
    `平均里程(公里)` = round(sum(里程) / n() / 1000, 1),
    .groups = "drop"
  ) %>% 
  arrange(年月) %>%
  mutate(
    月份 = paste0(substr(年月, 1, 3), "年", as.numeric(substr(年月, 4, 5)), "月")
  ) %>%
  select(月份, 搭乘人次, `平均里程(公里)`)

# 匯出 Excel (單一檔案，包含兩個分頁)
wb2 <- createWorkbook()

# 建立千分位與小數點格式樣式，讓 Excel 輸出更美觀
comma_style <- createStyle(numFmt = "#,##0")
float_style <- createStyle(numFmt = "0.0")

# 加入臺東分頁並套用格式
addWorksheet(wb2, "臺東市區客運月平均里程")
writeData(wb2, sheet = "臺東市區客運月平均里程", x = monthly_ttt_city)
addStyle(wb2, "臺東市區客運月平均里程", style = comma_style, cols = 2, rows = 2:(nrow(monthly_ttt_city)+1), gridExpand = TRUE)
addStyle(wb2, "臺東市區客運月平均里程", style = float_style, cols = 3, rows = 2:(nrow(monthly_ttt_city)+1), gridExpand = TRUE)

# 加入花蓮分頁並套用格式
addWorksheet(wb2, "花蓮市區客運月平均里程")
writeData(wb2, sheet = "花蓮市區客運月平均里程", x = monthly_hua_city)
addStyle(wb2, "花蓮市區客運月平均里程", style = comma_style, cols = 2, rows = 2:(nrow(monthly_hua_city)+1), gridExpand = TRUE)
addStyle(wb2, "花蓮市區客運月平均里程", style = float_style, cols = 3, rows = 2:(nrow(monthly_hua_city)+1), gridExpand = TRUE)

# 儲存檔案
saveWorkbook(wb2, file = file.path(tbl_out_dir, paste0(file_time_label, "花東市區客運月平均搭乘里程統計表.xlsx")), overwrite = TRUE)

message("✅ 分析完成！圖表與 Excel 檔案已儲存至桌面 115_Final-Report_Output 資料夾中")