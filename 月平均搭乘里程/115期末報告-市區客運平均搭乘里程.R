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


unique(data2$搭乘路線名稱)
unique(data3$搭乘路線名稱)
unique(data6$搭乘路線名稱)
unique(data8$搭乘路線名稱)

# ── 台東市區：只保留 data8 有的路線 ──────────────────────────
valid_routes_ttt_city <- as.character(unique(data8$搭乘路線名稱))
# "101", "201", "202", "203", "市區循環", "陸海空線"

data_ttt_city <- data3 %>%
  filter(搭乘路線名稱 %in% valid_routes_ttt_city)

nrow(data_ttt_city)   # 確認過濾後筆數

# ── 花蓮市區：只保留 data6 有的路線 ──────────────────────────
valid_routes_hua_city <- as.character(unique(data6$搭乘路線名稱))
# "301", "302", "303", "305", "307", "308", "309", "310", "311"

data_hua_city <- data2 %>%
  mutate(搭乘路線名稱 = as.character(搭乘路線名稱)) %>%
  filter(搭乘路線名稱 %in% valid_routes_hua_city)

nrow(data_hua_city)   # 確認過濾後筆數
# 看data3的業者編號
unique(data3$業者編號)

# 看上下車站牌名稱
head(data3[, c("上車站牌名稱", "下車站牌名稱", "搭乘路線名稱")], 10)
# ── 建立市區客運站牌累積里程對照表 ───────────────────────────
# 合併花蓮+台東市區站間距離（對應公路客運的 bus_mileage_raw）
city_mileage_raw <- rbind(data6, data8)

# 確認筆數
nrow(data6)            # 花蓮市區站間距離
nrow(data8)            # 台東市區站間距離
nrow(city_mileage_raw) # 應等於兩者相加

# 計算累積里程
city_mileage_cum <- city_mileage_raw %>%
  group_by(搭乘路線名稱, 搭乘附屬路線名稱, 搭乘公車路線方向) %>%
  arrange(站序資料, .by_group = TRUE) %>%
  mutate(
    站間距離 = replace_na(站間距離, 0),
    累積里程 = cumsum(站間距離)
  ) %>%
  ungroup() %>%
  mutate(
    搭乘路線名稱 = as.character(搭乘路線名稱),
    站牌代碼     = as.character(站牌代碼)
  )

# 確認結果
head(city_mileage_cum)
nrow(city_mileage_cum)

# ── 台東市區客運里程計算 ──────────────────────────────────────
df_mileage_ttt_city <- data_ttt_city %>%
  mutate(
    搭乘路線名稱 = as.character(搭乘路線名稱),
    資料日期     = as.Date(`資料代表日期(yyyy-MM-dd)`)
  ) %>%
  filter(
    資料日期 >= as.Date(paste0(year.from, "-", sprintf("%02d", month.from), "-01")),
    資料日期 <= as.Date(paste0(year.to, "-", sprintf("%02d", month.to), "-01")) +
      months(1) - days(1),
    !is.na(上車計費站序資料),
    !is.na(下車站牌站序)
  ) %>%
  mutate(年月 = paste0(year(資料日期) - 1911, sprintf("%02d", month(資料日期)))) %>%
  # join 上車累積里程
  left_join(
    city_mileage_cum %>%
      select(搭乘路線名稱, 搭乘公車路線方向, 站序資料, 累積里程) %>%
      rename(上車里程 = 累積里程),
    by = c("搭乘路線名稱"     = "搭乘路線名稱",
           "搭乘公車路線方向" = "搭乘公車路線方向",
           "上車計費站序資料" = "站序資料")
  ) %>%
  # join 下車累積里程
  left_join(
    city_mileage_cum %>%
      select(搭乘路線名稱, 搭乘公車路線方向, 站序資料, 累積里程) %>%
      rename(下車里程 = 累積里程),
    by = c("搭乘路線名稱"     = "搭乘路線名稱",
           "搭乘公車路線方向" = "搭乘公車路線方向",
           "下車站牌站序"     = "站序資料")
  ) %>%
  mutate(
    上車里程 = as.numeric(上車里程),
    下車里程 = as.numeric(下車里程),
    里程      = 下車里程 - 上車里程,
    里程      = ifelse(里程 > 0, 里程, NA_real_)
  ) %>%
  filter(!is.na(里程))

# ── 花蓮市區客運里程計算 ──────────────────────────────────────
df_mileage_hua_city <- data_hua_city %>%
  mutate(
    搭乘路線名稱 = as.character(搭乘路線名稱),
    資料日期     = as.Date(`資料代表日期(yyyy-MM-dd)`)
  ) %>%
  filter(
    資料日期 >= as.Date(paste0(year.from, "-", sprintf("%02d", month.from), "-01")),
    資料日期 <= as.Date(paste0(year.to, "-", sprintf("%02d", month.to), "-01")) +
      months(1) - days(1),
    !is.na(上車計費站序資料),
    !is.na(下車站牌站序)
  ) %>%
  mutate(年月 = paste0(year(資料日期) - 1911, sprintf("%02d", month(資料日期)))) %>%
  left_join(
    city_mileage_cum %>%
      select(搭乘路線名稱, 搭乘公車路線方向, 站序資料, 累積里程) %>%
      rename(上車里程 = 累積里程),
    by = c("搭乘路線名稱"     = "搭乘路線名稱",
           "搭乘公車路線方向" = "搭乘公車路線方向",
           "上車計費站序資料" = "站序資料")
  ) %>%
  left_join(
    city_mileage_cum %>%
      select(搭乘路線名稱, 搭乘公車路線方向, 站序資料, 累積里程) %>%
      rename(下車里程 = 累積里程),
    by = c("搭乘路線名稱"     = "搭乘路線名稱",
           "搭乘公車路線方向" = "搭乘公車路線方向",
           "下車站牌站序"     = "站序資料")
  ) %>%
  mutate(
    上車里程 = as.numeric(上車里程),
    下車里程 = as.numeric(下車里程),
    里程      = 下車里程 - 上車里程,
    里程      = ifelse(里程 > 0, 里程, NA_real_)
  ) %>%
  filter(!is.na(里程))

# ── 確認筆數 ──────────────────────────────────────────────────
nrow(df_mileage_ttt_city)
nrow(df_mileage_hua_city)

# ── 繪圖儲存 ──────────────────────────────────────────────────
## 市區客運繪圖函數
city_bus_monthly_average_mileage_plot <- function(df, title_main) {
  color <- gray.colors(2)
  data <- df %>%
    select(年月, 里程) %>%
    group_by(年月) %>%
    summarise(
      人次        = n(),
      總里程_公尺 = sum(里程),
      .groups     = "drop"
    ) %>%
    mutate(平均里程_公里 = 總里程_公尺 / 人次 / 1000) %>%
    arrange(年月)
  
  par(family = "kai", mar = c(5, 6, 4, 10))
  
  plot(x    = 1:nrow(data),
       y    = data$平均里程_公里,
       type = "o", lwd = 2, pch = 16,
       col  = color[1],
       xlab = "月份", ylab = "",
       ylim = c(min(data$平均里程_公里) * 0.9,
                max(data$平均里程_公里) * 1.2),
       cex.main = 2, cex.lab = 2, cex.axis = 1.5, cex = 1.5,
       xaxt = "n", yaxt = "n", bty = "n")
  
  title(main = title_main, cex.main = 2, adj = 0)
  
  axis(side = 1, at = 1:nrow(data),
       labels = data$年月, cex.axis = 1.5)
  
  grid()
  
  # 奇數月份標在上面，偶數月份標在下面
  offset <- ifelse(seq_along(data$平均里程_公里) %% 2 ==0 , 
                   max(data$平均里程_公里) * 0.07,
                   -max(data$平均里程_公里) * 0.07)
  
  text(x      = 1:nrow(data),
       y      = data$平均里程_公里 + offset,
       labels = round(data$平均里程_公里, 1),
       cex = 1.2, col = "black")
  
  legend("topright",
         legend = c("公里"),
         col    = color[1],
         lwd = 2, pch = 16, bty = "n",
         inset = c(-0.15, 0), xpd = TRUE, cex = 1.5)
}

# 台東市區
png(paste0(path, yr_label, "臺東縣市區客運平均搭乘里程折線圖.png"),
    width = 15, height = 5, units = "in", res = 300, family = "kai")
city_bus_monthly_average_mileage_plot(
  df_mileage_ttt_city,
  paste0(yr_label, "臺東縣市區客運平均搭乘里程折線圖"))
dev.off()

# 花蓮市區
png(paste0(path, yr_label, "花蓮縣市區客運平均搭乘里程折線圖.png"),
    width = 15, height = 5, units = "in", res = 300, family = "kai")
city_bus_monthly_average_mileage_plot(
  df_mileage_hua_city,
  paste0(yr_label, "花蓮縣市區客運平均搭乘里程折線圖"))
dev.off()

# ── 統計表存 Excel ────────────────────────────────────────────
monthly_ttt_city <- df_mileage_ttt_city %>%
  group_by(年月) %>%
  summarise(
    搭乘人次      = n(),
    總里程_公尺   = sum(里程),
    平均里程_公里 = round(總里程_公尺 / 搭乘人次 / 1000, 2),
    .groups = "drop"
  ) %>% arrange(年月)

monthly_hua_city <- df_mileage_hua_city %>%
  group_by(年月) %>%
  summarise(
    搭乘人次      = n(),
    總里程_公尺   = sum(里程),
    平均里程_公里 = round(總里程_公尺 / 搭乘人次 / 1000, 2),
    .groups = "drop"
  ) %>% arrange(年月)

wb2 <- createWorkbook()
addWorksheet(wb2, "臺東市區客運平均里程")
writeData(wb2, sheet = "臺東市區客運平均里程", x = monthly_ttt_city)
addWorksheet(wb2, "花蓮市區客運平均里程")
writeData(wb2, sheet = "花蓮市區客運平均里程", x = monthly_hua_city)

saveWorkbook(wb2,
             file = paste0("output_tables/", yr_label,
                           "花東市區客運平均搭乘里程統計表.xlsx"),
             overwrite = TRUE)

message("✅ 市區客運里程完成！")
