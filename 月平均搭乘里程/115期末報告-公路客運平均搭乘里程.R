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


unique(data1$搭乘路線名稱)
# 看附屬路線名稱有哪些不同的值
sort(unique(data1$搭乘附屬路線名稱))
# 確認這三條路線在資料中是否有資料

# ── 台東路線定義 ──────────────────────────────────────────────
route_type_taitung <- list()
route_type_taitung$coast  <- c("1145",
                               "8101","8101A","8101B","8101C","8101D",
                               "8102","8103","8105","8107","8109",
                               "8119","8120","8122","8125")

route_type_taitung$valley <- c("8117",
                               "8161",
                               "8163","8163A","8163B",
                               "8165","8165A",
                               "8166","8166A",
                               "8167","8167A","8167B",
                               "8168","8168A","8168B",
                               "8170","8170A",
                               "8171","8171A","8171B",
                               "8172","8173","8178")

route_type_taitung$cross  <- c("8181")

route_type_taitung$south  <- c("8132","8135","8136","8137","8138",
                               "8150",
                               "8151","8151A",
                               "8152A",   # ← 注意！只有8152A，沒有8152
                               "8156","8157","8158")

route_type_taitung$zhiben <- c("8113","8115","8128",
                               "8129","8129A",
                               "8130","8130A",
                               "8131","8131A",
                               "8153")
route_map_taitung <- data.frame(
  搭乘附屬路線名稱 = as.character(c(
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

# ── 花蓮路線定義 ──────────────────────────────────────────────
route_type_hualien <- list()
route_type_hualien$coast  <- c("1129","11290",
                               "1132","1132A",
                               "1136","1140","1145",
                               "8101","8101A","8101B","8101C","8101D",
                               "8102","8105","8119")

route_type_hualien$valley <- c("1121","11210",
                               "1122","11220",
                               "1128","1130",
                               "1135","1135A",
                               "1137",
                               "1139","1139B","1139C",
                               "1142","11420",
                               "1143",
                               "8161","8173")

route_type_hualien$cross  <- c("1125","1133",
                               "1141","11410","1141A",
                               "8181")
route_map_hualien <- data.frame(
  搭乘附屬路線名稱 = as.character(c(
    route_type_hualien$coast,
    route_type_hualien$valley,
    route_type_hualien$cross
  )),
  路線類別 = c(
    rep("海岸線", length(route_type_hualien$coast)),
    rep("縱谷線", length(route_type_hualien$valley)),
    rep("山海線", length(route_type_hualien$cross))
  ),
  stringsAsFactors = FALSE
)

# ── 套用到資料 ────────────────────────────────────────────────
data1$搭乘附屬路線名稱 <- as.character(data1$搭乘附屬路線名稱)

#__清除外縣市資料

data_968 <- data1[data1$搭乘路線名稱 == 968, ]


# 再執行 join 就不會報錯
data_taitung <- data1 %>%
  inner_join(route_map_taitung, by = "搭乘附屬路線名稱")

data_hualien <- data1 %>%
  inner_join(route_map_hualien, by = "搭乘附屬路線名稱")

# 確認筆數
nrow(data_taitung)
nrow(data_hualien)
table(data_taitung$路線類別)
table(data_hualien$路線類別)


#____ 里程_________

# 看花蓮公路客運站間距離
head(data5)
str(data5)

# 看台東公路客運站間距離
head(data7)
str(data7)
# 合併兩份站間距離資料
bus_mileage_raw <- rbind(data5, data7)

# 確認合併後筆數
nrow(data5)          # 花蓮公路客運站間距離筆數
nrow(data7)          # 台東公路客運站間距離筆數
nrow(bus_mileage_raw)  # 應等於兩者相加

# ── Step 1：建立 bus_mileage_cum（對應原本的 bus_mileage_reduced）──
bus_mileage_raw <- rbind(data5, data7)   # 花蓮+台東公路客運站間距離

bus_mileage_cum <- bus_mileage_raw %>%
  group_by(搭乘路線名稱, 搭乘附屬路線名稱, 搭乘公車路線方向) %>%
  arrange(站序資料, .by_group = TRUE) %>%
  mutate(
    站間距離 = replace_na(站間距離, 0),
    累積里程 = cumsum(站間距離)
  ) %>%
  ungroup() %>%
  mutate(
    搭乘附屬路線名稱 = as.character(搭乘附屬路線名稱),
    站牌代碼     = as.character(站牌代碼)
  )


year.from = 2024
month.from = 6
year.to = 2026
month.to = 6
#___台東
# 原本用站牌代碼join（對不起來）
# 改成用站序join

df_mileage_taitung <- data_taitung %>%
  mutate(
    搭乘附屬路線名稱 = as.character(搭乘附屬路線名稱),
    資料日期     = as.Date(`資料代表日期(yyyy-MM-dd)`)
  ) %>%
  filter(
    資料日期 >= as.Date(paste0(year.from, "-", sprintf("%02d", month.from), "-01")),
    資料日期 <= as.Date(paste0(year.to, "-", sprintf("%02d", month.to), "-01")) + months(1) - days(1),
    !is.na(上車計費站序資料),
    !is.na(下車站牌站序)
  ) %>%
  mutate(
    年月 = paste0(year(資料日期) - 1911, sprintf("%02d", month(資料日期)))
  ) %>%
  # join上車累積里程
  left_join(
    bus_mileage_cum %>% select(搭乘附屬路線名稱, 搭乘公車路線方向, 站序資料, 累積里程) %>%
      rename(上車里程 = 累積里程),
    by = c("搭乘附屬路線名稱"     = "搭乘附屬路線名稱",
           "搭乘公車路線方向" = "搭乘公車路線方向",
           "上車計費站序資料" = "站序資料")
  ) %>%
  # join下車累積里程
  left_join(
    bus_mileage_cum %>% select(搭乘附屬路線名稱, 搭乘公車路線方向, 站序資料, 累積里程) %>%
      rename(下車里程 = 累積里程),
    by = c("搭乘附屬路線名稱"     = "搭乘附屬路線名稱",
           "搭乘公車路線方向" = "搭乘公車路線方向",
           "下車站牌站序"     = "站序資料")
  ) %>%
  mutate(
    上車里程 = as.numeric(上車里程),
    下車里程 = as.numeric(下車里程)
  )
#___花蓮
df_mileage_hualien <- data_hualien %>%
  mutate(
    搭乘附屬路線名稱 = as.character(搭乘附屬路線名稱),
    資料日期     = as.Date(`資料代表日期(yyyy-MM-dd)`)
  ) %>%
  filter(
    資料日期 >= as.Date(paste0(year.from, "-", sprintf("%02d", month.from), "-01")),
    資料日期 <= as.Date(paste0(year.to, "-", sprintf("%02d", month.to), "-01")) + months(1) - days(1),
    !is.na(上車計費站序資料),
    !is.na(下車站牌站序)
  ) %>%
  mutate(
    年月 = paste0(year(資料日期) - 1911, sprintf("%02d", month(資料日期)))
  ) %>%
  # join上車累積里程
  left_join(
    bus_mileage_cum %>% select(搭乘附屬路線名稱, 搭乘公車路線方向, 站序資料, 累積里程) %>%
      rename(上車里程 = 累積里程),
    by = c("搭乘附屬路線名稱"     = "搭乘附屬路線名稱",
           "搭乘公車路線方向" = "搭乘公車路線方向",
           "上車計費站序資料" = "站序資料")
  ) %>%
  # join下車累積里程
  left_join(
    bus_mileage_cum %>% select(搭乘附屬路線名稱, 搭乘公車路線方向, 站序資料, 累積里程) %>%
      rename(下車里程 = 累積里程),
    by = c("搭乘附屬路線名稱"     = "搭乘附屬路線名稱",
           "搭乘公車路線方向" = "搭乘公車路線方向",
           "下車站牌站序"     = "站序資料")
  ) %>%
  mutate(
    上車里程 = as.numeric(上車里程),
    下車里程 = as.numeric(下車里程)
  )

# 檢查上下車里程是否存在 NA 情況，如有則需修改對照表
# NA台東
up_na_ttt <- df_mileage_taitung %>%
  filter(is.na(上車里程)) %>%
  select(搭乘附屬路線名稱, 上車站牌代碼, 搭乘公車路線方向) %>%
  distinct() %>%
  rename(站牌代碼 = 上車站牌代碼)

# 下車里程為 NA 的站點
down_na_ttt <- df_mileage_taitung %>%
  filter(is.na(下車里程)) %>%
  select(搭乘附屬路線名稱, 下車站牌代碼, 搭乘公車路線方向) %>%
  distinct() %>%
  rename(站牌代碼 = 下車站牌代碼)

na_check_ttt <- bind_rows(up_na_ttt, down_na_ttt) %>%
  distinct() %>%
  arrange(搭乘附屬路線名稱, 搭乘公車路線方向)
print(na_check_ttt)   

# ── NA 檢查（花蓮）───────────────────────────────────────────
up_na_hua <- df_mileage_hualien %>%
  filter(is.na(上車里程)) %>%
  select(搭乘附屬路線名稱, 上車站牌代碼, 搭乘公車路線方向) %>%
  distinct() %>%
  rename(站牌代碼 = 上車站牌代碼)

down_na_hua <- df_mileage_hualien %>%
  filter(is.na(下車里程)) %>%
  select(搭乘附屬路線名稱, 下車站牌代碼, 搭乘公車路線方向) %>%
  distinct() %>%
  rename(站牌代碼 = 下車站牌代碼)

na_check_hua <- bind_rows(up_na_hua, down_na_hua) %>%
  distinct() %>%
  arrange(搭乘附屬路線名稱, 搭乘公車路線方向)
print(na_check_hua)


# ── 計算里程 + 過濾 NA ────────────────────────────────────────
df_mileage_taitung <- df_mileage_taitung %>%
  mutate(里程 = 下車里程 - 上車里程,
         里程 = ifelse(里程 > 0, 里程, NA_real_)) %>%
  filter(!is.na(里程))

df_mileage_hualien <- df_mileage_hualien %>%
  mutate(里程 = 下車里程 - 上車里程,
         里程 = ifelse(里程 > 0, 里程, NA_real_)) %>%
  filter(!is.na(里程))

# 確認筆數
nrow(df_mileage_taitung)
nrow(df_mileage_hualien)

# ── Step 5：月平均里程繪圖函數（對應原本的繪圖函數）─────────
intercity_bus_monthly_average_mileage_plot <- function(df, title_main) {
  color <- gray.colors(2)
  # 直接傳入資料和標題即可
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
  
  text(x      = 1:nrow(data),
       y      = data$平均里程_公里 + max(data$平均里程_公里) * 0.02,
       labels = round(data$平均里程_公里, 1),
       pos = 3, cex = 1, col = "black")
  
  legend("topright",
         legend = c("公里"),
         col    = color[1],
         lwd = 2, pch = 16, bty = "n",
         inset = c(-0.15, 0), xpd = TRUE, cex = 1.5)
}
nrow(df_mileage_taitung)
sum(is.na(df_mileage_taitung$上車里程))
sum(is.na(df_mileage_taitung$下車里程))
# ── Step 6：儲存圖片 ──────────────────────────────────────────
check_path <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

path <- "img/line_plot/"
check_path(path)

yr_label <- paste0(year.from - 1911, "年", month.from, "月至",
                   year.to   - 1911, "年", month.to,   "月")

# 台東 ✅
png(paste0(path, yr_label, "臺東縣公路客運平均搭乘里程折線圖.png"),
    width = 15, height = 5, units = "in", res = 300, family = "kai")
intercity_bus_monthly_average_mileage_plot(
  df_mileage_taitung,
  paste0(yr_label, "臺東縣公路客運平均搭乘里程折線圖"))
dev.off()

# 花蓮 ✅
png(paste0(path, yr_label, "花蓮縣公路客運平均搭乘里程折線圖.png"),
    width = 15, height = 5, units = "in", res = 300, family = "kai")
intercity_bus_monthly_average_mileage_plot(
  df_mileage_hualien,
  paste0(yr_label, "花蓮縣公路客運平均搭乘里程折線圖"))
dev.off()

# ── 台東月統計表 ──────────────────────────────────────────────
monthly_ttt <- df_mileage_taitung %>%
  group_by(年月) %>%
  summarise(
    搭乘人次    = n(),
    總里程_公尺 = sum(里程),
    平均里程_公里 = round(總里程_公尺 / 搭乘人次 / 1000, 2),
    .groups = "drop"
  ) %>%
  arrange(年月)

# ── 花蓮月統計表 ──────────────────────────────────────────────
monthly_hua <- df_mileage_hualien %>%
  group_by(年月) %>%
  summarise(
    搭乘人次      = n(),
    總里程_公尺   = sum(里程),
    平均里程_公里 = round(總里程_公尺 / 搭乘人次 / 1000, 2),
    .groups = "drop"
  ) %>%
  arrange(年月)

# ── 先看看結果 ────────────────────────────────────────────────
print(monthly_ttt)
print(monthly_hua)
wb <- createWorkbook()
addWorksheet(wb, "臺東公路客運月平均里程")
writeData(wb, sheet = "臺東公路客運月平均里程", x = monthly_ttt)
addWorksheet(wb, "花蓮公路客運月平均里程")
writeData(wb, sheet = "花蓮公路客運月平均里程", x = monthly_hua)

check_path("output_tables/")
saveWorkbook(wb,
             file = paste0("output_tables/", yr_label,
                           "花東公路客運月平均搭乘里程統計表.xlsx"),
             overwrite = TRUE)

message("✅ 統計表已儲存！")
