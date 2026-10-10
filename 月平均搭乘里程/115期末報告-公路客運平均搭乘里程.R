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



# ── 台東路線定義（不需要修改）────────────────────────────────
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
                               "8152A",
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

# ── 花蓮路線定義（刪除 8101, 8101A~D, 8102, 8105）────────────
route_type_hualien <- list()
route_type_hualien$coast  <- c("1129","11290",
                               "1132","1132A",
                               "1136","1140","1145",
                               "8119")              # ← 刪掉 8101系列, 8102, 8105

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
data1$搭乘路線名稱 <- as.character(data1$搭乘路線名稱)
data1$搭乘附屬路線名稱 <- as.character(data1$搭乘附屬路線名稱)
data1$搭乘公車路線方向 <- as.character(data1$搭乘公車路線方向)

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
# 說明：去除THB（台東）和HUA（花蓮）前綴，統一成純數字格式
normalize_stop_code <- function(x) {
  x <- trimws(toupper(as.character(x)))
  x <- sub("^THB", "", x)   # 去台東前綴
  x <- sub("^HUA", "", x)   # 去花蓮前綴
  x[x %in% c("", "NA", "NULL")] <- NA_character_
  return(x)
}
# 花蓮專用累積里程（只用data5）
bus_mileage_cum_hua <- data5 %>%
  group_by(搭乘路線名稱, 搭乘附屬路線名稱, 搭乘公車路線方向) %>%
  arrange(站序資料, .by_group = TRUE) %>%
  mutate(
    站間距離         = replace_na(站間距離, 0),
    累積里程         = cumsum(站間距離),          # ← 加這行
    搭乘路線名稱     = as.character(搭乘路線名稱),
    搭乘附屬路線名稱 = as.character(搭乘附屬路線名稱),
    搭乘公車路線方向 = as.character(搭乘公車路線方向),
    站牌代碼         = normalize_stop_code(站牌代碼)
  ) %>%
  ungroup() %>%
  
  group_by(搭乘路線名稱, 搭乘附屬路線名稱, 搭乘公車路線方向, 站牌代碼) %>%
  slice(1) %>%
  ungroup()

# 台東專用累積里程（只用data7）
bus_mileage_cum_ttt <- data7 %>%
  group_by(搭乘路線名稱, 搭乘附屬路線名稱, 搭乘公車路線方向) %>%
  arrange(站序資料, .by_group = TRUE) %>%
  mutate(
    站間距離         = replace_na(站間距離, 0),
    累積里程         = cumsum(站間距離),          # ← 加這行
    搭乘路線名稱     = as.character(搭乘路線名稱),
    搭乘附屬路線名稱 = as.character(搭乘附屬路線名稱),
    搭乘公車路線方向 = as.character(搭乘公車路線方向),
    站牌代碼         = normalize_stop_code(站牌代碼)
  ) %>%
  ungroup() %>%
  
  group_by(搭乘路線名稱, 搭乘附屬路線名稱, 搭乘公車路線方向, 站牌代碼) %>%
  slice(1) %>%
  ungroup()

nrow(bus_mileage_cum_hua)
nrow(bus_mileage_cum_ttt)

# 在join之前統一格式
data_taitung$搭乘公車路線方向 <- as.character(data_taitung$搭乘公車路線方向)
data_hualien$搭乘公車路線方向 <- as.character(data_hualien$搭乘公車路線方向)    
bus_mileage_cum_ttt$搭乘公車路線方向 <- as.character(bus_mileage_cum_ttt$搭乘公車路線方向)
bus_mileage_cum_hua$搭乘公車路線方向 <- as.character(bus_mileage_cum_hua$搭乘公車路線方向)

year.from = 2024
month.from = 6
year.to = 2026
month.to = 6
# ── 台東里程計算 ──────────────────────────────────────────
# 說明：篩時間範圍 → 標準化站牌代碼 → join累積里程 → 計算里程差
df_mileage_taitung <- data_taitung %>%
  mutate(
    搭乘路線名稱     = as.character(搭乘路線名稱),
    搭乘附屬路線名稱 = as.character(搭乘附屬路線名稱),
    資料日期         = as.Date(`資料代表日期(yyyy-MM-dd)`),
    上車站牌代碼     = normalize_stop_code(上車站牌代碼),
    下車站牌代碼     = normalize_stop_code(下車站牌代碼)
  ) %>%
  filter(
    資料日期 >= as.Date(paste0(year.from, "-", sprintf("%02d", month.from), "-01")),
    資料日期 <= as.Date(paste0(year.to, "-", sprintf("%02d", month.to), "-01")) +
      months(1) - days(1),
    !is.na(上車站牌代碼), 上車站牌代碼 != "",
    !is.na(下車站牌代碼), 下車站牌代碼 != ""
  ) %>%
  mutate(年月 = paste0(year(資料日期) - 1911, "/", sprintf("%02d", month(資料日期)))) %>%
  left_join(
    bus_mileage_cum_ttt %>%
      select(搭乘路線名稱, 搭乘附屬路線名稱, 搭乘公車路線方向, 站牌代碼, 累積里程) %>%
      rename(上車里程 = 累積里程),
    by = c("搭乘路線名稱"     = "搭乘路線名稱",
           "搭乘附屬路線名稱" = "搭乘附屬路線名稱",
           "搭乘公車路線方向" = "搭乘公車路線方向",
           "上車站牌代碼"     = "站牌代碼")
  ) %>%
  left_join(
    bus_mileage_cum_ttt %>%
      select(搭乘路線名稱, 搭乘附屬路線名稱, 搭乘公車路線方向, 站牌代碼, 累積里程) %>%
      rename(下車里程 = 累積里程),
    by = c("搭乘路線名稱"     = "搭乘路線名稱",
           "搭乘附屬路線名稱" = "搭乘附屬路線名稱",
           "搭乘公車路線方向" = "搭乘公車路線方向",
           "下車站牌代碼"     = "站牌代碼")
  ) %>%
  mutate(
    上車里程 = as.numeric(上車里程),
    下車里程 = as.numeric(下車里程),
    里程      = 下車里程 - 上車里程,
    里程      = ifelse(里程 > 0, 里程, NA_real_)
  ) %>%
  filter(!is.na(里程))

# ──  花蓮里程計算 ──────────────────────────────────────────
# 說明：邏輯同台東，但改用bus_mileage_cum_hua
df_mileage_hualien <- data_hualien %>%
  mutate(
    搭乘路線名稱     = as.character(搭乘路線名稱),
    搭乘附屬路線名稱 = as.character(搭乘附屬路線名稱),
    資料日期         = as.Date(`資料代表日期(yyyy-MM-dd)`),
    上車站牌代碼     = normalize_stop_code(上車站牌代碼),
    下車站牌代碼     = normalize_stop_code(下車站牌代碼)
  ) %>%
  filter(
    資料日期 >= as.Date(paste0(year.from, "-", sprintf("%02d", month.from), "-01")),
    資料日期 <= as.Date(paste0(year.to, "-", sprintf("%02d", month.to), "-01")) +
      months(1) - days(1),
    !is.na(上車站牌代碼), 上車站牌代碼 != "",
    !is.na(下車站牌代碼), 下車站牌代碼 != ""
  ) %>%
  mutate(年月 = paste0(year(資料日期) - 1911, "/", sprintf("%02d", month(資料日期)))) %>%
  left_join(
    bus_mileage_cum_hua %>%
      select(搭乘路線名稱, 搭乘附屬路線名稱, 搭乘公車路線方向, 站牌代碼, 累積里程) %>%
      rename(上車里程 = 累積里程),
    by = c("搭乘路線名稱"     = "搭乘路線名稱",
           "搭乘附屬路線名稱" = "搭乘附屬路線名稱",
           "搭乘公車路線方向" = "搭乘公車路線方向",
           "上車站牌代碼"     = "站牌代碼")
  ) %>%
  left_join(
    bus_mileage_cum_hua %>%
      select(搭乘路線名稱, 搭乘附屬路線名稱, 搭乘公車路線方向, 站牌代碼, 累積里程) %>%
      rename(下車里程 = 累積里程),
    by = c("搭乘路線名稱"     = "搭乘路線名稱",
           "搭乘附屬路線名稱" = "搭乘附屬路線名稱",
           "搭乘公車路線方向" = "搭乘公車路線方向",
           "下車站牌代碼"     = "站牌代碼")
  ) %>%
  mutate(
    上車里程 = as.numeric(上車里程),
    下車里程 = as.numeric(下車里程),
    里程      = 下車里程 - 上車里程,
    里程      = ifelse(里程 > 0, 里程, NA_real_)
  ) %>%
  filter(!is.na(里程))
# ── 確認結果 ──────────────────────────────────────────────
nrow(df_mileage_taitung)
nrow(df_mileage_hualien)

df_mileage_taitung %>%
  group_by(路線類別) %>%
  summarise(平均里程公里 = mean(里程) / 1000) %>%
  arrange(desc(平均里程公里))

df_mileage_hualien %>%
  group_by(路線類別) %>%
  summarise(平均里程公里 = mean(里程) / 1000) %>%
  arrange(desc(平均里程公里))

# ── Step 5：月平均里程繪圖函數（對應原本的繪圖函數）─────────
intercity_bus_monthly_average_mileage_plot <- function(df, title_main, missing_months = c()) {
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
  
  # ← 新增pch_list
  pch_list <- rep(16, nrow(data))
  if (length(missing_months) > 0) {
    flag_idx <- which(data$年月 %in% missing_months)
    pch_list[flag_idx] <- 2
  }
  
  par(family = "kai", mar = c(7, 8, 4, 10))
  plot(x    = 1:nrow(data),
       y    = data$平均里程_公里,
       type = "o", lwd = 2, pch = pch_list,  # ← 改這裡
       col  = color[1],
       xlab = "月份", ylab = "",
       ylim = c(20, 37),
       cex.main = 2, cex.lab = 1.5, cex.axis = 1.2, cex = 1.2,
       xaxt = "n", yaxt = "n", bty = "n")
  title(main = title_main, cex.main = 2, adj = 0)
  axis(side = 2, at = seq(20, 35, by = 5),
       labels = seq(20, 35, by = 5), las = 1, cex.axis = 1.2)
  x_pos <- seq(1, nrow(data), by = 2)
  axis(side = 1, at = x_pos,
       labels = data$年月[x_pos], cex.axis = 1.2, las = 1)
  grid()
  text(x      = 1:nrow(data),
       y      = data$平均里程_公里 + max(data$平均里程_公里) * 0.02,
       labels = round(data$平均里程_公里, 1),
       pos = 3, cex = 0.9, col = "black")
  legend("topright",
         legend = c("公里"),
         col    = color[1],
         lwd    = 2, pch = 16, bty = "n",
         inset  = c(-0.15, 0), xpd = TRUE, cex = 1.2)
}
# ── Step 6：儲存圖片 ──────────────────────────────────────────
check_path <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

path <- "img/line_plot/"
check_path(path)

yr_label <- paste0(year.from - 1911, "年", month.from, "月至",
                   year.to   - 1911, "年", month.to,   "月")

# 台東公路（114/11 東台灣客運缺資料）
png(paste0(path, yr_label, "臺東縣公路客運平均搭乘里程折線圖.png"),
    width = 15, height = 5, units = "in", res = 300, family = "kai")
intercity_bus_monthly_average_mileage_plot(
  df_mileage_taitung,
  paste0(yr_label, "臺東縣公路客運平均搭乘里程折線圖"),
  missing_months = c("114/11"))   # ← 加這個
dev.off()

# 花蓮公路（沒有公路缺資料，不需要△）
png(paste0(path, yr_label, "花蓮縣公路客運平均搭乘里程折線圖.png"),
    width = 15, height = 5, units = "in", res = 300, family = "kai")
intercity_bus_monthly_average_mileage_plot(
  df_mileage_hualien,
  paste0(yr_label, "花蓮縣公路客運平均搭乘里程折線圖"),
  missing_months = c())   # ← 花蓮公路無缺資料
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




#####學長作法檢測
# 1. 站牌代碼標準化
normalize_stop_code <- function(x) {
  x <- trimws(toupper(as.character(x)))
  x <- sub("^THB", "", x)
  x <- sub("^HUA", "", x)
  x[x %in% c("", "NA", "NULL")] <- NA_character_
  return(x)
}

# 2. 資料夾建立
check_path <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

# 3. △標記函數
flag_points <- function(x, y, months, flag, cex = 1.3) {
  if (is.null(flag)) return(invisible())
  idx <- which(months %in% flag)
  if (length(idx) == 0) return(invisible())
  points(x[idx], y[idx], pch = 2, cex = cex, col = "black", lwd = 2)
}

# 4. 繪圖函數
intercity_bus_monthly_average_mileage_plot <- function(df, title_main, missing_months = c()) {
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
  par(family = "kai", mar = c(7, 8, 4, 10))
  plot(x    = 1:nrow(data),
       y    = data$平均里程_公里,
       type = "o", lwd = 2, pch = 16,
       col  = color[1],
       xlab = "月份", ylab = "",
       ylim = c(20, 37),
       cex.main = 2, cex.lab = 1.5, cex.axis = 1.2, cex = 1.2,
       xaxt = "n", yaxt = "n", bty = "n")
  title(main = title_main, cex.main = 2, adj = 0)
  axis(side = 2, at = seq(20, 35, by = 5),
       labels = seq(20, 35, by = 5), las = 1, cex.axis = 1.2)
  x_pos <- seq(1, nrow(data), by = 2)
  axis(side = 1, at = x_pos,
       labels = data$年月[x_pos], cex.axis = 1.2, las = 1)
  grid()
  text(x      = 1:nrow(data),
       y      = data$平均里程_公里 + max(data$平均里程_公里) * 0.02,
       labels = round(data$平均里程_公里, 1),
       pos = 3, cex = 0.9, col = "black")
  flag_points(x      = 1:nrow(data),
              y      = data$平均里程_公里,
              months = data$年月,
              flag   = missing_months)
  if (length(missing_months) > 0) {
    legend("topright",
           legend = c("公里", "△ 業者未提交資料"),
           col    = c(color[1], "black"),
           lwd    = c(2, 2), pch = c(16, 2),
           bty    = "n", inset = c(-0.15, 0),
           xpd    = TRUE, cex = 1.2)
  } else {
    legend("topright",
           legend = c("公里"),
           col    = color[1],
           lwd    = 2, pch = 16, bty = "n",
           inset  = c(-0.15, 0), xpd = TRUE, cex = 1.2)
  }
}
# 花蓮專用（只用data5）
bus_mileage_cum_hua <- data5 %>%
  mutate(
    搭乘路線名稱     = as.character(搭乘路線名稱),
    搭乘附屬路線名稱 = as.character(搭乘附屬路線名稱),
    搭乘公車路線方向 = as.character(搭乘公車路線方向),
    站牌代碼         = normalize_stop_code(站牌代碼)
  ) %>%
  group_by(搭乘路線名稱, 搭乘附屬路線名稱, 搭乘公車路線方向) %>%
  arrange(站序資料, .by_group = TRUE) %>%
  mutate(
    站間距離 = replace_na(站間距離, 0),
    累積里程 = cumsum(站間距離)
  ) %>%
  ungroup()

# 台東專用（只用data7）
bus_mileage_cum_ttt <- data7 %>%
  mutate(
    搭乘路線名稱     = as.character(搭乘路線名稱),
    搭乘附屬路線名稱 = as.character(搭乘附屬路線名稱),
    搭乘公車路線方向 = as.character(搭乘公車路線方向),
    站牌代碼         = normalize_stop_code(站牌代碼)
  ) %>%
  group_by(搭乘路線名稱, 搭乘附屬路線名稱, 搭乘公車路線方向) %>%
  arrange(站序資料, .by_group = TRUE) %>%
  mutate(
    站間距離 = replace_na(站間距離, 0),
    累積里程 = cumsum(站間距離)
  ) %>%
  ungroup()

# 確認
nrow(bus_mileage_cum_hua)
nrow(bus_mileage_cum_ttt)
# 統一路線方向格式
data_taitung$搭乘公車路線方向 <- as.character(data_taitung$搭乘公車路線方向)
data_hualien$搭乘公車路線方向 <- as.character(data_hualien$搭乘公車路線方向)

df_mileage_taitung <- data_taitung %>%
  mutate(
    搭乘路線名稱     = as.character(搭乘路線名稱),
    搭乘附屬路線名稱 = as.character(搭乘附屬路線名稱),
    搭乘公車路線方向 = as.character(搭乘公車路線方向),
    資料日期         = as.Date(`資料代表日期(yyyy-MM-dd)`),
    上車站牌代碼     = normalize_stop_code(上車站牌代碼),
    下車站牌代碼     = normalize_stop_code(下車站牌代碼)
  ) %>%
  filter(
    資料日期 >= as.Date(paste0(year.from, "-", sprintf("%02d", month.from), "-01")),
    資料日期 <= as.Date(paste0(year.to, "-", sprintf("%02d", month.to), "-01")) +
      months(1) - days(1),
    !is.na(上車站牌代碼), 上車站牌代碼 != "",
    !is.na(下車站牌代碼), 下車站牌代碼 != ""
  ) %>%
  mutate(年月 = paste0(year(資料日期) - 1911, "/",
                     sprintf("%02d", month(資料日期)))) %>%
  # 層A：附屬路線＋方向＋站牌
  left_join(
    bus_mileage_cum_ttt %>%
      select(搭乘附屬路線名稱, 搭乘公車路線方向, 站牌代碼, 累積里程) %>%
      rename(上車里程_A = 累積里程),
    by = c("搭乘附屬路線名稱" = "搭乘附屬路線名稱",
           "搭乘公車路線方向" = "搭乘公車路線方向",
           "上車站牌代碼"     = "站牌代碼")
  ) %>%
  left_join(
    bus_mileage_cum_ttt %>%
      select(搭乘附屬路線名稱, 搭乘公車路線方向, 站牌代碼, 累積里程) %>%
      rename(下車里程_A = 累積里程),
    by = c("搭乘附屬路線名稱" = "搭乘附屬路線名稱",
           "搭乘公車路線方向" = "搭乘公車路線方向",
           "下車站牌代碼"     = "站牌代碼")
  ) %>%
  # 層B：主路線＋方向＋站牌
  left_join(
    bus_mileage_cum_ttt %>%
      group_by(搭乘路線名稱, 搭乘公車路線方向, 站牌代碼) %>%
      slice(1) %>% ungroup() %>%
      select(搭乘路線名稱, 搭乘公車路線方向, 站牌代碼, 累積里程) %>%
      rename(上車里程_B = 累積里程),
    by = c("搭乘路線名稱"     = "搭乘路線名稱",
           "搭乘公車路線方向" = "搭乘公車路線方向",
           "上車站牌代碼"     = "站牌代碼")
  ) %>%
  left_join(
    bus_mileage_cum_ttt %>%
      group_by(搭乘路線名稱, 搭乘公車路線方向, 站牌代碼) %>%
      slice(1) %>% ungroup() %>%
      select(搭乘路線名稱, 搭乘公車路線方向, 站牌代碼, 累積里程) %>%
      rename(下車里程_B = 累積里程),
    by = c("搭乘路線名稱"     = "搭乘路線名稱",
           "搭乘公車路線方向" = "搭乘公車路線方向",
           "下車站牌代碼"     = "站牌代碼")
  ) %>%
  # 層C：主路線＋站牌（不限方向）
  left_join(
    bus_mileage_cum_ttt %>%
      group_by(搭乘路線名稱, 站牌代碼) %>%
      slice(1) %>% ungroup() %>%
      select(搭乘路線名稱, 站牌代碼, 累積里程) %>%
      rename(上車里程_C = 累積里程),
    by = c("搭乘路線名稱" = "搭乘路線名稱",
           "上車站牌代碼" = "站牌代碼")
  ) %>%
  left_join(
    bus_mileage_cum_ttt %>%
      group_by(搭乘路線名稱, 站牌代碼) %>%
      slice(1) %>% ungroup() %>%
      select(搭乘路線名稱, 站牌代碼, 累積里程) %>%
      rename(下車里程_C = 累積里程),
    by = c("搭乘路線名稱" = "搭乘路線名稱",
           "下車站牌代碼" = "站牌代碼")
  ) %>%
  mutate(
    上車里程 = coalesce(上車里程_A, 上車里程_B, 上車里程_C),
    下車里程 = coalesce(下車里程_A, 下車里程_B, 下車里程_C),
    里程      = 下車里程 - 上車里程,
    里程      = ifelse(里程 > 0, 里程, NA_real_)
  ) %>%
  filter(!is.na(里程)) %>%
  select(-上車里程_A, -上車里程_B, -上車里程_C,
         -下車里程_A, -下車里程_B, -下車里程_C)

nrow(df_mileage_taitung)
sum(is.na(df_mileage_taitung$里程))
df_mileage_hualien <- data_hualien %>%
  mutate(
    搭乘路線名稱     = as.character(搭乘路線名稱),
    搭乘附屬路線名稱 = as.character(搭乘附屬路線名稱),
    搭乘公車路線方向 = as.character(搭乘公車路線方向),
    資料日期         = as.Date(`資料代表日期(yyyy-MM-dd)`),
    上車站牌代碼     = normalize_stop_code(上車站牌代碼),
    下車站牌代碼     = normalize_stop_code(下車站牌代碼)
  ) %>%
  filter(
    資料日期 >= as.Date(paste0(year.from, "-", sprintf("%02d", month.from), "-01")),
    資料日期 <= as.Date(paste0(year.to, "-", sprintf("%02d", month.to), "-01")) +
      months(1) - days(1),
    !is.na(上車站牌代碼), 上車站牌代碼 != "",
    !is.na(下車站牌代碼), 下車站牌代碼 != ""
  ) %>%
  mutate(年月 = paste0(year(資料日期) - 1911, "/",
                     sprintf("%02d", month(資料日期)))) %>%
  left_join(
    bus_mileage_cum_hua %>%
      select(搭乘附屬路線名稱, 搭乘公車路線方向, 站牌代碼, 累積里程) %>%
      rename(上車里程_A = 累積里程),
    by = c("搭乘附屬路線名稱" = "搭乘附屬路線名稱",
           "搭乘公車路線方向" = "搭乘公車路線方向",
           "上車站牌代碼"     = "站牌代碼")
  ) %>%
  left_join(
    bus_mileage_cum_hua %>%
      select(搭乘附屬路線名稱, 搭乘公車路線方向, 站牌代碼, 累積里程) %>%
      rename(下車里程_A = 累積里程),
    by = c("搭乘附屬路線名稱" = "搭乘附屬路線名稱",
           "搭乘公車路線方向" = "搭乘公車路線方向",
           "下車站牌代碼"     = "站牌代碼")
  ) %>%
  left_join(
    bus_mileage_cum_hua %>%
      group_by(搭乘路線名稱, 搭乘公車路線方向, 站牌代碼) %>%
      slice(1) %>% ungroup() %>%
      select(搭乘路線名稱, 搭乘公車路線方向, 站牌代碼, 累積里程) %>%
      rename(上車里程_B = 累積里程),
    by = c("搭乘路線名稱"     = "搭乘路線名稱",
           "搭乘公車路線方向" = "搭乘公車路線方向",
           "上車站牌代碼"     = "站牌代碼")
  ) %>%
  left_join(
    bus_mileage_cum_hua %>%
      group_by(搭乘路線名稱, 搭乘公車路線方向, 站牌代碼) %>%
      slice(1) %>% ungroup() %>%
      select(搭乘路線名稱, 搭乘公車路線方向, 站牌代碼, 累積里程) %>%
      rename(下車里程_B = 累積里程),
    by = c("搭乘路線名稱"     = "搭乘路線名稱",
           "搭乘公車路線方向" = "搭乘公車路線方向",
           "下車站牌代碼"     = "站牌代碼")
  ) %>%
  left_join(
    bus_mileage_cum_hua %>%
      group_by(搭乘路線名稱, 站牌代碼) %>%
      slice(1) %>% ungroup() %>%
      select(搭乘路線名稱, 站牌代碼, 累積里程) %>%
      rename(上車里程_C = 累積里程),
    by = c("搭乘路線名稱" = "搭乘路線名稱",
           "上車站牌代碼" = "站牌代碼")
  ) %>%
  left_join(
    bus_mileage_cum_hua %>%
      group_by(搭乘路線名稱, 站牌代碼) %>%
      slice(1) %>% ungroup() %>%
      select(搭乘路線名稱, 站牌代碼, 累積里程) %>%
      rename(下車里程_C = 累積里程),
    by = c("搭乘路線名稱" = "搭乘路線名稱",
           "下車站牌代碼" = "站牌代碼")
  ) %>%
  mutate(
    上車里程 = coalesce(上車里程_A, 上車里程_B, 上車里程_C),
    下車里程 = coalesce(下車里程_A, 下車里程_B, 下車里程_C),
    里程      = 下車里程 - 上車里程,
    里程      = ifelse(里程 > 0, 里程, NA_real_)
  ) %>%
  filter(!is.na(里程)) %>%
  select(-上車里程_A, -上車里程_B, -上車里程_C,
         -下車里程_A, -下車里程_B, -下車里程_C)

nrow(df_mileage_hualien)
sum(is.na(df_mileage_hualien$里程))
df_mileage_taitung %>%
  group_by(路線類別) %>%
  summarise(平均里程公里 = mean(里程) / 1000) %>%
  arrange(desc(平均里程公里))

df_mileage_hualien %>%
  group_by(路線類別) %>%
  summarise(平均里程公里 = mean(里程) / 1000) %>%
  arrange(desc(平均里程公里))
yr_label <- paste0(year.from - 1911, "年", month.from, "月至",
                   year.to - 1911, "年", month.to, "月")
path <- "imgs/line_plot/"
check_path(path)

# 台東公路（114/11 東台灣客運缺資料）
png(paste0(path, yr_label, "臺東縣公路客運平均搭乘里程折線圖.png"),
    width = 15, height = 5, units = "in", res = 300, family = "kai")
intercity_bus_monthly_average_mileage_plot(
  df_mileage_taitung,
  paste0(yr_label, "臺東縣公路客運平均搭乘里程折線圖"),
  missing_months = c("114/11"))
dev.off()

# 花蓮公路（缺資料月份待確認）
png(paste0(path, yr_label, "花蓮縣公路客運平均搭乘里程折線圖.png"),
    width = 15, height = 5, units = "in", res = 300, family = "kai")
intercity_bus_monthly_average_mileage_plot(
  df_mileage_hualien,
  paste0(yr_label, "花蓮縣公路客運平均搭乘里程折線圖"))
dev.off()