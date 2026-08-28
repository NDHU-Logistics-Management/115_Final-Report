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
data1$搭乘附屬路線名稱 <- as.character(data1$搭乘附屬路線名稱)

data_taitung <- data1 %>%
  inner_join(route_map_taitung, by = "搭乘附屬路線名稱")

nrow(data_taitung)
table(data_taitung$路線類別)
# 台東市區路線過濾
valid_routes_ttt_city <- as.character(unique(data8$搭乘附屬路線名稱))
data_ttt_city <- data3 %>%
  mutate(搭乘附屬路線名稱 = as.character(搭乘附屬路線名稱)) %>%
  filter(搭乘附屬路線名稱 %in% valid_routes_ttt_city)

data_taitung <- data_taitung %>%
  mutate(
    搭乘路線名稱     = as.character(搭乘路線名稱),
    搭乘附屬路線名稱 = as.character(搭乘附屬路線名稱)
  )

data_ttt_city <- data_ttt_city %>%
  mutate(
    搭乘路線名稱     = as.character(搭乘路線名稱),
    搭乘附屬路線名稱 = as.character(搭乘附屬路線名稱)
  )


# 再合併
data_taitung_all <- bind_rows(
  data_taitung %>% mutate(客運類型 = "公路客運"),
  data_ttt_city %>% mutate(客運類型 = "市區客運")
)

# 確認合併後筆數
nrow(data_taitung_all)
# 應等於
nrow(data_taitung) + nrow(data_ttt_city)

# 看空值和非空值的分布
sum(is.na(data_taitung_all$轉乘代碼紀錄))          # NA 有幾筆
sum(data_taitung_all$轉乘代碼紀錄 == "", na.rm=TRUE) # 空字串有幾筆
sum(data_taitung_all$轉乘代碼紀錄 == 0,  na.rm=TRUE) # 0 有幾筆

# 看所有不重複的值
sort(unique(data_taitung_all$轉乘代碼紀錄))
# 看有沒有代碼是 0 或空值
table(data_taitung_all$轉乘代碼紀錄)


year.from  <- 2024
month.from <- 6
year.to    <- 2026
month.to   <- 6

# ── 統一時間格式 ──────────────────────────────────────────────
data_taitung <- data_taitung %>%
  mutate(
    刷卡上車時間               = as.POSIXct(刷卡上車時間),
    刷卡下車時間               = as.POSIXct(刷卡下車時間),
    `資料代表日期(yyyy-MM-dd)` = as.Date(`資料代表日期(yyyy-MM-dd)`)
  )

data_ttt_city <- data_ttt_city %>%
  mutate(
    刷卡上車時間               = as.POSIXct(刷卡上車時間),
    刷卡下車時間               = as.POSIXct(刷卡下車時間),
    `資料代表日期(yyyy-MM-dd)` = as.Date(`資料代表日期(yyyy-MM-dd)`)
  )

# 重新合併（時間格式統一後）
data_taitung_all <- bind_rows(
  data_taitung  %>% mutate(客運類型 = "公路客運"),
  data_ttt_city %>% mutate(客運類型 = "市區客運")
)

# ── 轉乘資料整理 ──────────────────────────────────────────────
data_transfer <- data_taitung_all %>%
  mutate(
    資料日期 = `資料代表日期(yyyy-MM-dd)`,
    年月     = sprintf("%03d/%02d",
                     year(資料日期) - 1911,
                     month(資料日期)),
    票種分類 = ifelse(票種類型 == 4, "TPASS", "其他票種")
  ) %>%
  filter(
    資料日期 >= as.Date("2024-06-01"),
    資料日期 <= as.Date("2026-06-30"),
    !is.na(卡號),
    !is.na(刷卡上車時間),
    !is.na(刷卡下車時間),
    !is.na(搭乘路線名稱)
  )

# ── 轉乘判斷 ──────────────────────────────────────────────────
# 條件：同一卡號、同一天、相鄰兩筆、前次下車到本次上車0~120分鐘、前後路線不同、不限業者
transfer_trip <- data_transfer %>%
  arrange(卡號, 資料日期, 刷卡上車時間) %>%
  group_by(卡號, 資料日期) %>%
  mutate(
    前一筆下車時間 = lag(刷卡下車時間),
    前一路線       = lag(搭乘路線名稱),
    間隔分鐘       = as.numeric(
      difftime(刷卡上車時間, 前一筆下車時間, units = "mins")
    ),
    是否轉乘 = if_else(
      !is.na(間隔分鐘)    &
        間隔分鐘 >= 0     &
        間隔分鐘 <= 120   &
        !is.na(前一路線)  &
        搭乘路線名稱 != 前一路線,
      1L, 0L
    )
  ) %>%
  ungroup()

# ── 檢查轉乘判定結果 ──────────────────────────────────────────
transfer_check <- transfer_trip %>%
  filter(是否轉乘 == 1) %>%
  select(
    卡號,
    資料日期,
    前一路線,
    前一筆下車時間,
    本次路線     = 搭乘路線名稱,
    本次上車時間 = 刷卡上車時間,
    間隔分鐘,
    票種分類,
    客運類型
  ) %>%
  arrange(資料日期, 卡號, 本次上車時間)

check_path("output_tables/")
write.xlsx(
  transfer_check,
  "output_tables/臺東縣轉乘人次詳細統計表.xlsx",
  overwrite = TRUE
)

# ── 每月平均轉乘次數 ──────────────────────────────────────────
monthly_transfer <- transfer_trip %>%
  group_by(年月) %>%
  summarise(
    總搭乘人次       = sum(原始票證筆數, na.rm = TRUE),
    總轉乘次數       = sum(原始票證筆數[是否轉乘 == 1], na.rm = TRUE),
    TPASS搭乘人次    = sum(原始票證筆數[票種分類 == "TPASS"], na.rm = TRUE),
    TPASS轉乘次數    = sum(原始票證筆數[是否轉乘 == 1 & 票種分類 == "TPASS"], na.rm = TRUE),
    其他票種搭乘人次 = sum(原始票證筆數[票種分類 == "其他票種"], na.rm = TRUE),
    其他票種轉乘次數 = sum(原始票證筆數[是否轉乘 == 1 & 票種分類 == "其他票種"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    平均轉乘次數         = round(總轉乘次數 / 總搭乘人次, 3),
    TPASS平均轉乘次數    = round(TPASS轉乘次數 / TPASS搭乘人次, 3),
    其他票種平均轉乘次數 = round(其他票種轉乘次數 / 其他票種搭乘人次, 3)
  ) %>%
  arrange(年月)

write.xlsx(
  monthly_transfer,
  "output_tables/臺東縣月平均轉乘次數.xlsx",
  overwrite = TRUE
)

print(monthly_transfer)


# ── 定義 check_path 函數 ──────────────────────────────────────
check_path <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)


# ── 建立統計表（對應圖片格式）────────────────────────────────
transfer_summary <- monthly_transfer %>%
  select(
    `年/月`            = 年月,
    `整體平均轉乘次數`  = 平均轉乘次數,
    `TPASS平均轉乘次數` = TPASS平均轉乘次數,
    `其他票種平均轉乘次數` = 其他票種平均轉乘次數
  )
}
# ── 繪圖函數 ──────────────────────────────────────────────────
bus_average_transfer_plot <- function(df, title_main) {
  color <- gray.colors(3)
  
  par(family = "kai", mar = c(5, 6, 4, 10))
  
  all_vals <- c(df$平均轉乘次數,
                df$TPASS平均轉乘次數,
                df$其他票種平均轉乘次數)
  
  plot(x    = 1:nrow(df),
       y    = df$平均轉乘次數,
       type = "o", lwd = 2, pch = 16,
       col  = color[1],
       xlab = "月份", ylab = "",
       ylim = c(min(all_vals, na.rm = TRUE) * 0.9,
                max(all_vals, na.rm = TRUE) * 1.2),
       cex.main = 2, cex.lab = 2, cex.axis = 1.5, cex = 1.5,
       xaxt = "n", yaxt = "n", bty = "n")
  
  lines(x = 1:nrow(df), y = df$TPASS平均轉乘次數,
        type = "o", lwd = 2, pch = 16, cex = 1.5, col = color[2])
  
  lines(x = 1:nrow(df), y = df$其他票種平均轉乘次數,
        type = "o", lwd = 2, pch = 16, cex = 1.5, col = color[3])
  
  title(main = title_main, cex.main = 2, adj = 0)
  
  axis(side = 1, at = 1:nrow(df),
       labels = df$年月, cex.axis = 1.5)
  axis(side = 2, las = 1, cex.axis = 1.5, line = -1.5)
  
  grid()
  
  legend("topright",
         legend = c("平均轉乘次數", "TPASS", "其他票種"),
         col    = color[1:3],
         lwd = 2, pch = 16, bty = "n",
         inset = c(-0.15, 0), xpd = TRUE, cex = 1.5)
}

# ── 儲存圖片 ──────────────────────────────────────────────────
yr_label <- paste0(year.from - 1911, "年", month.from, "月至",
                   year.to   - 1911, "年", month.to,   "月")
path <- "img/line_plot/"
check_path(path)

png(paste0(path, yr_label, "臺東縣客運平均轉乘次數折線圖.png"),
    width = 15, height = 5, units = "in", res = 300, family = "kai")
bus_average_transfer_plot(
  monthly_transfer,
  paste0(yr_label, "臺東縣客運平均轉乘次數折線圖"))
dev.off()
# ── 存 Excel ──────────────────────────────────────────────────
check_path("output_tables/")

write.xlsx(
  transfer_summary,
  paste0("output_tables/", yr_label,
         "臺東縣客運平均轉乘次數統計表.xlsx"),
  overwrite = TRUE
)

message("✅ 統計表已儲存！")