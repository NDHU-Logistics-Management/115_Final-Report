
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

# ══════════════════════════════════════════════════════════════
# 委員建議補充分析
# 針對114年12月花蓮縣公路客運TPASS持卡人進行分析
# 目的：回應委員對「僅一次」族群及高頻使用族群之疑問
# ══════════════════════════════════════════════════════════════

# ── Step 1：篩出114年12月花蓮公路客運TPASS持卡人 ─────────────
# 說明：只保留HUA月票（花蓮在地月票），排除TTT-299（臺東月票）
dec_hualien <- data_hualien %>%
  mutate(資料日期 = as.Date(`資料代表日期(yyyy-MM-dd)`)) %>%
  filter(year(資料日期) == 2025, month(資料日期) == 12,
         票種次類型 %in% c("#HUA-399", "#HUA-199"))

# ── Step 2：每人搭乘次數分析 ─────────────────────────────────
# 說明：以卡號+票種次類型為單位，計算每人當月搭乘次數
# 並區分三個族群：僅一次、二至四次、五次以上
dec_analysis <- data_hualien %>%
  mutate(資料日期 = as.Date(`資料代表日期(yyyy-MM-dd)`)) %>%
  filter(year(資料日期) == 2025, month(資料日期) == 12) %>%
  group_by(卡號, 票種次類型) %>%
  summarise(搭乘次數 = n(), .groups = "drop") %>%
  mutate(
    族群 = case_when(
      搭乘次數 == 1 ~ "僅一次",
      搭乘次數 <= 4 ~ "二至四次",
      搭乘次數 >= 5 ~ "五次以上"
    )
  )

# ── Step 3：「僅一次」族群中TTT-299的比例 ────────────────────
# 說明：確認「僅一次」中有多少是臺東月票持卡人
# 結果：88%持TTT-299，為臺東通勤者偶爾搭乘跨縣市路線所致
dec_analysis %>%
  filter(族群 == "僅一次", 票種次類型 != "") %>%
  group_by(票種次類型) %>%
  summarise(人數 = n()) %>%
  mutate(佔比 = round(人數 / sum(人數) * 100, 1))

# ── Step 4：排除TTT-299後的族群分布 ──────────────────────────
# 說明：排除臺東月票後，花蓮在地HUA月票持卡人的族群分布
# 結果：五次以上71%、二至四次21.1%、僅一次7.9%
dec_analysis %>%
  filter(票種次類型 != "", 票種次類型 != "#TTT-299") %>%
  group_by(族群) %>%
  summarise(人數 = n()) %>%
  mutate(佔比 = round(人數 / sum(人數) * 100, 1))

# ── Step 5：找出HUA月票高頻使用者（五次以上）────────────────
# 說明：分母修正為只看五次以上的花蓮在地HUA月票持卡人
high_freq_cards <- dec_analysis %>%
  filter(族群 == "五次以上",
         票種次類型 %in% c("#HUA-399", "#HUA-199")) %>%
  pull(卡號)

# 篩出這些高頻使用者的搭乘記錄
dec_high_freq <- dec_hualien %>%
  filter(卡號 %in% high_freq_cards)

nrow(dec_high_freq)

# ── Step 6：高頻使用族群持卡身分 ─────────────────────────────
dec_high_freq %>%
  as.data.frame() %>%
  group_by(持卡身分) %>%
  summarise(人次 = n()) %>%
  mutate(佔比 = round(人次 / sum(人次) * 100, 1)) %>%
  arrange(desc(人次))

# ── Step 7：高頻使用族群搭乘路線類別 ─────────────────────────
dec_high_freq %>%
  as.data.frame() %>%
  group_by(路線類別) %>%
  summarise(人次 = n()) %>%
  mutate(佔比 = round(人次 / sum(人次) * 100, 1)) %>%
  arrange(desc(人次))