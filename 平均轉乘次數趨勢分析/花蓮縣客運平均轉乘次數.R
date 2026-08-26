library(dplyr)
library(data.table)
library(lubridate)
library(openxlsx)

data1 <- fread(
  "C:/Users/Angela/Documents/運籌期末/公路客運2024_to_202606.txt"
)
data2 <- fread(
  "C:/Users/Angela/Documents/運籌期末/花蓮縣公車.txt"
)

data1$搭乘路線名稱 <- trimws(as.character(data1$搭乘路線名稱))
data2$搭乘路線名稱 <- trimws(as.character(data2$搭乘路線名稱))

hualien_routes <- c(
  "1121","1122","1125","1128","1129","1130","1132","1133","1135","1136",
  "1137","1139","1140","1141","1142","1143","1145","8119","8161","8173","8181"
) 
data1 <- data1[
  搭乘路線名稱 %in% hualien_routes
]

#統一日期與時間格式
# 公路客運
data1 <- data1 %>%
  mutate(
    刷卡上車時間 = as.POSIXct(刷卡上車時間),
    刷卡下車時間 = as.POSIXct(刷卡下車時間),
    `資料代表日期(yyyy-MM-dd)` =
      as.Date(`資料代表日期(yyyy-MM-dd)`)
  )
# 市區客運
data2 <- data2 %>%
  mutate(
    刷卡上車時間 = as.POSIXct(
      刷卡上車時間,
      format = "%Y/%m/%d %H:%M"
    ),
    刷卡下車時間 = as.POSIXct(
      刷卡下車時間,
      format = "%Y/%m/%d %H:%M"
    ),
    `資料代表日期(yyyy-MM-dd)` = as.Date(
      `資料代表日期(yyyy-MM-dd)`,
      format = "%Y/%m/%d"
    )
  )

hualien_all <- bind_rows(
  data1 %>% mutate(客運類型 = "公路客運"),
  data2 %>% mutate(客運類型 = "市區客運")
)

#-----轉乘資料整理-----

data_transfer <- hualien_all %>%
  mutate(
    資料日期 = `資料代表日期(yyyy-MM-dd)`,
    
    年月 = sprintf(
      "%03d/%02d",
      year(資料日期) - 1911,
      month(資料日期)
    ),
    
    票種分類 = ifelse(
      票種類型 == 4,
      "TPASS",
      "其他票種"
    )
  ) %>%
  
  filter(
    資料日期 >= as.Date("2024-06-01"),
    資料日期 <= as.Date("2026-06-30"),
    
    !is.na(卡號),
    !is.na(刷卡上車時間),
    !is.na(刷卡下車時間),
    !is.na(搭乘路線名稱)
  )

#--------------------------------------------------
# 轉乘判斷條件
# 1. 同一卡號
# 2. 同一天
# 3. 相鄰兩筆搭乘
# 4. [前次下車,本次上車] <= 120分鐘
# 5. 前後路線不同
# 6. 不限制業者

transfer_trip <- data_transfer %>%
  
  arrange(
    卡號,
    資料日期,
    刷卡上車時間
  ) %>%
  
  group_by(
    卡號,
    資料日期
  ) %>%
  
  mutate(
    前一筆下車時間 = lag(刷卡下車時間),
    前一路線 = lag(搭乘路線名稱),
    間隔分鐘 = as.numeric(
      difftime(
        刷卡上車時間,
        前一筆下車時間,
        units = "mins"
      )
    ),
    
    # 判斷是否為轉乘
    是否轉乘 = if_else(
      !is.na(間隔分鐘) &
        間隔分鐘 >= 0 &
        間隔分鐘 <= 120 &
        !is.na(前一路線) &
        搭乘路線名稱 != 前一路線,
      1L,
      0L
    )
  ) %>%
  
  ungroup()

#----檢查轉乘判定結果----

transfer_check <- transfer_trip %>%
  
  filter(是否轉乘 == 1) %>%
  
  select(
    卡號,
    資料日期,
    前一路線,
    前一筆下車時間,
    本次路線 = 搭乘路線名稱,
    本次上車時間 = 刷卡上車時間,
    間隔分鐘,
    票種分類,
    客運類型
  ) %>%
  
  arrange(
    資料日期,
    卡號,
    本次上車時間
  )

write.xlsx(
  transfer_check,
  "C:/Users/Angela/Documents/運籌期末/花蓮縣轉乘人次詳細統計表.xlsx",
  overwrite = TRUE
)

#----每月平均轉乘次數----

monthly_transfer <- transfer_trip %>%
  group_by(年月) %>%
  
  summarise(
    # 全部
    總搭乘人次 = sum(
      原始票證筆數,
      na.rm = TRUE
    ),
    
    總轉乘次數 = sum(
      原始票證筆數[是否轉乘 == 1],
      na.rm = TRUE
    ),
    
    # TPASS
    TPASS搭乘人次 = sum(
      原始票證筆數[
        票種分類 == "TPASS"
      ],
      na.rm = TRUE
    ),
    
    TPASS轉乘次數 = sum(
      原始票證筆數[
        是否轉乘 == 1 &
          票種分類 == "TPASS"
      ],
      na.rm = TRUE
    ),
    
    # 其他票種
    其他票種搭乘人次 = sum(
      原始票證筆數[
        票種分類 == "其他票種"
      ],
      na.rm = TRUE
    ),
    
    其他票種轉乘次數 = sum(
      原始票證筆數[
        是否轉乘 == 1 &
          票種分類 == "其他票種"
      ],
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  
  mutate(
    平均轉乘次數 = round(
      總轉乘次數 / 總搭乘人次,
      3
    ),
    
    TPASS平均轉乘次數 = round(
      TPASS轉乘次數 / TPASS搭乘人次,
      3
    ),
    
    其他票種平均轉乘次數 = round(
      其他票種轉乘次數 / 其他票種搭乘人次,
      3
    )
  ) %>%
  
  arrange(年月)

write.xlsx(
  monthly_transfer,
  "C:/Users/Angela/Documents/運籌期末/花蓮縣月平均轉乘次數.xlsx",
  overwrite = TRUE
)


#-----繪圖-----

windowsFonts(msjh = windowsFont("Microsoft JhengHei"))

png("花蓮縣客運平均轉乘次數折線圖.png",
    width = 15, height = 5, units = "in", res = 300 )

par(
  family = "msjh",
  mar = c(4.5, 5, 4, 8),
  mgp = c(3.2, 0.8, 0),
  bty = "l",
  cex.lab = 1.5
)
x <- 1:nrow(monthly_transfer)

y <- c(
  monthly_transfer$平均轉乘次數,
  monthly_transfer$TPASS平均轉乘次數,
  monthly_transfer$其他票種平均轉乘次數
)

ymin <- floor(min(y, na.rm = TRUE) * 100) / 100 
ymax <- ceiling(max(y, na.rm = TRUE) * 100) / 100 

plot(
  x,
  monthly_transfer$平均轉乘次數,
  type = "n",
  xaxt = "n",
  yaxt = "n", 
  xlab = "月份",
  ylab = "平均轉乘次數",
  main = "",
  ylim = c(ymin - 0.002, ymax + 0.002)
)

title(
  main = "113年6月至115年6月花蓮縣客運平均轉乘次數折線圖",
  adj = 0,
  cex.main = 1.8
)

grid()

lines(
  x,
  monthly_transfer$平均轉乘次數,
  type = "o",
  pch = 16,
  lwd = 2,
  col = "grey25"
)

lines(
  x,
  monthly_transfer$TPASS平均轉乘次數,
  type = "o",
  pch = 16,
  lwd = 2,
  col = "grey55"
)

lines(
  x,
  monthly_transfer$其他票種平均轉乘次數,
  type = "o",
  pch = 16,
  lwd = 2,
  col = "grey70"
)

x_pos <- seq(1, nrow(monthly_transfer), by = 2)

axis(
  side = 1,
  at = x_pos,
  labels = monthly_transfer$年月[x_pos],
  las = 1,
  cex.axis = 1.2
)

axis(
  side = 2,
  las = 1,
  cex.axis = 1.2
)

legend(
  "topright",
  inset = c(-0.125, 0),
  xpd = TRUE,
  legend = c(
    "平均轉乘次數",
    "TPASS",
    "其他票種"
  ),
  col = c(
    "grey25",
    "grey55",
    "grey70"
  ),
  lwd = 2,
  pch = 16,
  bty = "n",
  cex = 1.2
)

dev.off()
