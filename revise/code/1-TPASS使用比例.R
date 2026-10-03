# ============================================================
# 第 1 章　TPASS 使用比例
#
# 公路、市區四組共用同一計算與作圖函數（原 Rmd 兩份定義只差註解與 copy()，已合併）。
# 輸出：4 張 png、TPASS使用比例.xlsx
# ============================================================

source(file.path(if (exists("CODE_DIR")) CODE_DIR else ".", "00_設定.R"), encoding = "UTF-8")
ensure_clean_data()

# =========================
# 計算函數
# =========================

calculate_tpass_ratio <- function(data) {

  data <- copy(data)

  # 建立月份
  data[, 月份 := sprintf(
    "%03d/%02d",
    as.integer(format(`資料代表日期(yyyy-MM-dd)`, "%Y")) - 1911,
    as.integer(format(`資料代表日期(yyyy-MM-dd)`, "%m"))
  )]

  # 篩選期間
  data <- data[
    月份 >= "113/06" &
    月份 <= "115/06"
  ]

  # TPASS
  tpass_data <- data[
    票種次類型 %in% c(
      "#HUA-199",
      "#HUA-399",
      "#TTT-299"
    )
  ]

  # 全部搭乘次數
  total <- data[
    ,
    .(
      總搭乘人次 = .N
    ),
    by = 月份
  ]

  # TPASS 搭乘次數
  tpass_total <- tpass_data[
    ,
    .(
      TPASS搭乘人次 = .N
    ),
    by = 月份
  ]

  # 合併
  result <- merge(
    total,
    tpass_total,
    by = "月份",
    all.x = TRUE
  )

  # 沒有 TPASS 的月份補 0
  result[
    is.na(TPASS搭乘人次),
    TPASS搭乘人次 := 0
  ]

  # TPASS 使用比例
  result[
    ,
    TPASS比例 :=
      TPASS搭乘人次 /
      總搭乘人次 * 100
  ]

  result[
    ,
    TPASS比例 := round(
      TPASS比例,
      1
    )
  ]

  setorder(
    result,
    月份
  )

  return(result)
}

# =========================
# 計算
# =========================

花蓮公路_TPASS比例 <- calculate_tpass_ratio(
  copy(花蓮公路)
)

臺東公路_TPASS比例 <- calculate_tpass_ratio(
  copy(臺東公路)
)

print(花蓮公路_TPASS比例)
print(臺東公路_TPASS比例)

花蓮市區_TPASS比例 <- calculate_tpass_ratio(
  花蓮市區_clean
)

print(
  花蓮市區_TPASS比例
)


# =========================
# 臺東市區
# =========================

臺東市區_TPASS比例 <- calculate_tpass_ratio(
  臺東市區_clean
)

print(
  臺東市區_TPASS比例
)


# =========================
# 作圖函數
# =========================

plot_tpass_ratio <- function(
  data,
  title,
  filename,
  flag = NULL
) {

  ymin <- floor(
    min(
      data$TPASS比例,
      na.rm = TRUE
    ) / 5
  ) * 5

  ymax <- ceiling(
    max(
      data$TPASS比例,
      na.rm = TRUE
    ) / 5
  ) * 5

  png(
    filename = file.path(FIG_DIR, filename),
    width = 15,
    height = 5,
    units = "in",
    res = 300
  )

  par(
    family = "msjh",
    mar = c(5, 6, 4, 8),
    mgp = c(3.5, 0.8, 0),
    bty = "l",
    cex.lab = 1.5
  )

  plot(
    1:nrow(data),
    data$TPASS比例,
    type = "n",
    xaxt = "n",
    yaxt = "n",
    xlab = "月份",
    ylab = "TPASS使用比例",
    main = "",
    cex.main = 1.7,
    ylim = c(
      ymin - 0.8,
      ymax + 0.8
    )
  )

  # 水平網格
  abline(
    h = seq(
      ymin,
      ymax,
      by = 5
    ),
    col = "grey60",
    lty = 3,
    lwd = 0.6
  )

  # 垂直網格
  abline(
    v = seq(
      1,
      nrow(data),
      by = 4
    ),
    col = "grey60",
    lty = 3,
    lwd = 0.6
  )

  # 折線
  lines(
    x = 1:nrow(data),
    y = data$TPASS比例,
    type = "o",
    pch = 16,
    lwd = 1.5,
    cex = 1,
    col = "grey30"
  )

  # [修訂 A1/A2] 業者資料不完整月份
  flag_points(1:nrow(data), data$TPASS比例, data$月份, flag, cex = 1.3)

  # [修訂] 主標題置左
  title(main = title, adj = 0, cex.main = 1.7)

  # X 軸
  x_pos <- seq(
    1,
    nrow(data),
    by = 2
  )

  axis(
    side = 1,
    at = x_pos,
    labels = data$月份[x_pos],
    las = 1,
    cex.axis = 1.2
  )

  # Y 軸
  axis(
    side = 2,
    at = seq(
      ymin,
      ymax,
      by = 5
    ),
    las = 1,
    cex.axis = 1.2
  )

  # 數值標籤
  text(
    x = 1:nrow(data),
    y = data$TPASS比例 + 1.75,
    labels = sprintf(
      "%.1f%%",
      data$TPASS比例
    ),
    cex = 0.95
  )

  dev.off()
}

# =========================
# 花蓮作圖
# =========================

plot_tpass_ratio(
  花蓮公路_TPASS比例,
  "113年6月至115年6月花蓮縣公路客運TPASS使用比例折線圖",
  "花蓮公路客運TPASS比例.png"
)


# =========================
# 臺東作圖
# =========================

plot_tpass_ratio(
  臺東公路_TPASS比例,
  "113年6月至115年6月臺東縣公路客運TPASS使用比例折線圖",
  "臺東公路客運TPASS比例.png",
  flag = FLAG_MONTHS$臺東公路
)

# =========================
# 花蓮市區作圖
# =========================

plot_tpass_ratio(
  花蓮市區_TPASS比例,
  "113年6月至115年6月花蓮縣市區客運TPASS使用比例折線圖",
  "花蓮市區客運TPASS比例.png",
  flag = FLAG_MONTHS$花蓮市區
)


# =========================
# 臺東市區作圖
# =========================

plot_tpass_ratio(
  臺東市區_TPASS比例,
  "113年6月至115年6月臺東縣市區客運TPASS使用比例折線圖",
  "臺東市區客運TPASS比例.png"
)

# =========================
# 表格
# =========================

table_list <- list(
  "花蓮公路" = 花蓮公路_TPASS比例,
  "臺東公路" = 臺東公路_TPASS比例,
  "花蓮市區" = 花蓮市區_TPASS比例,
  "臺東市區" = 臺東市區_TPASS比例
)

write.xlsx(
  table_list,
  file = file.path(TBL_DIR, "TPASS使用比例.xlsx"),
  overwrite = TRUE
)
