if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("尚未安裝 data.table，請先執行 install.packages('data.table')")
}

library(data.table)

# ---------------------------
# 0. 可自行修改的設定
# ---------------------------

# 若 CSV 不在目前工作目錄，請改成完整路徑，例如：
# csv_path <- "C:/Users/user/Desktop/臺東縣公車.csv"
csv_path <- file.choose()

output_dir <- file.path(
  dirname(csv_path),
  "臺東縣市區客運_11306至11506_輸出"
)

start_date <- as.IDate("2024-06-01") # 民國113年6月1日
end_date   <- as.IDate("2026-06-30") # 民國115年6月30日

# 搭乘次數區間設定。
# 若報告規定的區間不同，只需修改下面兩行，圖與表會一起更新。
ride_breaks <- c(0, 1, 5, 10, 20, 40, Inf)
ride_labels <- c("1次", "2–5次", "6–10次", "11–20次", "21–40次", "41次以上")

# 輸出圖片大小（適合橫式報告）
png_width  <- 4800
png_height <- 2400
png_res    <- 300

if (!file.exists(csv_path)) {
  stop(paste0("找不到資料檔：", csv_path, "\n請修改 csv_path 為正確位置。"))
}

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Windows 使用微軟正黑體；其他系統使用 sans。
if (.Platform$OS.type == "windows") {
  windowsFonts(Kai = windowsFont("Microsoft JhengHei"))
  plot_family <- "Kai"
} else {
  plot_family <- "sans"
}

# ---------------------------
# 1. 讀取及篩選資料
# ---------------------------

needed_columns <- c(
  "業者編號",
  "卡號",
  "票種類型",
  "票種次類型",
  "搭乘路線代碼",
  "資料代表日期(yyyy-MM-dd)"
)

raw_data <- fread(
  csv_path,
  select = needed_columns,
  colClasses = "character",
  encoding = "UTF-8",
  showProgress = TRUE
)

setnames(raw_data, "資料代表日期(yyyy-MM-dd)", "搭乘日期")
raw_data[, 搭乘日期 := as.IDate(搭乘日期)]

# 先限制日期，再檢查是否混入非臺東資料。
period_data <- raw_data[
  !is.na(搭乘日期) &
    搭乘日期 >= start_date &
    搭乘日期 <= end_date
]

non_taitung_n <- period_data[
  is.na(業者編號) |
    業者編號 != "1202" |
    is.na(搭乘路線代碼) |
    !grepl("^TTT", 搭乘路線代碼),
  .N
]

cat("指定期間原始資料筆數：", format(nrow(period_data), big.mark = ","), "\n")
cat("排除的非臺東資料筆數：", format(non_taitung_n, big.mark = ","), "\n")

bus_data <- period_data[
  業者編號 == "1202" &
    !is.na(搭乘路線代碼) &
    grepl("^TTT", 搭乘路線代碼)
]

if (nrow(bus_data) == 0L) {
  stop("篩選後沒有臺東市區客運資料，請檢查欄位內容及日期範圍。")
}

if (anyNA(bus_data$卡號) || any(bus_data$卡號 == "")) {
  warning("資料中有空白卡號；空白卡號不會納入乘客搭乘次數區間分析。")
}

bus_data[, 年月 := format(搭乘日期, "%Y-%m")]
bus_data[, 是否TPASS := 票種類型 == "4" & 票種次類型 == "#TTT-299"]

# 完整的25個月份，確保即使某月沒有資料仍會在圖表中顯示。
month_starts <- seq(
  from = as.Date(format(as.Date(start_date), "%Y-%m-01")),
  to = as.Date(format(as.Date(end_date), "%Y-%m-01")),
  by = "month"
)

month_info <- data.table(
  年月 = format(month_starts, "%Y-%m"),
  月份日期 = month_starts,
  月份標示 = sprintf(
    "%d/%02d",
    as.integer(format(month_starts, "%Y")) - 1911,
    as.integer(format(month_starts, "%m"))
  )
)

# 各月實際日數。
next_month_starts <- seq(
  from = month_starts[1],
  by = "month",
  length.out = length(month_starts) + 1L
)
month_info[, 當月日數 := as.integer(diff(next_month_starts))]

# 釋放不再使用的大型資料，降低記憶體占用。
rm(raw_data, period_data)
invisible(gc())

# ---------------------------
# 2. 共用函數
# ---------------------------

make_ylim <- function(y, top_ratio = 1.12) {
  y <- y[is.finite(y)]
  if (length(y) == 0L) {
    return(c(0, 1))
  }
  ymax <- max(y)
  if (ymax <= 0) {
    return(c(0, 1))
  }
  c(0, ymax * top_ratio)
}

save_png <- function(filename, plot_function) {
  png(
    filename = file.path(output_dir, filename),
    width = png_width,
    height = png_height,
    res = png_res,
    bg = "white"
  )
  on.exit(dev.off(), add = TRUE)
  plot_function()
}

make_interval_data <- function(data) {
  # 同一卡號、同一月份的資料列數，就是該乘客該月的搭乘次數。
  passenger_month <- data[
    !is.na(卡號) & 卡號 != "",
    .(搭乘次數 = .N),
    by = .(年月, 卡號)
  ]
  
  passenger_month[, 搭乘次數區間 := cut(
    搭乘次數,
    breaks = ride_breaks,
    labels = ride_labels,
    right = TRUE,
    include.lowest = TRUE,
    ordered_result = TRUE
  )]
  
  interval_count <- passenger_month[
    , .(月人數 = .N),
    by = .(年月, 搭乘次數區間)
  ]
  
  # 補齊所有月份 × 所有區間，沒有乘客的組合填0。
  complete_grid <- CJ(
    年月 = month_info$年月,
    搭乘次數區間 = factor(
      ride_labels,
      levels = ride_labels,
      ordered = TRUE
    ),
    unique = TRUE
  )
  
  interval_count <- merge(
    complete_grid,
    interval_count,
    by = c("年月", "搭乘次數區間"),
    all.x = TRUE,
    sort = FALSE
  )
  
  interval_count[is.na(月人數), 月人數 := 0L]
  interval_count[, 搭乘次數區間 := factor(
    搭乘次數區間,
    levels = ride_labels,
    ordered = TRUE
  )]
  
  setorder(interval_count, 年月, 搭乘次數區間)
  interval_count
}

make_wide_table <- function(interval_data) {
  wide_table <- dcast(
    interval_data,
    年月 ~ 搭乘次數區間,
    value.var = "月人數",
    fill = 0
  )
  
  wide_table <- merge(
    month_info[, .(年月, 月份標示)],
    wide_table,
    by = "年月",
    all.x = TRUE,
    sort = FALSE
  )
  
  setorder(wide_table, 年月)
  wide_table[, 年月 := NULL]
  setnames(wide_table, "月份標示", "年月")
  setcolorder(wide_table, c("年月", ride_labels))
  wide_table
}

plot_interval_lines <- function(wide_table, main_title) {
  y_matrix <- as.matrix(wide_table[, ..ride_labels])
  storage.mode(y_matrix) <- "numeric"
  
  n_series <- length(ride_labels)
  line_colors <- gray.colors(n_series, start = 0.15, end = 0.72)
  line_types <- seq_len(n_series)
  point_types <- 15:(14 + n_series)
  
  par(
    family = plot_family,
    mar = c(8, 6, 5, 13),
    xpd = FALSE
  )
  
  matplot(
    x = seq_len(nrow(wide_table)),
    y = y_matrix,
    type = "o",
    lty = line_types,
    lwd = 2,
    pch = point_types,
    cex = 1.1,
    col = line_colors,
    xaxt = "n",
    yaxt = "n",
    xlab = "月份",
    ylab = "月人數",
    ylim = make_ylim(y_matrix),
    bty = "n",
    cex.lab = 1.7,
    cex.main = 1.8,
    main = main_title
  )
  
  axis(
    side = 1,
    at = seq_len(nrow(wide_table)),
    labels = wide_table$年月,
    las = 2,
    cex.axis = 0.9
  )
  axis(side = 2, las = 1, cex.axis = 1.2)
  grid(col = "gray88", lty = "dotted")
  
  legend(
    "topright",
    inset = c(-0.08, 0),
    legend = ride_labels,
    col = line_colors,
    lty = line_types,
    lwd = 2,
    pch = point_types,
    bty = "n",
    xpd = TRUE,
    cex = 1.15,
    title = "每月搭乘次數"
  )
}

# ---------------------------
# 3. 圖一：平均每日使用次數折線圖
# ---------------------------

monthly_daily_mean <- bus_data[
  , .(月使用次數 = .N),
  by = 年月
]

monthly_daily_mean <- merge(
  month_info,
  monthly_daily_mean,
  by = "年月",
  all.x = TRUE,
  sort = FALSE
)

monthly_daily_mean[is.na(月使用次數), 月使用次數 := 0L]
setorder(monthly_daily_mean, 年月)
monthly_daily_mean[, 日平均使用次數 := 月使用次數 / 當月日數]

daily_mean_title <- "113年6月至115年6月臺東縣市區客運平均每日使用次數折線圖"

daily_mean_plot <- function() {
  y <- monthly_daily_mean$日平均使用次數
  
  par(
    family = plot_family,
    mar = c(8, 6, 5, 4)
  )
  
  plot(
    x = seq_len(nrow(monthly_daily_mean)),
    y = y,
    type = "o",
    lwd = 2.5,
    pch = 16,
    cex = 1.2,
    col = "gray25",
    xaxt = "n",
    yaxt = "n",
    xlab = "月份",
    ylab = "平均每日使用次數",
    ylim = make_ylim(y, top_ratio = 1.18),
    bty = "n",
    cex.lab = 1.7,
    cex.main = 1.8,
    main = daily_mean_title
  )
  
  axis(
    side = 1,
    at = seq_len(nrow(monthly_daily_mean)),
    labels = monthly_daily_mean$月份標示,
    las = 2,
    cex.axis = 0.9
  )
  axis(side = 2, las = 1, cex.axis = 1.2)
  grid(col = "gray88", lty = "dotted")
  
  text(
    x = seq_len(nrow(monthly_daily_mean)),
    y = y,
    labels = format(round(y, 1), nsmall = 1),
    pos = 3,
    offset = 0.7,
    cex = 0.78,
    col = "black"
  )
}

save_png(
  paste0(daily_mean_title, ".png"),
  daily_mean_plot
)

# ---------------------------
# 4. 圖二、表三：全體乘客搭乘次數區間
# ---------------------------

all_interval_data <- make_interval_data(bus_data)
all_interval_table <- make_wide_table(all_interval_data)

all_interval_plot_title <- "113年6月至115年6月臺東縣市區客運乘客各搭乘次數區間月人數變化折線圖"
all_interval_table_title <- "113年6月至115年6月臺東縣市區客運乘客各搭乘次數區間月人數變化表"

save_png(
  paste0(all_interval_plot_title, ".png"),
  function() plot_interval_lines(all_interval_table, all_interval_plot_title)
)

fwrite(
  all_interval_table,
  file.path(output_dir, paste0(all_interval_table_title, ".csv")),
  bom = TRUE
)

# ---------------------------
# 5. 圖四、表五：TPASS乘客搭乘次數區間
# ---------------------------

tpass_data <- bus_data[是否TPASS == TRUE]

if (nrow(tpass_data) == 0L) {
  stop("找不到TPASS資料，請確認票種類型及票種次類型的定義。")
}

tpass_interval_data <- make_interval_data(tpass_data)
tpass_interval_table <- make_wide_table(tpass_interval_data)

tpass_interval_plot_title <- "113年6月至115年6月臺東縣市區客運TPASS乘客各搭乘次數區間月人數變化折線圖"
tpass_interval_table_title <- "113年6月至115年6月臺東縣市區客運TPASS乘客各搭乘次數區間月人數變化表"

save_png(
  paste0(tpass_interval_plot_title, ".png"),
  function() plot_interval_lines(tpass_interval_table, tpass_interval_plot_title)
)

fwrite(
  tpass_interval_table,
  file.path(output_dir, paste0(tpass_interval_table_title, ".csv")),
  bom = TRUE
)

# ---------------------------
# 6. 在 RStudio 中顯示結果
# ---------------------------

cat("\n已完成輸出，資料夾位置：\n", normalizePath(output_dir), "\n\n")

cat("【平均每日使用次數資料】\n")
print(monthly_daily_mean[, .(年月 = 月份標示, 月使用次數, 當月日數, 日平均使用次數)])

cat("\n【全體乘客各搭乘次數區間月人數變化表】\n")
print(all_interval_table)

cat("\n【TPASS乘客各搭乘次數區間月人數變化表】\n")
print(tpass_interval_table)

if (interactive()) {
  daily_mean_plot()
  plot_interval_lines(all_interval_table, all_interval_plot_title)
  plot_interval_lines(tpass_interval_table, tpass_interval_plot_title)
  
  View(monthly_daily_mean)
  View(all_interval_table)
  View(tpass_interval_table)
}
