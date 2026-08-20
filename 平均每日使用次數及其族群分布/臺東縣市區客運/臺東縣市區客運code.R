# ================================================================
# 113年6月至115年6月 臺東縣市區客運圖表（最終完整版）
#
# 目前產出：
# 1. 平均每日使用次數折線圖（PNG）
# 2. 平均每日使用次數表（報告式PNG＋計算明細CSV）
# 3. 全體乘客各搭乘次數區間月人數變化折線圖（PNG）
# 4. 全體乘客各搭乘次數區間月人數及百分比變化表（報告式PNG＋CSV）
# 5. TPASS乘客各搭乘次數區間月人數變化折線圖（PNG）
# 6. TPASS乘客各搭乘次數區間月人數及百分比變化表（報告式PNG＋CSV）
# 7. 本次實際納入的市區客運路線檢查表（CSV）
#
# 統計原則：
# - 每列刷卡紀錄計為一次搭乘。
# - 乘客以匿名卡號辨識；同一卡號在同一月份的列數為該月搭乘次數。
# - 日均使用次數 = 每人每月平均使用次數 / 該月天數。
# - 每人每月平均使用次數 = 該月總使用次數 / 該月使用人數。
# - 區間百分比 = 該區間人數 / 該月總乘客人數 × 100。
# - TPASS：票種類型為4，且票種次類型為#TTT-299。
# - 臺東市區客運：業者編號1202，且搭乘路線代碼為
#   TTT0981、TTT0982、TTT0984、TTT0985之一。
# ================================================================

# 第一次執行若尚未安裝 data.table，請先執行：
# install.packages("data.table")

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("尚未安裝 data.table，請先執行 install.packages('data.table')")
}

library(data.table)

# ---------------------------
# 0. 執行設定
# ---------------------------

# 執行程式時會跳出視窗，請選擇「臺東縣公車.csv」。
csv_path <- file.choose()

# 輸出資料夾會建立在 CSV 所在的位置。
output_dir <- file.path(
  dirname(csv_path),
  "臺東縣市區客運_11306至11506_最終輸出"
)

start_date <- as.IDate("2024-06-01") # 民國113年6月1日
end_date   <- as.IDate("2026-06-30") # 民國115年6月30日

# 固定使用三個搭乘次數區間。
ride_breaks <- c(0, 1, 4, Inf)
ride_labels <- c("僅一次", "二至四次", "五次以上")

# 臺東市區客運在原始CSV中的實際系統路線代碼。
city_route_system_codes <- c("TTT0981", "TTT0982", "TTT0984", "TTT0985")

is_taitung_city_route <- function(route_code) {
  !is.na(route_code) & toupper(trimws(route_code)) %chin% city_route_system_codes
}

# 三條折線由深至淺，點型全部使用實心圓。
line_colors <- c("#333333", "#777777", "#B8B8B8")
line_point  <- 16

# 輸出圖片比例依期末報告中的橫式折線圖設定。
png_width  <- 4800
png_height <- 2100
png_res    <- 300

# 報告式表格圖片大小；高度會依資料列數自動增加。
table_png_width <- 3300

# TRUE：程式完成後自動開啟輸出資料夾；FALSE：不自動開啟。
open_output_folder_when_done <- TRUE

if (!file.exists(csv_path)) {
  stop(paste0("找不到資料檔：", csv_path, "\n請重新選擇正確的CSV檔。"))
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

# 先檢查CSV欄位，避免缺欄位時只看到難以理解的fread錯誤。
available_columns <- names(fread(
  csv_path,
  nrows = 0L,
  encoding = "UTF-8",
  showProgress = FALSE
))

missing_columns <- setdiff(needed_columns, available_columns)

if (length(missing_columns) > 0L) {
  stop(
    paste0(
      "原始CSV缺少必要欄位：\n",
      paste(missing_columns, collapse = "\n")
    )
  )
}

raw_data <- fread(
  csv_path,
  select = needed_columns,
  colClasses = "character",
  encoding = "UTF-8",
  showProgress = TRUE
)

setnames(raw_data, "資料代表日期(yyyy-MM-dd)", "搭乘日期")

# 去除代碼及卡號前後可能存在的空白，再轉換日期。
text_columns <- c(
  "業者編號",
  "卡號",
  "票種類型",
  "票種次類型",
  "搭乘路線代碼"
)

raw_data[, (text_columns) := lapply(.SD, trimws), .SDcols = text_columns]
raw_data[, 搭乘日期 := as.IDate(trimws(搭乘日期))]

# 先限制日期，再依業者與市區客運路線白名單篩選。
period_data <- raw_data[
  !is.na(搭乘日期) &
    搭乘日期 >= start_date &
    搭乘日期 <= end_date
]

non_operator_n <- period_data[
  is.na(業者編號) | 業者編號 != "1202",
  .N
]

operator_data <- period_data[業者編號 == "1202"]

excluded_route_n <- operator_data[
  !is_taitung_city_route(搭乘路線代碼),
  .N
]

bus_data <- operator_data[
  is_taitung_city_route(搭乘路線代碼)
]

cat("指定期間原始資料筆數：", format(nrow(period_data), big.mark = ","), "\n")
cat("排除的非1202業者資料筆數：", format(non_operator_n, big.mark = ","), "\n")
cat(
  "1202業者中排除的非指定市區路線筆數：",
  format(excluded_route_n, big.mark = ","),
  "\n"
)

if (nrow(bus_data) == 0L) {
  observed_routes <- operator_data[
    !is.na(搭乘路線代碼) & trimws(搭乘路線代碼) != "",
    sort(unique(搭乘路線代碼))
  ]

  stop(
    paste0(
      "篩選後沒有指定的臺東市區客運資料。\n",
      "程式要找的系統代碼：TTT0981、TTT0982、TTT0984、TTT0985。\n",
      "資料中業者1202的實際路線代碼如下：\n",
      paste(observed_routes, collapse = "\n")
    )
  )
}

# 列出實際納入的路線及筆數，供執行後人工核對。
included_route_summary <- bus_data[
  , .(刷卡筆數 = .N),
  by = 搭乘路線代碼
][order(搭乘路線代碼)]

cat("\n【本次納入的臺東市區客運路線】\n")
print(included_route_summary)

fwrite(
  included_route_summary,
  file.path(output_dir, "本次納入的臺東市區客運路線檢查表.csv"),
  bom = TRUE
)

# 日均使用次數與乘客區間都需要辨識乘客，因此空白卡號不納入分析。
blank_card_n <- bus_data[is.na(卡號) | 卡號 == "", .N]

if (blank_card_n > 0L) {
  warning(
    paste0(
      "共有 ", format(blank_card_n, big.mark = ","),
      " 筆空白卡號；因無法辨識乘客，已排除於所有每人及乘客區間分析。"
    )
  )
}

analysis_data <- bus_data[!is.na(卡號) & 卡號 != ""]

if (nrow(analysis_data) == 0L) {
  stop("排除空白卡號後沒有可分析資料。")
}

analysis_data[, 年月 := format(搭乘日期, "%Y-%m")]
analysis_data[, 是否TPASS :=
  !is.na(票種類型) &
  !is.na(票種次類型) &
  票種類型 == "4" &
  票種次類型 == "#TTT-299"
]

# 完整的25個月份，確保即使某月沒有資料仍會在圖表中顯示。
month_starts <- seq(
  from = as.Date(format(as.Date(start_date), "%Y-%m-01")),
  to = as.Date(format(as.Date(end_date), "%Y-%m-01")),
  by = "month"
)

month_info <- data.table(
  年月 = format(month_starts, "%Y-%m"),
  月份日期 = month_starts,
  # 圖表橫軸依期末報告格式顯示為11306、11307……。
  月份標示 = sprintf(
    "%d%02d",
    as.integer(format(month_starts, "%Y")) - 1911,
    as.integer(format(month_starts, "%m"))
  ),
  # 報告表格依期末報告格式顯示為「113 年 6 月」。
  表格月份標示 = sprintf(
    "%d 年 %d 月",
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
rm(raw_data, period_data, operator_data, bus_data)
invisible(gc())

# ---------------------------
# 2. 共用函數
# ---------------------------

make_ylim <- function(y, top_ratio = 1.10) {
  y <- y[is.finite(y)]
  if (length(y) == 0L || max(y) <= 0) {
    return(c(0, 1))
  }
  c(0, max(y) * top_ratio)
}

# 平均每日使用次數依期末報告採局部範圍，避免從0開始造成折線過度壓縮。
make_focused_ylim <- function(y, padding_ratio = 0.15) {
  y <- y[is.finite(y)]
  if (length(y) == 0L) {
    return(c(0, 1))
  }

  y_range <- range(y)
  y_span <- diff(y_range)

  if (y_span <= 0) {
    padding <- max(abs(y_range[1]) * 0.10, 0.001)
  } else {
    padding <- y_span * padding_ratio
  }

  c(max(0, y_range[1] - padding), y_range[2] + padding)
}

make_y_ticks <- function(y_limits, n = 6L) {
  ticks <- pretty(y_limits, n = n)
  ticks[ticks >= y_limits[1] & ticks <= y_limits[2]]
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

# 畫出期末報告格式：每月保留網格，但橫軸每隔一個月才顯示月份文字。
draw_grid_and_labels <- function(x_labels, y_ticks, y_digits = NULL) {
  x_at <- seq_along(x_labels)
  x_label_at <- seq(1L, length(x_labels), by = 2L)
  displayed_x_labels <- rep("", length(x_labels))
  displayed_x_labels[x_label_at] <- x_labels[x_label_at]

  abline(
    v = x_at,
    h = y_ticks,
    col = "gray88",
    lty = "solid",
    lwd = 0.8
  )

  axis(
    side = 1,
    at = x_at,
    labels = displayed_x_labels,
    las = 1,
    tick = TRUE,
    lwd = 0.8,
    lwd.ticks = 0.8,
    col.axis = "black",
    cex.axis = 0.76,
    line = 0
  )

  if (is.null(y_digits)) {
    y_labels <- format(
      y_ticks,
      big.mark = ",",
      trim = TRUE,
      scientific = FALSE
    )
  } else {
    y_labels <- format(
      round(y_ticks, y_digits),
      trim = TRUE,
      scientific = FALSE
    )
  }

  axis(
    side = 2,
    at = y_ticks,
    labels = y_labels,
    las = 1,
    tick = TRUE,
    lwd = 0.8,
    lwd.ticks = 0.8,
    col.axis = "black",
    cex.axis = 0.90
  )
}

make_interval_data <- function(data) {
  # 同一卡號、同一月份的資料列數，就是該乘客該月的搭乘次數。
  passenger_month <- data[
    , .(搭乘次數 = .N),
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
    搭乘次數區間 = ride_labels,
    unique = TRUE
  )

  complete_grid[, 搭乘次數區間 := factor(
    搭乘次數區間,
    levels = ride_labels,
    ordered = TRUE
  )]

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

  # 百分比的分母是該月全部可辨識乘客人數。
  interval_count[, 當月總乘客人數 := sum(月人數), by = 年月]
  interval_count[, 百分比 := fifelse(
    當月總乘客人數 > 0,
    月人數 / 當月總乘客人數 * 100,
    0
  )]

  setorder(interval_count, 年月, 搭乘次數區間)
  interval_count
}

# 折線圖使用的人數寬表。
make_count_wide_table <- function(interval_data) {
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

  for (column_name in ride_labels) {
    set(wide_table, which(is.na(wide_table[[column_name]])), column_name, 0L)
  }

  setorder(wide_table, 年月)
  wide_table
}

# CSV及報告式表格PNG使用的人數＋百分比寬表。
make_report_table <- function(interval_data) {
  count_wide <- dcast(
    interval_data,
    年月 ~ 搭乘次數區間,
    value.var = "月人數",
    fill = 0
  )

  percent_wide <- dcast(
    interval_data,
    年月 ~ 搭乘次數區間,
    value.var = "百分比",
    fill = 0
  )

  count_names <- paste0(ride_labels, "人數")
  percent_names <- paste0(ride_labels, "百分比(%)")

  setnames(count_wide, ride_labels, count_names)
  setnames(percent_wide, ride_labels, percent_names)

  report_table <- merge(
    month_info[, .(年月, 表格月份標示)],
    count_wide,
    by = "年月",
    all.x = TRUE,
    sort = FALSE
  )

  report_table <- merge(
    report_table,
    percent_wide,
    by = "年月",
    all.x = TRUE,
    sort = FALSE
  )

  for (column_name in count_names) {
    set(report_table, which(is.na(report_table[[column_name]])), column_name, 0L)
  }

  for (column_name in percent_names) {
    set(report_table, which(is.na(report_table[[column_name]])), column_name, 0)
  }

  # 期末報告表格的百分比顯示至小數點後一位，並保留百分比符號。
  report_table[, (percent_names) := lapply(
    .SD,
    function(x) sprintf("%.1f%%", x)
  ), .SDcols = percent_names]

  setorder(report_table, 年月)
  report_table[, 年月 := NULL]
  setnames(report_table, "表格月份標示", "月份")

  interleaved_columns <- as.vector(rbind(count_names, percent_names))
  setcolorder(
    report_table,
    c("月份", interleaved_columns)
  )

  report_table
}

# 輸出期末報告格式的區間表格PNG：左上斜線表頭、區間合併欄位，
# 人數與百分比不另加第二層欄名。
save_interval_table_png <- function(report_table, filename, table_title) {
  table_height <- max(3000, 720 + nrow(report_table) * 105)

  png(
    filename = file.path(output_dir, filename),
    width = table_png_width,
    height = table_height,
    res = png_res,
    bg = "white"
  )
  on.exit(dev.off(), add = TRUE)

  par(
    family = plot_family,
    mar = c(1.2, 1.2, 5.2, 1.2),
    xpd = NA
  )

  # 月份欄較寬；其餘每個區間仍各占「人數、百分比」兩個資料欄。
  column_widths <- c(1.55, 0.92, 0.82, 0.92, 0.82, 0.92, 0.82)
  x_edges <- c(0, cumsum(column_widths))
  data_rows <- nrow(report_table)
  header_height <- 1.8
  y_top <- data_rows + header_height

  plot.new()
  plot.window(
    xlim = c(0, max(x_edges)),
    ylim = c(0, y_top),
    xaxs = "i",
    yaxs = "i"
  )

  # 左上角依PDF使用斜線表頭：右上為「搭乘次數」、左下為「月份」。
  rect(x_edges[1], data_rows, x_edges[2], y_top, border = "black", lwd = 1.0)
  segments(
    x0 = x_edges[1],
    y0 = y_top,
    x1 = x_edges[2],
    y1 = data_rows,
    col = "black",
    lwd = 1.0
  )
  text(
    x = x_edges[1] + column_widths[1] * 0.72,
    y = data_rows + header_height * 0.72,
    labels = "搭乘次數",
    cex = 1.0
  )
  text(
    x = x_edges[1] + column_widths[1] * 0.08,
    y = data_rows + header_height * 0.25,
    labels = "月份",
    adj = c(0, 0.5),
    cex = 1.0
  )

  # 三個搭乘區間表頭各跨「人數＋百分比」兩欄。
  for (group_index in seq_along(ride_labels)) {
    left_index <- 2 + (group_index - 1L) * 2L
    rect(
      x_edges[left_index],
      data_rows,
      x_edges[left_index + 2L],
      y_top,
      border = "black",
      lwd = 1.0
    )
    text(
      mean(c(x_edges[left_index], x_edges[left_index + 2L])),
      data_rows + header_height / 2,
      ride_labels[group_index],
      cex = 1.05,
      font = 1
    )
  }

  count_names <- paste0(ride_labels, "人數")
  percent_names <- paste0(ride_labels, "百分比(%)")

  # 資料列由上往下繪製。
  for (row_index in seq_len(data_rows)) {
    y_bottom <- data_rows - row_index
    y_center <- y_bottom + 0.5

    for (column_index in seq_len(length(column_widths))) {
      rect(
        x_edges[column_index],
        y_bottom,
        x_edges[column_index + 1L],
        y_bottom + 1,
        border = "black",
        lwd = 0.75
      )
    }

    text(
      x_edges[1] + column_widths[1] * 0.06,
      y_center,
      report_table$月份[row_index],
      adj = c(0, 0.5),
      cex = 0.88
    )

    for (group_index in seq_along(ride_labels)) {
      left_index <- 2 + (group_index - 1L) * 2L
      count_text <- format(
        report_table[[count_names[group_index]]][row_index],
        big.mark = ",",
        scientific = FALSE,
        trim = TRUE
      )
      percent_text <- report_table[[percent_names[group_index]]][row_index]

      text(
        x_edges[left_index + 1L] - column_widths[left_index] * 0.06,
        y_center,
        count_text,
        adj = c(1, 0.5),
        cex = 0.88
      )
      text(
        x_edges[left_index + 2L] - column_widths[left_index + 1L] * 0.06,
        y_center,
        percent_text,
        adj = c(1, 0.5),
        cex = 0.88
      )
    }
  }

  mtext(
    table_title,
    side = 3,
    line = 2.0,
    adj = 0.5,
    cex = 1.15,
    font = 1
  )
}

# 輸出平均每日使用次數的兩欄報告式表格PNG。
save_daily_table_png <- function(daily_table, filename, table_title) {
  table_height <- max(3000, 620 + nrow(daily_table) * 105)

  png(
    filename = file.path(output_dir, filename),
    width = 2100,
    height = table_height,
    res = png_res,
    bg = "white"
  )
  on.exit(dev.off(), add = TRUE)

  par(
    family = plot_family,
    mar = c(1.2, 1.2, 5.2, 1.2),
    xpd = NA
  )

  column_widths <- c(1.5, 1.45)
  x_edges <- c(0, cumsum(column_widths))
  data_rows <- nrow(daily_table)
  y_top <- data_rows + 1L

  plot.new()
  plot.window(
    xlim = c(0, max(x_edges)),
    ylim = c(0, y_top),
    xaxs = "i",
    yaxs = "i"
  )

  header_labels <- c("月份", "日均使用次數")
  for (column_index in seq_along(column_widths)) {
    rect(
      x_edges[column_index],
      data_rows,
      x_edges[column_index + 1L],
      y_top,
      border = "black",
      lwd = 1.0
    )
    text(
      mean(x_edges[c(column_index, column_index + 1L)]),
      data_rows + 0.5,
      header_labels[column_index],
      cex = 1.0,
      font = 1
    )
  }

  for (row_index in seq_len(data_rows)) {
    y_bottom <- data_rows - row_index
    y_center <- y_bottom + 0.5

    for (column_index in seq_along(column_widths)) {
      rect(
        x_edges[column_index],
        y_bottom,
        x_edges[column_index + 1L],
        y_bottom + 1,
        border = "black",
        lwd = 0.75
      )
    }

    text(
      x_edges[1] + column_widths[1] * 0.07,
      y_center,
      daily_table$月份[row_index],
      adj = c(0, 0.5),
      cex = 0.9
    )
    text(
      x_edges[3] - column_widths[2] * 0.07,
      y_center,
      sprintf("%.3f", daily_table$日均使用次數[row_index]),
      adj = c(1, 0.5),
      cex = 0.9
    )
  }

  mtext(
    table_title,
    side = 3,
    line = 2.0,
    adj = 0.5,
    cex = 1.1,
    font = 1
  )
}

plot_interval_lines <- function(wide_table, main_title) {
  y_matrix <- as.matrix(wide_table[, ..ride_labels])
  storage.mode(y_matrix) <- "numeric"

  x_at <- seq_len(nrow(wide_table))
  y_limits <- make_ylim(y_matrix, top_ratio = 1.12)
  y_ticks <- make_y_ticks(y_limits)

  par(
    family = plot_family,
    # 加大右側留白，確保「搭乘次數」圖例完整顯示。
    mar = c(5.0, 5.5, 4.5, 14),
    xpd = NA
  )

  plot(
    x = x_at,
    y = y_matrix[, 1],
    type = "n",
    xlim = c(1, nrow(wide_table)),
    ylim = y_limits,
    xaxs = "i",
    yaxs = "i",
    axes = FALSE,
    ann = FALSE,
    bty = "n"
  )

  draw_grid_and_labels(
    x_labels = wide_table$月份標示,
    y_ticks = y_ticks
  )

  for (series_index in seq_along(ride_labels)) {
    lines(
      x = x_at,
      y = y_matrix[, series_index],
      type = "o",
      lty = 1,
      lwd = 1.8,
      pch = line_point,
      cex = 0.82,
      col = line_colors[series_index]
    )
  }

  mtext(
    main_title,
    side = 3,
    line = 1.5,
    adj = 0,
    cex = 1.35,
    font = 2
  )
  mtext("月份", side = 1, line = 3.0, cex = 1.15)
  mtext("人數", side = 2, line = 3.3, cex = 1.15)

  plot_region <- par("usr")

  legend(
    # 將圖例左緣固定在繪圖區右界之外，完整置於右側留白。
    x = plot_region[2] + diff(plot_region[1:2]) * 0.04,
    y = plot_region[4],
    legend = ride_labels,
    col = line_colors,
    lty = 1,
    lwd = 1.8,
    pch = line_point,
    pt.cex = 0.82,
    bty = "n",
    xpd = NA,
    cex = 0.92,
    title = "搭乘次數"
  )
}

# ---------------------------
# 3. 圖一：平均每日使用次數折線圖
# ---------------------------

# 每人每月平均使用次數 = 該月總使用次數 / 該月使用人數。
# 日均使用次數 = 每人每月平均使用次數 / 該月天數。
monthly_daily_mean <- analysis_data[
  , .(
    月總使用次數 = .N,
    當月使用人數 = uniqueN(卡號)
  ),
  by = 年月
]

monthly_daily_mean <- merge(
  month_info,
  monthly_daily_mean,
  by = "年月",
  all.x = TRUE,
  sort = FALSE
)

monthly_daily_mean[is.na(月總使用次數), 月總使用次數 := 0L]
monthly_daily_mean[is.na(當月使用人數), 當月使用人數 := 0L]
setorder(monthly_daily_mean, 年月)

monthly_daily_mean[, 每人每月平均使用次數 := fifelse(
  當月使用人數 > 0,
  月總使用次數 / 當月使用人數,
  0
)]

monthly_daily_mean[, 日均使用次數 :=
  每人每月平均使用次數 / 當月日數
]

daily_mean_title <- "113年6月至115年6月臺東縣市區客運平均每日使用次數折線圖"

daily_mean_plot <- function() {
  x_at <- seq_len(nrow(monthly_daily_mean))
  y <- monthly_daily_mean$日均使用次數
  y_limits <- make_focused_ylim(y, padding_ratio = 0.15)
  y_ticks <- make_y_ticks(y_limits)

  par(
    family = plot_family,
    mar = c(5.0, 5.5, 4.5, 10),
    xpd = NA
  )

  plot(
    x = x_at,
    y = y,
    type = "n",
    xlim = c(1, nrow(monthly_daily_mean)),
    ylim = y_limits,
    xaxs = "i",
    yaxs = "i",
    axes = FALSE,
    ann = FALSE,
    bty = "n"
  )

  draw_grid_and_labels(
    x_labels = monthly_daily_mean$月份標示,
    y_ticks = y_ticks,
    y_digits = 3
  )

  lines(
    x = x_at,
    y = y,
    type = "o",
    lty = 1,
    lwd = 1.8,
    pch = line_point,
    cex = 0.82,
    col = line_colors[1]
  )

  # 標題從繪圖區左緣開始，與縱向網格起點對齊。
  mtext(
    daily_mean_title,
    side = 3,
    line = 1.5,
    adj = 0,
    cex = 1.35,
    font = 2
  )
  mtext("月份", side = 1, line = 3.0, cex = 1.15)
  mtext("日均使用次數", side = 2, line = 3.3, cex = 1.15)

  legend(
    "topright",
    inset = c(-0.15, 0),
    legend = "日均使用次數",
    col = line_colors[1],
    lty = 1,
    lwd = 1.8,
    pch = line_point,
    pt.cex = 0.82,
    bty = "n",
    xpd = NA,
    cex = 0.92
  )
}

save_png(
  paste0(daily_mean_title, ".png"),
  daily_mean_plot
)

# 平均每日使用次數對應表格：CSV保留完整計算欄位，PNG呈現報告用兩欄表。
daily_mean_table_title <- "113年6月至115年6月臺東縣市區客運平均每日使用次數表"

daily_mean_report_table <- monthly_daily_mean[
  , .(
    月份 = 表格月份標示,
    月總使用次數,
    當月使用人數,
    當月日數,
    每人每月平均使用次數 = round(每人每月平均使用次數, 3),
    日均使用次數 = round(日均使用次數, 3)
  )
]

fwrite(
  daily_mean_report_table,
  file.path(output_dir, paste0(daily_mean_table_title, ".csv")),
  bom = TRUE
)

save_daily_table_png(
  daily_table = daily_mean_report_table,
  filename = paste0(daily_mean_table_title, ".png"),
  table_title = daily_mean_table_title
)

# ---------------------------
# 4. 全體乘客搭乘次數區間圖表
# ---------------------------

all_interval_data <- make_interval_data(analysis_data)
all_interval_count_table <- make_count_wide_table(all_interval_data)
all_interval_report_table <- make_report_table(all_interval_data)

all_interval_plot_title <- "113年6月至115年6月臺東縣市區客運乘客各搭乘次數區間月人數變化折線圖"
all_interval_table_title <- "113年6月至115年6月臺東縣市區客運乘客各搭乘次數區間月人數變化表"

save_png(
  paste0(all_interval_plot_title, ".png"),
  function() plot_interval_lines(
    all_interval_count_table,
    all_interval_plot_title
  )
)

fwrite(
  all_interval_report_table,
  file.path(output_dir, paste0(all_interval_table_title, ".csv")),
  bom = TRUE
)

save_interval_table_png(
  report_table = all_interval_report_table,
  filename = paste0(all_interval_table_title, ".png"),
  table_title = all_interval_table_title
)

# ---------------------------
# 5. TPASS乘客搭乘次數區間圖表
# ---------------------------

tpass_data <- analysis_data[是否TPASS == TRUE]

if (nrow(tpass_data) == 0L) {
  stop("找不到TPASS資料，請確認票種類型及票種次類型的定義。")
}

tpass_interval_data <- make_interval_data(tpass_data)
tpass_interval_count_table <- make_count_wide_table(tpass_interval_data)
tpass_interval_report_table <- make_report_table(tpass_interval_data)

tpass_interval_plot_title <- "113年6月至115年6月臺東縣市區客運TPASS乘客各搭乘次數區間月人數變化折線圖"
tpass_interval_table_title <- "113年6月至115年6月臺東縣市區客運TPASS乘客各搭乘次數區間月人數變化表"

save_png(
  paste0(tpass_interval_plot_title, ".png"),
  function() plot_interval_lines(
    tpass_interval_count_table,
    tpass_interval_plot_title
  )
)

fwrite(
  tpass_interval_report_table,
  file.path(output_dir, paste0(tpass_interval_table_title, ".csv")),
  bom = TRUE
)

save_interval_table_png(
  report_table = tpass_interval_report_table,
  filename = paste0(tpass_interval_table_title, ".png"),
  table_title = tpass_interval_table_title
)

# ---------------------------
# 6. 在 RStudio 中顯示結果
# ---------------------------

cat("\n已完成輸出，資料夾位置：\n", normalizePath(output_dir), "\n\n")

cat("【平均每日使用次數資料】\n")
print(daily_mean_report_table)

cat("\n【全體乘客各搭乘次數區間月人數及百分比變化表】\n")
print(all_interval_report_table)

cat("\n【TPASS乘客各搭乘次數區間月人數及百分比變化表】\n")
print(tpass_interval_report_table)

if (interactive()) {
  # 圖檔已在前面輸出；此處只開啟資料表，避免RStudio小型Plots視窗報錯。
  View(daily_mean_report_table)
  View(all_interval_report_table)
  View(tpass_interval_report_table)
}

# Windows版RStudio執行完成後，直接開啟成果資料夾。
if (
  interactive() &&
    isTRUE(open_output_folder_when_done) &&
    .Platform$OS.type == "windows"
) {
  shell.exec(normalizePath(output_dir))
}
