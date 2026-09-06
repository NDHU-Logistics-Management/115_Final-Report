# ================================================================
# 臺東縣市區客運與公路客運合併分析程式
# 分析期間：民國113年6月至115年6月
#
# execute_city_bus／execute_road_bus 可控制要執行的部分。
# 預設兩者皆為 TRUE，依序執行市區客運及公路客運。
# 兩份程式分別封裝成函數，變數與共用函數不會互相覆蓋。
# ================================================================

execute_city_bus <- TRUE
execute_road_bus <- TRUE

run_city_bus_analysis <- function() {
# ----------------------------------------------------------------
# A. 臺東縣市區客運完整程式
# 來源：outputs/20260828_taitung_city_clean_delivery/臺東縣市區客運code.R
# ----------------------------------------------------------------
# 臺東縣市區客運（113年6月至115年6月）資料清理、指標計算與圖表完整分析

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
  "臺東縣市區客運_11306至11506_清理後完整輸出"
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

# 輸出圖片尺寸依期末報告設定（英吋）。
png_width  <- 15
png_height <- 5
png_res    <- 300

# 報告式表格圖片大小；高度會依資料列數自動增加。
table_png_width <- 3300

# TRUE：程式完成後自動開啟輸出資料夾；FALSE：不自動開啟。
open_output_folder_when_done <- TRUE

# 資料品質處理模式：
# exclude_flagged_anomalies：主分析只排除CSV明確標記的異常值。
# strict_validated：僅保留異常旗標為0、檢核錯誤代碼為0、檢核結果為3的紀錄。
# 預設採前者，避免把僅有站牌／站位檢核問題的搭乘一併刪除。
quality_filter_mode <- "exclude_flagged_anomalies"
quality_filter_mode <- match.arg(
  quality_filter_mode,
  c("exclude_flagged_anomalies", "strict_validated")
)

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

anomaly_flag_column <- paste0(
  "是否包含異常值(係指上下站值為-99或上下車時間為",
  "1970-01-01 00:00:00之情況)"
)
validation_error_column <- paste0(
  "檢核錯誤代碼[-2: 未經二階段檢核; -1: 路線資訊錯誤; ",
  "0: 驗證通過; 1: 站牌或站位資訊錯誤; ",
  "2: 業者與路線資訊不匹配; 3: 路線、站牌與業者資訊不匹配]"
)
validation_result_column <- paste0(
  "檢核結果[0: 值域或各式檢核未通過; ",
  "1: 值域各式檢核通過但包含異常值; ",
  "2: 代碼或邏輯檢核未通過; ",
  "3: 值域格式及代碼邏輯皆通過]"
)

needed_columns <- c(
  "業者編號",
  "卡號",
  "票種類型",
  "票種次類型",
  "搭乘路線代碼",
  anomaly_flag_column,
  validation_error_column,
  validation_result_column,
  "原始票證筆數",
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
setnames(
  raw_data,
  old = c(
    anomaly_flag_column,
    validation_error_column,
    validation_result_column
  ),
  new = c("異常值旗標", "檢核錯誤代碼", "檢核結果")
)

# 去除代碼、卡號及品質欄位前後可能存在的空白，再轉換日期。
text_columns <- c(
  "業者編號",
  "卡號",
  "票種類型",
  "票種次類型",
  "搭乘路線代碼",
  "異常值旗標",
  "檢核錯誤代碼",
  "檢核結果",
  "原始票證筆數"
)

raw_data[, (text_columns) := lapply(.SD, trimws), .SDcols = text_columns]

parse_original_ticket_count <- function(x) {
  cleaned <- gsub(",", "", trimws(fcoalesce(as.character(x), "")), fixed = TRUE)
  value <- suppressWarnings(as.numeric(cleaned))
  invalid <- cleaned != "" & is.na(value)

  if (any(invalid)) {
    examples <- head(unique(cleaned[invalid]), 5L)
    stop(
      "原始票證筆數含有無法轉為數字的內容，例如：",
      paste(examples, collapse = "、")
    )
  }
  if (any(value < 0, na.rm = TRUE)) {
    stop("原始票證筆數不可為負數。")
  }
  if (any(abs(value - round(value)) > 1e-8, na.rm = TRUE)) {
    stop("原始票證筆數必須是整數。")
  }

  value
}

raw_data[, 原始票證筆數 := parse_original_ticket_count(原始票證筆數)]
missing_original_count_n <- raw_data[is.na(原始票證筆數), .N]
if (missing_original_count_n > 0L) {
  warning(
    "原始票證筆數有 ",
    format(missing_original_count_n, big.mark = ","),
    " 筆空白值；計算加總時將依 na.rm = TRUE 忽略。"
  )
}

raw_data[, 搭乘日期 := as.IDate(trimws(搭乘日期))]

if (raw_data[!is.na(搭乘日期), .N] == 0L) {
  stop("日期欄位沒有可辨識的有效日期，請檢查CSV的日期格式。")
}

source_date_range <- raw_data[
  !is.na(搭乘日期),
  range(搭乘日期)
]

cat(
  "CSV有效日期範圍：",
  format(source_date_range[1]),
  "至",
  format(source_date_range[2]),
  "\n"
)
cat(
  "本次設定分析期間：",
  format(start_date),
  "至",
  format(end_date),
  "\n"
)

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

# ---------------------------
# 1-1. 資料品質清理與稽核
# ---------------------------
# 先保存已鎖定日期、業者與路線的完整母體，再依品質模式建立分析資料。
# 原始票證筆數除用於稽核外，也作為使用次數相關指標的加總權重。
scope_data <- copy(bus_data)

scope_data[, 明確異常值 := (
  !is.na(異常值旗標) & 異常值旗標 == "1"
)]
scope_data[, 完整驗證通過 := (
  !is.na(異常值旗標) & 異常值旗標 == "0" &
    !is.na(檢核錯誤代碼) & 檢核錯誤代碼 == "0" &
    !is.na(檢核結果) & 檢核結果 == "3"
)]
scope_data[, 清理狀態 := fcase(
  明確異常值,
  "明確異常值",
  完整驗證通過,
  "完整驗證通過",
  is.na(異常值旗標) | !異常值旗標 %chin% c("0", "1"),
  "異常旗標缺漏或非0/1",
  default = "其他檢核疑慮"
)]
scope_data[, 清理年月 := format(搭乘日期, "%Y-%m")]

quality_audit_month <- scope_data[
  ,
  .(
    範圍母體筆數 = .N,
    明確異常筆數 = sum(明確異常值),
    排除明確異常後筆數 = sum(異常值旗標 == "0", na.rm = TRUE),
    完整驗證通過筆數 = sum(完整驗證通過)
  ),
  by = 清理年月
][order(清理年月)]

quality_audit_month[, 明確異常排除比例 :=
  明確異常筆數 / 範圍母體筆數
]
quality_audit_month[, 嚴格驗證排除比例 :=
  1 - 完整驗證通過筆數 / 範圍母體筆數
]

quality_audit_reason <- scope_data[
  ,
  .(筆數 = .N),
  by = .(
    清理年月,
    搭乘路線代碼,
    清理狀態,
    異常值旗標,
    檢核錯誤代碼,
    檢核結果
  )
][order(清理年月, 搭乘路線代碼, 清理狀態)]

original_count_audit <- scope_data[
  ,
  .(筆數 = .N),
  by = .(清理年月, 原始票證筆數, 清理狀態)
][order(清理年月, 原始票證筆數, 清理狀態)]

fwrite(
  quality_audit_month,
  file.path(output_dir, "資料清理逐月稽核表.csv"),
  bom = TRUE
)
fwrite(
  quality_audit_reason,
  file.path(output_dir, "資料清理原因稽核表.csv"),
  bom = TRUE
)
fwrite(
  original_count_audit,
  file.path(output_dir, "原始票證筆數稽核表.csv"),
  bom = TRUE
)

if (quality_filter_mode == "exclude_flagged_anomalies") {
  bus_data <- scope_data[
    !is.na(異常值旗標) & 異常值旗標 == "0"
  ]
  selected_filter_label <- "排除CSV明確標記的異常值"
} else {
  bus_data <- scope_data[完整驗證通過]
  selected_filter_label <- "僅保留完整驗證通過紀錄"
}

cat("\n【資料品質清理】\n")
cat("清理模式：", selected_filter_label, "\n")
cat(
  "鎖定日期、業者及路線後的母體筆數：",
  format(nrow(scope_data), big.mark = ","),
  "\n"
)
cat(
  "清理後納入分析筆數：",
  format(nrow(bus_data), big.mark = ","),
  "\n"
)
cat(
  "清理排除筆數：",
  format(nrow(scope_data) - nrow(bus_data), big.mark = ","),
  "\n"
)
cat("使用次數相關指標以原始票證筆數加總計算。\n")

strict_high_impact_months <- quality_audit_month[
  嚴格驗證排除比例 >= 0.20,
  清理年月
]
if (length(strict_high_impact_months) > 0L) {
  warning(
    paste0(
      "若改採strict_validated，以下月份會排除20%以上資料：",
      paste(strict_high_impact_months, collapse = "、"),
      "。請先核對檢核規則是否與其他運具一致。"
    )
  )
}

# 列出清理後實際納入的路線及筆數，供執行後人工核對。
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

# 日期範圍應完整涵蓋113年6月至115年6月，共25個月份。
if (
  nrow(month_info) != 25L ||
    month_info$年月[1] != "2024-06" ||
    month_info$年月[nrow(month_info)] != "2026-06"
) {
  stop("月份設定錯誤：應為2024-06至2026-06，共25個月份。")
}

analysis_date_range <- analysis_data[, range(搭乘日期)]
observed_months <- sort(unique(analysis_data$年月))
missing_months <- setdiff(month_info$年月, observed_months)

cat(
  "篩選後實際資料日期：",
  format(analysis_date_range[1]),
  "至",
  format(analysis_date_range[2]),
  "\n"
)
cat("圖表月份數：", nrow(month_info), "（應為25）\n")

if (length(missing_months) > 0L) {
  missing_month_labels <- month_info[
    年月 %chin% missing_months,
    表格月份標示
  ]
  warning(
    paste0(
      "下列月份沒有符合條件的市區客運資料，圖表將以0呈現：",
      paste(missing_month_labels, collapse = "、")
    )
  )
}

# 釋放不再使用的大型資料，降低記憶體占用。
rm(raw_data, period_data, operator_data, bus_data, scope_data)
invisible(gc())

# ---------------------------
# 2. 共用函數
# ---------------------------

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
    units = "in",
    res = png_res,
    bg = "white"
  )
  on.exit(dev.off(), add = TRUE)
  plot_function()
}

# 畫出期末報告格式：橫軸每隔一個月顯示月份文字，網格依圖別設定。
draw_grid_and_labels <- function(
    x_labels,
    y_ticks,
    y_digits = NULL,
    vertical_grid_mode = c("monthly", "major"),
    grid_lty = "solid",
    show_zero_y = TRUE,
    show_axis_lines = TRUE,
    grid_inset_ratio = 0
) {
  vertical_grid_mode <- match.arg(vertical_grid_mode)
  x_at <- seq_along(x_labels)
  x_label_at <- seq(1L, length(x_labels), by = 2L)
  displayed_x_labels <- rep("", length(x_labels))
  displayed_x_labels[x_label_at] <- x_labels[x_label_at]

  if (vertical_grid_mode == "monthly") {
    vertical_grid_at <- x_at
  } else {
    x_limits <- par("usr")[1:2]
    vertical_grid_at <- pretty(x_limits, n = 5L)
    vertical_grid_at <- vertical_grid_at[
      vertical_grid_at > x_limits[1] &
        vertical_grid_at <= x_limits[2]
    ]
  }

  displayed_y_ticks <- if (show_zero_y) {
    y_ticks
  } else {
    y_ticks[y_ticks > 0]
  }

  # 網格只能畫在繪圖區內；右側留白僅供圖例使用。
  original_xpd <- par("xpd")
  par(xpd = FALSE)

  plot_region <- par("usr")
  x_grid_inset <- diff(plot_region[1:2]) * grid_inset_ratio
  y_grid_inset <- diff(plot_region[3:4]) * grid_inset_ratio

  segments(
    x0 = vertical_grid_at,
    y0 = plot_region[3] + y_grid_inset,
    x1 = vertical_grid_at,
    y1 = plot_region[4] - y_grid_inset,
    col = "gray88",
    lty = grid_lty,
    lwd = 0.8
  )

  segments(
    x0 = plot_region[1] + x_grid_inset,
    y0 = displayed_y_ticks,
    x1 = plot_region[2] - x_grid_inset,
    y1 = displayed_y_ticks,
    col = "gray88",
    lty = grid_lty,
    lwd = 0.8
  )

  par(xpd = original_xpd)

  axis(
    side = 1,
    at = x_at,
    labels = displayed_x_labels,
    las = 1,
    tick = show_axis_lines,
    lwd = 0.8,
    lwd.ticks = 0.8,
    col = if (show_axis_lines) "black" else NA,
    col.axis = "black",
    cex.axis = 0.76,
    line = 0
  )

  if (is.null(y_digits)) {
    y_labels <- format(
      displayed_y_ticks,
      big.mark = ",",
      trim = TRUE,
      scientific = FALSE
    )
  } else {
    y_labels <- format(
      round(displayed_y_ticks, y_digits),
      trim = TRUE,
      scientific = FALSE
    )
  }

  axis(
    side = 2,
    at = displayed_y_ticks,
    labels = y_labels,
    las = 1,
    tick = show_axis_lines,
    lwd = 0.8,
    lwd.ticks = 0.8,
    col = if (show_axis_lines) "black" else NA,
    col.axis = "black",
    cex.axis = 0.90
  )
}

make_interval_data <- function(data) {
  # 同一卡號、同一月份的原始票證筆數加總，就是該乘客該月的搭乘次數。
  passenger_month <- data[
    , .(搭乘次數 = sum(原始票證筆數, na.rm = TRUE)),
    by = .(年月, 卡號)
  ]

  passenger_month[, 搭乘次數區間 := cut(
    搭乘次數,
    breaks = ride_breaks,
    labels = ride_labels,
    right = TRUE,
    include.lowest = FALSE,
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

plot_interval_lines <- function(wide_table, main_title, y_tick_step) {
  y_matrix <- as.matrix(wide_table[, ..ride_labels])
  storage.mode(y_matrix) <- "numeric"

  x_at <- seq_len(nrow(wide_table))
  y_max <- max(y_matrix, na.rm = TRUE)

  # 依圖別固定縱軸刻度：全體乘客每1,000人、TPASS每50人。
  y_upper <- ceiling(y_max / y_tick_step) * y_tick_step
  if (!is.finite(y_upper) || y_upper <= 0) {
    y_upper <- y_tick_step
  } else if (y_upper <= y_max) {
    y_upper <- y_upper + y_tick_step
  }

  # 依範例保留不對稱的上下空間：
  # - 最高橫向網格上方再延伸四分之一格，讓直向網格略微凸出；
  # - 0以下保留0.35格，因此最下面一格會比其餘網格稍高。
  y_limits <- c(
    -0.35 * y_tick_step,
    y_upper + 0.25 * y_tick_step
  )
  y_ticks <- seq(0, y_upper, by = y_tick_step)

  par(
    family = plot_family,
    # 右側僅保留圖例所需空間，避免市區客運圖右側留白過多。
    mar = c(5.0, 5.5, 4.5, 8.5),
    xpd = NA
  )

  plot(
    x = x_at,
    y = y_matrix[, 1],
    type = "n",
    # 左右各保留半個月份，使第一個及最後一個資料點不貼住縱軸或邊界。
    xlim = c(0.5, nrow(wide_table) + 0.5),
    ylim = y_limits,
    xaxs = "i",
    yaxs = "i",
    axes = FALSE,
    ann = FALSE,
    bty = "n"
  )

  draw_grid_and_labels(
    x_labels = wide_table$月份標示,
    y_ticks = y_ticks,
    vertical_grid_mode = "monthly",
    grid_lty = "solid",
    show_zero_y = FALSE,
    show_axis_lines = FALSE
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
    # 標題緊接在凸出的直向網格上方。
    line = 0.4,
    adj = 0,
    cex = 1.50,
    font = 2
  )
  mtext("月份", side = 1, line = 3.0, cex = 1.15)
  y_axis_label_at <- if (y_tick_step == 1000 && y_upper >= 4000) {
    3500
  } else {
    y_upper / 2
  }
  mtext(
    "人數",
    side = 2,
    line = 3.7,
    at = y_axis_label_at,
    cex = 1.15
  )

  plot_region <- par("usr")

  legend(
    # 圖例放在繪圖區右側並緊貼圖框，兼顧完整顯示與減少留白。
    x = plot_region[2] + diff(plot_region[1:2]) * 0.012,
    y = plot_region[4] - diff(plot_region[3:4]) * 0.10,
    legend = ride_labels,
    col = line_colors,
    lty = 1,
    lwd = 1.8,
    pch = line_point,
    pt.cex = 0.82,
    bty = "n",
    xpd = NA,
    xjust = 0,
    yjust = 1,
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
    月總使用次數 = sum(原始票證筆數, na.rm = TRUE),
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
    # 縮減右側留白，但仍保留完整圖例所需空間。
    mar = c(5.0, 5.5, 4.5, 8.5),
    xpd = NA
  )

  plot(
    x = x_at,
    y = y,
    type = "n",
    xlim = c(0.5, nrow(monthly_daily_mean) + 0.5),
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
    y_digits = 3,
    vertical_grid_mode = "major",
    grid_lty = "dotted",
    show_zero_y = TRUE,
    show_axis_lines = TRUE,
    # 將日均圖網格的四周略微收進，避免視覺上凸出圖框。
    grid_inset_ratio = 0.008
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
    # 標題往下靠近繪圖區，與參考圖一致。
    line = 0.2,
    adj = 0,
    cex = 1.50,
    font = 2
  )
  mtext("月份", side = 1, line = 3.0, cex = 1.15)
  mtext("日均使用次數", side = 2, line = 3.7, cex = 1.15)

  plot_region <- par("usr")

  legend(
    # 日均圖例緊貼繪圖區右側，避免產生過多空白。
    x = plot_region[2] + diff(plot_region[1:2]) * 0.012,
    y = plot_region[4] - diff(plot_region[3:4]) * 0.12,
    legend = "日均使用次數",
    col = line_colors[1],
    lty = 1,
    lwd = 1.8,
    pch = line_point,
    pt.cex = 0.82,
    bty = "n",
    xpd = NA,
    xjust = 0,
    yjust = 1,
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
    all_interval_plot_title,
    y_tick_step = 1000
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
    tpass_interval_plot_title,
    y_tick_step = 50
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

  invisible(normalizePath(output_dir, winslash = "/", mustWork = FALSE))
}

run_road_bus_analysis <- function() {
# ----------------------------------------------------------------
# B. 臺東縣公路客運完整程式
# 來源：最終交付_臺東縣公路客運_11306至11506/臺東縣公路客運code
# ----------------------------------------------------------------
# ================================================================
# 113年6月至115年6月臺東縣公路客運分析－最終交付版
#
# 本程式只輸出：
#   1. 平均每日使用次數折線圖（PNG）
#   2. 一般乘客各搭乘次數區間月人數變化折線圖（PNG）
#   3. TPASS乘客各搭乘次數區間月人數變化折線圖（PNG）
#   4. 平均每日使用次數完整數據表（Excel）
#   5. 一般乘客各搭乘次數區間月人數變化表（Excel）
#   6. TPASS乘客各搭乘次數區間月人數變化表（Excel）
#   7. 原始票證筆數稽核表（CSV；票證筆數亦作為使用次數計算權重）
# ================================================================


# ----------------------------------------------------------------
# Part 1：載入套件與中文字體
# ----------------------------------------------------------------
required_packages <- c("data.table", "ggplot2", "scales", "openxlsx")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

# 第一次執行且缺少套件時，自動安裝
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(scales)
  library(openxlsx)
})

# Windows中文字體設定：使用微軟正黑體，避免中文方框或亂碼
if (.Platform$OS.type == "windows") {
  windowsFonts(Kai = windowsFont("Microsoft JhengHei"))
}
font_family <- if (.Platform$OS.type == "windows") "Kai" else "sans"

# 建立最終交付資料夾
output_dir <- Sys.getenv(
  "TAITUNG_ROAD_OUTPUT_DIR",
  unset = file.path(getwd(), "最終交付_臺東縣公路客運_11306至11506")
)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}


# ----------------------------------------------------------------
# Part 2：讀取原始資料
# ----------------------------------------------------------------
# 若檔案不在預設位置，程式會自動開啟選檔視窗
# 也可用環境變數TAITUNG_ROAD_CSV指定檔案
default_file_path <- "C:/Users/b9081/Downloads/公路客運2024_to_202606 (1).csv.txt"
file_path <- Sys.getenv("TAITUNG_ROAD_CSV", unset = default_file_path)

if (!file.exists(file_path)) {
  message("找不到預設資料檔，請在視窗中選取CSV或CSV.txt檔案。")
  file_path <- file.choose()
}

# 本次分析實際使用的原始資料欄位
required_columns <- c(
  "卡號",
  "票種類型",
  "票種次類型",
  "搭乘路線代碼",
  "搭乘路線名稱",
  "原始票證筆數",
  "資料代表日期(yyyy-MM-dd)"
)

# 先檢查欄位名稱，再讀取大型資料檔
actual_columns <- names(fread(
  file = file_path,
  nrows = 0,
  encoding = "UTF-8",
  check.names = FALSE,
  showProgress = FALSE
))

missing_columns <- setdiff(required_columns, actual_columns)
if (length(missing_columns) > 0) {
  stop(
    "原始資料缺少必要欄位：",
    paste(missing_columns, collapse = "、"),
    "\n請確認是否選到正確檔案。"
  )
}

# 只讀取會用到的欄位，降低大型資料檔的記憶體用量
raw_data <- fread(
  file = file_path,
  select = required_columns,
  colClasses = "character",
  na.strings = c("", "NA"),
  encoding = "UTF-8",
  check.names = FALSE,
  showProgress = TRUE
)


# ----------------------------------------------------------------
# Part 3：資料清理與篩選
# ----------------------------------------------------------------
# 臺東縣公路客運路線清單
routes_taitung_THB <- c(
  "1145", "309", "8101", "8102", "8103", "8105", "8107", "8109",
  "8110", "8111", "8113", "8115", "8117", "8119", "8120", "8122",
  "8125", "8128", "8129", "8130", "8131", "8132", "8135", "8136",
  "8137", "8138", "8150", "8151", "8152", "8153", "8156", "8157",
  "8158", "8161", "8163", "8165", "8166", "8167", "8168", "8170",
  "8171", "8172", "8173", "8178", "8180", "8181"
)

# 固定分析期間：民國113年6月至115年6月，共25個月
start_date <- as.IDate("2024-06-01")
end_date <- as.IDate("2026-06-30")

month_dates <- as.IDate(seq.Date(
  from = as.Date("2024-06-01"),
  to = as.Date("2026-06-01"),
  by = "month"
))

month_lookup <- data.table(年月日期 = month_dates)
month_lookup[, 年月 := sprintf(
  "%03d/%02d",
  as.integer(format(年月日期, "%Y")) - 1911L,
  as.integer(format(年月日期, "%m"))
)]
month_levels <- month_lookup$年月

# 整理日期、卡號、票種與路線欄位
setnames(raw_data, "資料代表日期(yyyy-MM-dd)", "營運日期")
raw_data[, 營運日期 := as.IDate(營運日期, format = "%Y-%m-%d")]
raw_data[, `:=`(
  卡號 = trimws(卡號),
  票種類型 = trimws(票種類型),
  票種次類型 = trimws(fcoalesce(票種次類型, "")),
  搭乘路線代碼 = trimws(fcoalesce(搭乘路線代碼, "")),
  搭乘路線名稱 = trimws(fcoalesce(搭乘路線名稱, "")),
  原始票證筆數 = trimws(fcoalesce(原始票證筆數, ""))
)]

parse_original_ticket_count <- function(x) {
  cleaned <- gsub(",", "", trimws(fcoalesce(as.character(x), "")), fixed = TRUE)
  value <- suppressWarnings(as.numeric(cleaned))
  invalid <- cleaned != "" & is.na(value)

  if (any(invalid)) {
    examples <- head(unique(cleaned[invalid]), 5L)
    stop(
      "原始票證筆數含有無法轉為數字的內容，例如：",
      paste(examples, collapse = "、")
    )
  }
  if (any(value < 0, na.rm = TRUE)) {
    stop("原始票證筆數不可為負數。")
  }
  if (any(abs(value - round(value)) > 1e-8, na.rm = TRUE)) {
    stop("原始票證筆數必須是整數。")
  }

  value
}

raw_data[, 原始票證筆數 := parse_original_ticket_count(原始票證筆數)]
missing_original_count_n <- raw_data[is.na(原始票證筆數), .N]
if (missing_original_count_n > 0L) {
  warning(
    "原始票證筆數有 ",
    format(missing_original_count_n, big.mark = ","),
    " 筆空白值；計算加總時將依 na.rm = TRUE 忽略。"
  )
}

# 路線名稱有值時優先使用；空白時由THB路線代碼取出數字
raw_data[, 路線編號 := fifelse(
  搭乘路線名稱 != "",
  搭乘路線名稱,
  sub("^THB", "", 搭乘路線代碼)
)]

# 篩選113年6月至115年6月
analysis_data <- raw_data[
  !is.na(營運日期) &
    營運日期 >= start_date &
    營運日期 <= end_date
]

# 篩選臺東縣公路客運
analysis_data <- analysis_data[路線編號 %chin% routes_taitung_THB]

# 原始票證筆數除用於稽核外，也作為使用次數相關指標的加總權重。
# 稽核範圍為指定日期及指定公路客運路線內的原始資料列。
original_count_audit <- analysis_data[
  ,
  .(資料列數 = .N),
  by = .(
    年月 = format(營運日期, "%Y-%m"),
    原始票證筆數
  )
][order(年月, 原始票證筆數)]

original_count_audit_file <- file.path(
  output_dir,
  "原始票證筆數稽核表.csv"
)

fwrite(
  original_count_audit,
  original_count_audit_file,
  bom = TRUE
)

# 空白卡號無法計算不重複使用人數，因此排除
blank_card_rows <- analysis_data[is.na(卡號) | 卡號 == "", .N]
if (blank_card_rows > 0) {
  warning("已排除 ", format(blank_card_rows, big.mark = ","), " 筆空白卡號資料。")
  analysis_data <- analysis_data[!is.na(卡號) & 卡號 != ""]
}

if (nrow(analysis_data) == 0) {
  stop("篩選後沒有資料，請檢查檔案、日期及路線清單。")
}

# 建立真正的年月日期，確保依時間先後排序
analysis_data[, 年月日期 := as.IDate(format(營運日期, "%Y-%m-01"))]

# TPASS辨識方式：票種類型4，且票種次類型屬於下列定期票
tpass_subtypes <- c("#TTT-299", "#HUA-399", "#HUA-199")
analysis_data[, 是否TPASS :=
  票種類型 == "4" & 票種次類型 %chin% tpass_subtypes
]

# 檢查資料月份是否完整
missing_months <- month_lookup[
  !年月日期 %in% unique(analysis_data$年月日期),
  年月
]
if (length(missing_months) > 0) {
  warning(
    "下列月份沒有符合條件的資料，後續圖表將補0：",
    paste(missing_months, collapse = "、")
  )
}

# 顯示清單中在指定期間沒有資料的路線，不影響程式執行
missing_routes <- setdiff(routes_taitung_THB, unique(analysis_data$路線編號))
if (length(missing_routes) > 0) {
  message("指定期間沒有資料的路線：", paste(missing_routes, collapse = "、"))
}

# 完成篩選後移除大型原始物件
rm(raw_data)
invisible(gc())


# ----------------------------------------------------------------
# Part 4：建立三份共用分析資料
# ----------------------------------------------------------------
# 4-1 平均每日使用次數
# 操作型定義：
# 每人每月平均使用次數 = 該月總使用次數 ÷ 該月使用人數
# 平均每日使用次數 = 每人每月平均使用次數 ÷ 該月天數
# 月總使用次數以原始票證筆數加總計算。
daily_mean_data <- analysis_data[, .(
  搭乘次數 = sum(原始票證筆數, na.rm = TRUE),
  人數 = uniqueN(卡號)
), by = 年月日期]

daily_mean_data <- merge(
  month_lookup,
  daily_mean_data,
  by = "年月日期",
  all.x = TRUE,
  sort = TRUE
)

daily_mean_data[is.na(搭乘次數), 搭乘次數 := 0L]
daily_mean_data[is.na(人數), 人數 := 0L]
daily_mean_data[, 下月首日 := as.IDate(seq.Date(
  from = as.Date("2024-07-01"),
  to = as.Date("2026-07-01"),
  by = "month"
))]
daily_mean_data[, 天數 := as.integer(下月首日 - 年月日期)]
daily_mean_data[, 日平均 := fifelse(
  人數 > 0,
  搭乘次數 / 人數 / 天數,
  0
)]
daily_mean_data[, 下月首日 := NULL]
setorder(daily_mean_data, 年月日期)

# 搭乘次數區間：僅一次、二至四次、五次以上
interval_levels <- c("僅一次", "二至四次", "五次以上")

# 共用函數：同一份結果同時供折線圖與Excel表格使用
create_monthly_interval_data <- function(data) {
  card_monthly <- data[
    , .(搭乘次數 = sum(原始票證筆數, na.rm = TRUE)),
    by = .(年月日期, 卡號)
  ]

  card_monthly[, 次數區間 := cut(
    搭乘次數,
    breaks = c(0, 1, 4, Inf),
    labels = interval_levels,
    ordered_result = TRUE
  )]

  interval_data <- card_monthly[, .(人數 = .N), by = .(年月日期, 次數區間)]

  # 補齊25個月份與3個區間
  complete_grid <- CJ(
    年月日期 = month_dates,
    次數區間 = factor(
      interval_levels,
      levels = interval_levels,
      ordered = TRUE
    ),
    unique = TRUE
  )

  interval_data <- merge(
    complete_grid,
    interval_data,
    by = c("年月日期", "次數區間"),
    all.x = TRUE,
    sort = TRUE
  )
  interval_data[is.na(人數), 人數 := 0L]
  interval_data <- merge(
    interval_data,
    month_lookup,
    by = "年月日期",
    all.x = TRUE,
    sort = TRUE
  )
  interval_data[, 年月 := factor(年月, levels = month_levels, ordered = TRUE)]
  interval_data[, 次數區間 := factor(
    次數區間,
    levels = interval_levels,
    ordered = TRUE
  )]
  setorder(interval_data, 年月日期, 次數區間)
  interval_data[]
}

# 4-2 一般公路客運乘客（包含所有票種）
road_interval_data <- create_monthly_interval_data(analysis_data)

# 4-3 TPASS乘客
tpass_data <- analysis_data[是否TPASS == TRUE]
if (nrow(tpass_data) == 0) {
  stop("指定期間與路線中沒有TPASS資料。")
}
tpass_interval_data <- create_monthly_interval_data(tpass_data)


# ----------------------------------------------------------------
# Part 5：圖表共用函數
# ----------------------------------------------------------------
# 每月都有垂直網格線，月份文字每隔一個月顯示一次
report_month_labels_all <- ifelse(
  seq_along(month_levels) %% 2 == 1,
  gsub("/", "", month_levels),
  ""
)

# 灰階圖表共用排版
report_theme <- function(top_margin = 18, left_margin = 18) {
  theme_minimal(base_family = font_family, base_size = 12) +
    theme(
      text = element_text(family = font_family, color = "black"),
      axis.title = element_text(size = 16, face = "plain"),
      axis.title.x = element_text(size = 16, margin = margin(t = 6)),
      axis.title.y = element_text(size = 16, margin = margin(r = 24)),
      axis.text = element_text(size = 10, color = "gray25"),
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      plot.title = element_text(
        size = 18.5,
        face = "bold",
        hjust = 0,
        margin = margin(b = 12)
      ),
      plot.title.position = "panel",
      legend.title = element_text(
        size = 11,
        face = "plain",
        hjust = 0,
        margin = margin(l = 14)
      ),
      legend.text = element_text(size = 10, margin = margin(l = 4)),
      legend.key.width = grid::unit(1, "cm"),
      legend.position = "right",
      legend.justification = c(0.5, 0.90),
      panel.grid.major = element_line(color = "gray85", linewidth = 0.35),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(top_margin, 24, 15, left_margin)
    )
}

# 一般乘客與TPASS乘客共用折線圖函數
group_monthly_ridership_lineplot <- function(
    plot_data,
    plot_title,
    top_margin = 18,
    left_margin = 18,
    y_step_override = NULL) {
  color <- setNames(
    gray.colors(3, start = 0.10, end = 0.75),
    interval_levels
  )

  max_y <- max(plot_data$人數, na.rm = TRUE)
  if (is.null(y_step_override)) {
    y_pretty <- pretty(c(0, max_y), n = 10)
    y_step <- min(diff(y_pretty))
    y_upper <- min(y_pretty[y_pretty >= max_y])
  } else {
    y_step <- y_step_override
    y_upper <- ceiling(max_y / y_step) * y_step
  }
  y_breaks <- seq(y_step, y_upper, by = y_step)

  ggplot(
    plot_data,
    aes(
      x = 年月,
      y = 人數,
      color = 次數區間,
      group = 次數區間
    )
  ) +
    geom_line(linewidth = 1.0) +
    geom_point(size = 2.0) +
    scale_color_manual(values = color, drop = FALSE) +
    scale_x_discrete(
      breaks = month_levels,
      labels = report_month_labels_all,
      drop = FALSE
    ) +
    scale_y_continuous(
      limits = c(0, y_upper + y_step * 0.35),
      breaks = y_breaks,
      labels = scales::comma,
      expand = expansion(mult = 0)
    ) +
    labs(
      x = "月份",
      y = "人數",
      color = "搭乘次數",
      title = plot_title
    ) +
    report_theme(
      top_margin = top_margin,
      left_margin = left_margin
    )
}

# 平均每日使用次數折線圖函數
daily_mean_plot <- function(plot_title) {
  plot_values <- daily_mean_data$日平均
  x_positions <- seq_along(plot_values)

  daily_y_breaks <- pretty(range(plot_values, na.rm = TRUE), n = 7)
  daily_y_breaks <- daily_y_breaks[
    daily_y_breaks >= min(plot_values, na.rm = TRUE) -
      diff(range(plot_values, na.rm = TRUE)) * 0.20 &
      daily_y_breaks <= max(plot_values, na.rm = TRUE) +
        diff(range(plot_values, na.rm = TRUE)) * 0.20
  ]
  if (length(daily_y_breaks) < 2) {
    daily_y_breaks <- pretty(range(plot_values, na.rm = TRUE), n = 7)
  }
  daily_y_step <- min(diff(sort(unique(daily_y_breaks))))

  daily_y_limits <- c(
    min(daily_y_breaks) - daily_y_step * 0.5,
    max(daily_y_breaks) + daily_y_step * 0.35
  )

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  par(
    family = font_family,
    mar = c(5, 6, 4, 10),
    xaxs = "i",
    yaxs = "i"
  )

  current_plt <- par("plt")
  par(plt = current_plt + c(0, 0, -0.027, -0.027))

  plot(
    x = x_positions,
    y = plot_values,
    type = "n",
    xlab = "",
    ylab = "日均使用次數",
    xlim = c(-1.1, 26),
    ylim = daily_y_limits,
    xaxt = "n",
    yaxt = "n",
    bty = "n",
    cex.lab = 1.3,
    cex.axis = 1
  )

  # 點狀網格；水平網格由Y軸開始
  vertical_grid_values <- axTicks(1)
  vertical_grid_values <- vertical_grid_values[
    abs(vertical_grid_values) > 1e-8
  ]
  abline(
    v = vertical_grid_values,
    col = "gray88",
    lty = "dotted",
    lwd = 1
  )
  segments(
    x0 = 0,
    y0 = daily_y_breaks,
    x1 = par("usr")[2],
    y1 = daily_y_breaks,
    col = "gray88",
    lty = "dotted",
    lwd = 1
  )

  x_labels <- ifelse(
    seq_along(month_levels) %% 2 == 1,
    gsub("/", "", month_levels),
    ""
  )
  axis(
    side = 1,
    at = x_positions,
    labels = x_labels,
    hadj = 0.45,
    padj = 0.65,
    cex.axis = 1,
    col = "gray45",
    col.axis = "gray25"
  )
  axis(
    side = 2,
    at = daily_y_breaks,
    labels = sprintf("%.2f", daily_y_breaks),
    las = 1,
    pos = 0,
    lwd = 0,
    lwd.ticks = 1,
    cex.axis = 1,
    col = "gray45",
    col.axis = "gray25"
  )

  # Y軸與網格起點完全對齊，且上下不突出
  segments(
    x0 = 0,
    y0 = min(daily_y_breaks),
    x1 = 0,
    y1 = max(daily_y_breaks),
    col = "gray45",
    lwd = 1.5,
    xpd = FALSE
  )

  lines(
    x = x_positions,
    y = plot_values,
    type = "o",
    lwd = 1.5,
    pch = 16,
    cex = 0.9,
    col = gray.colors(2)[1]
  )

  mtext(
    text = "月份",
    side = 1,
    line = 2.85,
    at = mean(par("usr")[1:2]) + 1.35,
    cex = 1.3
  )
  mtext(
    text = plot_title,
    side = 3,
    line = 1.50,
    at = 0,
    adj = 0,
    font = 2,
    cex = 1.55
  )
  plot_region <- par("usr")
  legend(
    # 使用明確座標將圖例放入右側邊界，避免負 inset 造成文字被裁切。
    x = plot_region[2] + diff(plot_region[1:2]) * 0.012,
    y = plot_region[4] - diff(plot_region[3:4]) * 0.10,
    legend = "日均使用次數",
    col = gray.colors(2)[1],
    lwd = 1.5,
    pch = 16,
    bty = "n",
    xpd = NA,
    xjust = 0,
    yjust = 1,
    cex = 1
  )
}


# ----------------------------------------------------------------
# Part 6：Excel表格共用函數
# ----------------------------------------------------------------
# 將日期轉為「113 年 6 月」格式
format_roc_month <- function(date_value) {
  sprintf(
    "%d 年 %d 月",
    as.integer(format(date_value, "%Y")) - 1911L,
    as.integer(format(date_value, "%m"))
  )
}

# 由折線圖共用資料建立人數與百分比表，不重新統計
create_interval_table_data <- function(interval_data) {
  wide <- dcast(
    copy(interval_data),
    年月日期 + 年月 ~ 次數區間,
    value.var = "人數",
    fill = 0
  )
  setorder(wide, 年月日期)

  total <- wide[["僅一次"]] + wide[["二至四次"]] + wide[["五次以上"]]
  safe_prop <- function(x) fifelse(total > 0, x / total, 0)

  data.frame(
    月份 = format_roc_month(wide$年月日期),
    僅一次人數 = wide[["僅一次"]],
    僅一次比例 = safe_prop(wide[["僅一次"]]),
    二至四次人數 = wide[["二至四次"]],
    二至四次比例 = safe_prop(wide[["二至四次"]]),
    五次以上人數 = wide[["五次以上"]],
    五次以上比例 = safe_prop(wide[["五次以上"]]),
    check.names = FALSE
  )
}

# 匯出搭乘次數區間Excel表格
export_interval_table_to_excel <- function(
    table_data,
    file,
    table_title,
    sheet_name) {
  wb <- createWorkbook()
  addWorksheet(wb, sheet_name, gridLines = FALSE)

  title_style <- createStyle(
    fontName = "Microsoft JhengHei",
    fontSize = 14,
    halign = "center",
    valign = "center"
  )
  header_style <- createStyle(
    fontName = "Microsoft JhengHei",
    fontSize = 12,
    halign = "center",
    valign = "center",
    border = "TopBottomLeftRight",
    borderColour = "#000000",
    borderStyle = "thin"
  )
  diagonal_header_style <- createStyle(
    fontName = "Microsoft JhengHei",
    fontSize = 11,
    halign = "left",
    valign = "center",
    wrapText = TRUE,
    border = "TopBottomLeftRight",
    borderColour = "#000000",
    borderStyle = "thin"
  )
  diagonal_header_style$borderDiagonal <- "thin"
  diagonal_header_style$borderDiagonalColour <- list(rgb = "FF000000")
  diagonal_header_style$borderDiagonalDown <- TRUE
  month_style <- createStyle(
    fontName = "Microsoft JhengHei",
    fontSize = 11,
    halign = "left",
    valign = "center",
    border = "TopBottomLeftRight",
    borderColour = "#000000",
    borderStyle = "thin"
  )
  count_style <- createStyle(
    fontName = "Microsoft JhengHei",
    fontSize = 11,
    halign = "right",
    valign = "center",
    numFmt = "#,##0",
    border = "TopBottomLeftRight",
    borderColour = "#000000",
    borderStyle = "thin"
  )
  percent_style <- createStyle(
    fontName = "Microsoft JhengHei",
    fontSize = 11,
    halign = "right",
    valign = "center",
    numFmt = "0.0%",
    border = "TopBottomLeftRight",
    borderColour = "#000000",
    borderStyle = "thin"
  )

  # 標題列
  mergeCells(wb, sheet_name, cols = 1:7, rows = 1)
  writeData(wb, sheet_name, table_title, startCol = 1, startRow = 1)
  addStyle(wb, sheet_name, title_style, rows = 1, cols = 1:7, gridExpand = TRUE)

  # 斜線表頭與三個搭乘次數區間
  mergeCells(wb, sheet_name, cols = 1, rows = 3:4)
  mergeCells(wb, sheet_name, cols = 2:3, rows = 3:4)
  mergeCells(wb, sheet_name, cols = 4:5, rows = 3:4)
  mergeCells(wb, sheet_name, cols = 6:7, rows = 3:4)
  writeData(wb, sheet_name, "    搭乘次數\n月份", startCol = 1, startRow = 3)
  writeData(wb, sheet_name, "僅一次", startCol = 2, startRow = 3)
  writeData(wb, sheet_name, "二至四次", startCol = 4, startRow = 3)
  writeData(wb, sheet_name, "五次以上", startCol = 6, startRow = 3)
  addStyle(wb, sheet_name, header_style, rows = 3:4, cols = 1:7, gridExpand = TRUE)
  addStyle(
    wb,
    sheet_name,
    diagonal_header_style,
    rows = 3:4,
    cols = 1,
    gridExpand = TRUE,
    stack = TRUE
  )

  # 25個月份資料
  writeData(
    wb,
    sheet_name,
    table_data,
    startCol = 1,
    startRow = 5,
    colNames = FALSE,
    rowNames = FALSE
  )
  body_rows <- 5:(nrow(table_data) + 4)
  addStyle(wb, sheet_name, month_style, rows = body_rows, cols = 1, gridExpand = TRUE)
  addStyle(
    wb,
    sheet_name,
    count_style,
    rows = body_rows,
    cols = c(2, 4, 6),
    gridExpand = TRUE
  )
  addStyle(
    wb,
    sheet_name,
    percent_style,
    rows = body_rows,
    cols = c(3, 5, 7),
    gridExpand = TRUE
  )

  setColWidths(wb, sheet_name, cols = 1, widths = 16)
  setColWidths(wb, sheet_name, cols = c(2, 4, 6), widths = 12)
  setColWidths(wb, sheet_name, cols = c(3, 5, 7), widths = 10)
  setRowHeights(wb, sheet_name, rows = 1, heights = 28)
  setRowHeights(wb, sheet_name, rows = 3:4, heights = 24)
  setRowHeights(wb, sheet_name, rows = body_rows, heights = 22)
  freezePane(wb, sheet_name, firstActiveRow = 5, firstActiveCol = 2)
  pageSetup(
    wb,
    sheet_name,
    orientation = "portrait",
    fitToWidth = 1,
    fitToHeight = 0
  )

  saveWorkbook(wb, file, overwrite = TRUE)
  invisible(file)
}

# 匯出平均每日使用次數完整數據表
export_daily_mean_table_to_excel <- function(table_data, file, table_title) {
  sheet_name <- "平均每日使用次數"
  wb <- createWorkbook()
  addWorksheet(wb, sheet_name, gridLines = FALSE)

  title_style <- createStyle(
    fontName = "Microsoft JhengHei",
    fontSize = 13,
    halign = "center",
    valign = "center",
    wrapText = TRUE
  )
  header_style <- createStyle(
    fontName = "Microsoft JhengHei",
    fontSize = 12,
    halign = "center",
    valign = "center",
    border = "TopBottomLeftRight",
    borderColour = "#000000",
    borderStyle = "thin"
  )
  month_style <- createStyle(
    fontName = "Microsoft JhengHei",
    fontSize = 11,
    halign = "left",
    valign = "center",
    border = "TopBottomLeftRight",
    borderColour = "#000000",
    borderStyle = "thin"
  )
  integer_style <- createStyle(
    fontName = "Microsoft JhengHei",
    fontSize = 11,
    halign = "right",
    valign = "center",
    numFmt = "#,##0",
    border = "TopBottomLeftRight",
    borderColour = "#000000",
    borderStyle = "thin"
  )
  decimal_style <- createStyle(
    fontName = "Microsoft JhengHei",
    fontSize = 11,
    halign = "right",
    valign = "center",
    numFmt = "0.000",
    border = "TopBottomLeftRight",
    borderColour = "#000000",
    borderStyle = "thin"
  )

  # 標題與欄位名稱
  mergeCells(wb, sheet_name, cols = 1:6, rows = 1)
  writeData(wb, sheet_name, table_title, startCol = 1, startRow = 1)
  addStyle(wb, sheet_name, title_style, rows = 1, cols = 1:6, gridExpand = TRUE)
  writeData(
    wb,
    sheet_name,
    matrix(
      c(
        "月份",
        "月總使用次數",
        "當月使用人數",
        "當月日數",
        "每人每月平均使用次數",
        "日均使用次數"
      ),
      nrow = 1
    ),
    startCol = 1,
    startRow = 3,
    colNames = FALSE
  )
  addStyle(wb, sheet_name, header_style, rows = 3, cols = 1:6, gridExpand = TRUE)

  # 25個月份完整數據
  writeData(
    wb,
    sheet_name,
    table_data,
    startCol = 1,
    startRow = 4,
    colNames = FALSE,
    rowNames = FALSE
  )
  body_rows <- 4:(nrow(table_data) + 3)
  addStyle(wb, sheet_name, month_style, rows = body_rows, cols = 1, gridExpand = TRUE)
  addStyle(wb, sheet_name, integer_style, rows = body_rows, cols = 2:4, gridExpand = TRUE)
  addStyle(wb, sheet_name, decimal_style, rows = body_rows, cols = 5:6, gridExpand = TRUE)

  setColWidths(wb, sheet_name, cols = 1, widths = 16)
  setColWidths(wb, sheet_name, cols = 2:3, widths = 18)
  setColWidths(wb, sheet_name, cols = 4, widths = 14)
  setColWidths(wb, sheet_name, cols = 5, widths = 27)
  setColWidths(wb, sheet_name, cols = 6, widths = 19)
  setRowHeights(wb, sheet_name, rows = 1, heights = 46)
  setRowHeights(wb, sheet_name, rows = 3, heights = 34)
  setRowHeights(wb, sheet_name, rows = body_rows, heights = 22)
  freezePane(wb, sheet_name, firstActiveRow = 4, firstActiveCol = 2)
  pageSetup(
    wb,
    sheet_name,
    orientation = "landscape",
    fitToWidth = 1,
    fitToHeight = 0
  )

  saveWorkbook(wb, file, overwrite = TRUE)
  invisible(file)
}


# ----------------------------------------------------------------
# Part 7：輸出三張折線圖
# ----------------------------------------------------------------
# 若前一次繪圖中斷，先關閉殘留的繪圖裝置
try(grDevices::graphics.off(), silent = TRUE)

# 7-1 平均每日使用次數折線圖
title_daily_mean <-
  "113年6月至115年6月臺東縣公路客運平均每日使用次數折線圖"
daily_mean_png <- file.path(output_dir, paste0(title_daily_mean, ".png"))

png(
  filename = daily_mean_png,
  width = 15,
  height = 5,
  units = "in",
  res = 300,
  bg = "white",
  family = font_family
)
daily_mean_plot(title_daily_mean)
invisible(dev.off())

# 7-2 一般乘客搭乘次數區間折線圖
title_road_interval <-
  "113年6月至115年6月臺東縣公路客運乘客各搭乘次數區間月人數變化折線圖"
road_interval_plot <- group_monthly_ridership_lineplot(
  plot_data = road_interval_data,
  plot_title = title_road_interval,
  top_margin = 40,
  left_margin = 44
)
road_interval_png <- file.path(output_dir, paste0(title_road_interval, ".png"))
ggsave(
  filename = road_interval_png,
  plot = road_interval_plot,
  width = 15,
  height = 5,
  units = "in",
  dpi = 300,
  bg = "white"
)

# 7-3 TPASS乘客搭乘次數區間折線圖
title_tpass_interval <-
  "113年6月至115年6月臺東縣公路客運TPASS乘客各搭乘次數區間月人數變化折線圖"
tpass_interval_plot <- group_monthly_ridership_lineplot(
  plot_data = tpass_interval_data,
  plot_title = title_tpass_interval,
  top_margin = 26,
  left_margin = 54,
  y_step_override = 200
)
tpass_interval_png <- file.path(output_dir, paste0(title_tpass_interval, ".png"))
ggsave(
  filename = tpass_interval_png,
  plot = tpass_interval_plot,
  width = 15,
  height = 5,
  units = "in",
  dpi = 300,
  bg = "white"
)


# ----------------------------------------------------------------
# Part 8：輸出三個Excel表格
# ----------------------------------------------------------------
# 8-1 平均每日使用次數完整數據表
title_daily_mean_table <-
  "113年6月至115年6月臺東縣公路客運平均每日使用次數完整數據表"
daily_mean_table_data <- data.frame(
  月份 = format_roc_month(daily_mean_data$年月日期),
  月總使用次數 = daily_mean_data$搭乘次數,
  當月使用人數 = daily_mean_data$人數,
  當月日數 = daily_mean_data$天數,
  每人每月平均使用次數 = fifelse(
    daily_mean_data$人數 > 0,
    daily_mean_data$搭乘次數 / daily_mean_data$人數,
    0
  ),
  日均使用次數 = daily_mean_data$日平均,
  check.names = FALSE
)
daily_mean_table_file <- file.path(
  output_dir,
  paste0(title_daily_mean_table, ".xlsx")
)
export_daily_mean_table_to_excel(
  table_data = daily_mean_table_data,
  file = daily_mean_table_file,
  table_title = title_daily_mean_table
)

# 8-2 一般乘客搭乘次數區間月人數變化表
# 與第二張折線圖共用road_interval_data
road_interval_table_data <- create_interval_table_data(road_interval_data)
road_table_title <-
  "113年6月至115年6月臺東縣公路客運乘客各搭乘次數區間月人數變化表"
road_table_file <- file.path(output_dir, paste0(road_table_title, ".xlsx"))
export_interval_table_to_excel(
  table_data = road_interval_table_data,
  file = road_table_file,
  table_title = road_table_title,
  sheet_name = "公路客運乘客"
)

# 8-3 TPASS乘客搭乘次數區間月人數變化表
# 與第三張折線圖共用tpass_interval_data
tpass_interval_table_data <- create_interval_table_data(tpass_interval_data)
tpass_table_title <-
  "113年6月至115年6月臺東縣公路客運TPASS乘客各搭乘次數區間月人數變化表"
tpass_table_file <- file.path(output_dir, paste0(tpass_table_title, ".xlsx"))
export_interval_table_to_excel(
  table_data = tpass_interval_table_data,
  file = tpass_table_file,
  table_title = tpass_table_title,
  sheet_name = "公路客運TPASS乘客"
)


# ----------------------------------------------------------------
# Part 9：完成檢查
# ----------------------------------------------------------------
expected_output_files <- c(
  original_count_audit_file,
  daily_mean_png,
  road_interval_png,
  tpass_interval_png,
  daily_mean_table_file,
  road_table_file,
  tpass_table_file
)

missing_output_files <- expected_output_files[!file.exists(expected_output_files)]
if (length(missing_output_files) > 0) {
  stop(
    "下列交付檔案未成功產生：\n",
    paste(basename(missing_output_files), collapse = "\n")
  )
}

message("============================================================")
message("分析完成：共產生3張折線圖、3個Excel表格與1份原始票證筆數稽核表。")
message("輸出資料夾：")
message(normalizePath(output_dir, winslash = "/", mustWork = FALSE))
message("============================================================")

  invisible(normalizePath(output_dir, winslash = "/", mustWork = FALSE))
}

city_output_path <- NULL
road_output_path <- NULL

if (isTRUE(execute_city_bus)) {
  message("開始執行：臺東縣市區客運分析")
  city_output_path <- run_city_bus_analysis()
}

if (isTRUE(execute_road_bus)) {
  message("開始執行：臺東縣公路客運分析")
  road_output_path <- run_road_bus_analysis()
}

message("============================================================")
message("合併程式執行完成。")
if (!is.null(city_output_path)) {
  message("市區客運輸出資料夾：", city_output_path)
}
if (!is.null(road_output_path)) {
  message("公路客運輸出資料夾：", road_output_path)
}
message("============================================================")
