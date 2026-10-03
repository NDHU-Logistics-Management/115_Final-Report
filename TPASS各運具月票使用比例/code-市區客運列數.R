# TPASS 花東市區客運月票使用比例分析
#
# 研究期間：2024-06-01 至 2026-06-30（民國 113/06 至 115/06）
# 使用比例：TPASS 搭乘紀錄列數 / 全部搭乘紀錄列數 * 100%
# 每一列視為一筆搭乘紀錄，不使用「原始票證筆數」加權。
#
# 注意：原始票證資料可能包含敏感或可識別欄位，請勿將原始 TXT
# 上傳至公開 GitHub 儲存庫。本程式只輸出彙整結果。

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop(
    "缺少 data.table 套件。請先執行：install.packages(\"data.table\")"
  )
}

suppressPackageStartupMessages(
  library(data.table)
)


# -----------------------------------------------------------------------------
# 分析設定
# -----------------------------------------------------------------------------

ANALYSIS_START <- as.IDate("2024-06-01")
ANALYSIS_END <- as.IDate("2026-06-30")

KEEP_LABEL <- "保留：指定市區客運"

COUNTY_CONFIG <- list(
  hualien = list(
    county_name = "花蓮縣",
    expected_prefix = "HUA",
    routes = c(
      "HUA0027",
      "HUA0117",
      "HUA0300",
      "HUA0301",
      "HUA0302",
      "HUA0303",
      "HUA0305",
      "HUA0307",
      "HUA0308"
    ),
    tpass_codes = c(
      "#HUA-199",
      "#HUA-399"
    ),
    incomplete_months = c(
      "115/04",
      "115/05",
      "115/06"
    ),
    incomplete_note = paste0(
      "資料不完整：太魯閣客運自115/04/10起",
      "未提交301、302、303、305路線票證資料"
    )
  ),
  taitung = list(
    county_name = "臺東縣",
    expected_prefix = "TTT",
    routes = c(
      "TTT0981",
      "TTT0982",
      "TTT0984",
      "TTT0985"
    ),
    tpass_codes = c(
      "#TTT-299"
    ),
    incomplete_months = character(),
    incomplete_note = ""
  )
)


# -----------------------------------------------------------------------------
# 輸入與路徑
# -----------------------------------------------------------------------------

resolve_paths <- function() {

  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) >= 2) {
    hualien_file <- args[[1]]
    taitung_file <- args[[2]]
  } else if (interactive()) {
    message("請選擇花蓮縣公車 TXT 或 CSV")
    hualien_file <- file.choose()

    message("請選擇臺東縣公車 TXT 或 CSV")
    taitung_file <- file.choose()
  } else {
    stop(
      paste0(
        "請提供兩個輸入檔：Rscript tpass_city_bus_usage_analysis.R ",
        "<花蓮TXT或CSV> <臺東TXT或CSV> [輸出資料夾]"
      )
    )
  }

  hualien_file <- normalizePath(
    hualien_file,
    winslash = "/",
    mustWork = TRUE
  )

  taitung_file <- normalizePath(
    taitung_file,
    winslash = "/",
    mustWork = TRUE
  )

  output_dir <- if (length(args) >= 3) {
    args[[3]]
  } else {
    file.path(
      dirname(hualien_file),
      "TPASS市區客運分析結果_已排除外縣市"
    )
  }

  output_dir <- normalizePath(
    output_dir,
    winslash = "/",
    mustWork = FALSE
  )

  if (!dir.exists(output_dir)) {
    dir.create(
      output_dir,
      recursive = TRUE
    )
  }

  list(
    hualien_file = hualien_file,
    taitung_file = taitung_file,
    output_dir = output_dir
  )
}


# -----------------------------------------------------------------------------
# 讀取與清理
# -----------------------------------------------------------------------------

read_bus_data <- function(file_path) {

  dt <- fread(
    file_path,
    fill = TRUE,
    showProgress = TRUE,
    encoding = "UTF-8",
    colClasses = list(
      character = c(
        "業者編號",
        "票種次類型",
        "搭乘路線代碼",
        "搭乘路線名稱"
      )
    )
  )

  setnames(
    dt,
    sub("^\ufeff", "", names(dt))
  )

  required_columns <- c(
    "業者編號",
    "票種次類型",
    "搭乘路線代碼",
    "搭乘路線名稱",
    "原始票證筆數",
    "資料代表日期(yyyy-MM-dd)"
  )

  missing_columns <- setdiff(
    required_columns,
    names(dt)
  )

  if (length(missing_columns) > 0) {
    stop(
      paste0(
        basename(file_path),
        " 缺少必要欄位：",
        paste(missing_columns, collapse = "、")
      )
    )
  }

  dt[, 業者編號 := trimws(as.character(業者編號))]
  dt[, 票種次類型 := trimws(as.character(票種次類型))]
  dt[, 搭乘路線代碼 := trimws(as.character(搭乘路線代碼))]
  dt[, 搭乘路線名稱 := trimws(as.character(搭乘路線名稱))]
  dt[
    ,
    原始票證筆數 := suppressWarnings(
      as.numeric(原始票證筆數)
    )
  ]

  dt[
    ,
    `資料代表日期(yyyy-MM-dd)` := as.IDate(
      trimws(as.character(`資料代表日期(yyyy-MM-dd)`)),
      format = "%Y-%m-%d"
    )
  ]

  invalid_date_rows <- dt[
    is.na(`資料代表日期(yyyy-MM-dd)`),
    .N
  ]

  if (invalid_date_rows > 0) {
    warning(
      basename(file_path),
      " 有 ",
      invalid_date_rows,
      " 列日期無法解析，已排除。"
    )
  }

  invalid_weight_rows <- dt[is.na(原始票證筆數), .N]

  if (invalid_weight_rows > 0) {
    warning(
      basename(file_path),
      " 有 ",
      invalid_weight_rows,
      paste0(
        " 列原始票證筆數無法解析；",
        "列數比例不受影響，清理摘要加總時會略過。"
      )
    )
  }

  dt <- dt[!is.na(`資料代表日期(yyyy-MM-dd)`)]

  dt[]
}


classify_bus_records <- function(
  dt,
  source_name,
  config
) {

  dt[, 來源檔案 := source_name]

  dt[
    ,
    路線前綴 := substr(
      fifelse(
        is.na(搭乘路線代碼),
        "",
        搭乘路線代碼
      ),
      1,
      3
    )
  ]

  dt[
    ,
    資料分類 := fcase(
      is.na(搭乘路線代碼) |
        搭乘路線代碼 == "",
      "排除：路線代碼空白",

      路線前綴 != config$expected_prefix,
      "排除：外縣市資料",

      !搭乘路線代碼 %chin% config$routes,
      "排除：同縣市但非指定市區路線",

      default = KEEP_LABEL
    )
  ]

  dt[
    ,
    研究期間 := fifelse(
      `資料代表日期(yyyy-MM-dd)` >= ANALYSIS_START &
        `資料代表日期(yyyy-MM-dd)` <= ANALYSIS_END,
      "研究期間內",
      "研究期間外"
    )
  ]

  dt[]
}


summarize_overview <- function(dt) {
  dt[
    ,
    .(
      資料列數 = .N,
      原始票證筆數加總 = sum(
        原始票證筆數,
        na.rm = TRUE
      )
    ),
    by = .(
      來源檔案,
      研究期間,
      資料分類,
      路線前綴
    )
  ]
}


summarize_excluded <- function(dt) {
  dt[
    資料分類 != KEEP_LABEL,
    .(
      資料列數 = .N,
      原始票證筆數加總 = sum(
        原始票證筆數,
        na.rm = TRUE
      )
    ),
    by = .(
      來源檔案,
      研究期間,
      月份 = format(
        `資料代表日期(yyyy-MM-dd)`,
        "%Y-%m"
      ),
      資料分類,
      路線前綴,
      業者編號,
      搭乘路線代碼,
      搭乘路線名稱,
      票種次類型
    )
  ]
}


prepare_analysis_data <- function(dt, config) {

  result <- dt[
    資料分類 == KEEP_LABEL &
      搭乘路線代碼 %chin% config$routes &
      `資料代表日期(yyyy-MM-dd)` >= ANALYSIS_START &
      `資料代表日期(yyyy-MM-dd)` <= ANALYSIS_END
  ]

  result[
    ,
    月份 := sprintf(
      "%03d/%02d",
      as.integer(
        format(
          `資料代表日期(yyyy-MM-dd)`,
          "%Y"
        )
      ) - 1911,
      as.integer(
        format(
          `資料代表日期(yyyy-MM-dd)`,
          "%m"
        )
      )
    )
  ]

  setorder(
    result,
    `資料代表日期(yyyy-MM-dd)`
  )

  result[]
}


# -----------------------------------------------------------------------------
# 計算與檢核
# -----------------------------------------------------------------------------

calculate_monthly_rate <- function(
  dt,
  tpass_codes
) {

  # 一列代表一筆搭乘紀錄；分母直接計算每月資料列數。
  monthly_total <- dt[
    ,
    .(
      總搭乘人次 = .N
    ),
    by = 月份
  ]

  # 分子同樣計算符合本縣 TPASS 票種的資料列數。
  monthly_tpass <- dt[
    票種次類型 %chin% tpass_codes,
    .(
      TPASS搭乘人次 = .N
    ),
    by = 月份
  ]

  result <- merge(
    monthly_total,
    monthly_tpass,
    by = "月份",
    all.x = TRUE,
    sort = TRUE
  )

  result[
    is.na(TPASS搭乘人次),
    TPASS搭乘人次 := 0
  ]

  result[
    ,
    TPASS比例 := round(
      TPASS搭乘人次 / 總搭乘人次 * 100,
      1
    )
  ]

  setorder(result, 月份)
  result[]
}


mark_data_completeness <- function(
  result,
  config
) {

  result[
    ,
    資料完整性 := fifelse(
      月份 %chin% config$incomplete_months,
      config$incomplete_note,
      "完整"
    )
  ]

  result[]
}


expected_months <- function() {

  dates <- seq(
    as.Date("2024-06-01"),
    as.Date("2026-06-01"),
    by = "month"
  )

  sprintf(
    "%03d/%02d",
    as.integer(format(dates, "%Y")) - 1911,
    as.integer(format(dates, "%m"))
  )
}


validate_months <- function(
  result,
  county_name
) {

  expected <- expected_months()
  missing <- setdiff(expected, result$月份)
  extra <- setdiff(result$月份, expected)

  valid <- nrow(result) == length(expected) &&
    length(missing) == 0 &&
    length(extra) == 0 &&
    all(result$總搭乘人次 > 0)

  if (!valid) {
    stop(
      paste0(
        county_name,
        " 結果月份不完整。缺少月份：",
        ifelse(
          length(missing) == 0,
          "無",
          paste(missing, collapse = "、")
        ),
        "。請確認輸入檔為完整版本。"
      )
    )
  }

  invisible(TRUE)
}


route_coverage <- function(dt) {
  dt[
    ,
    .(
      最早日期 = min(`資料代表日期(yyyy-MM-dd)`),
      最晚日期 = max(`資料代表日期(yyyy-MM-dd)`),
      資料列數 = .N,
      原始票證筆數加總 = sum(
        原始票證筆數,
        na.rm = TRUE
      )
    ),
    by = .(
      搭乘路線代碼,
      搭乘路線名稱
    )
  ][order(搭乘路線代碼, 搭乘路線名稱)]
}


# -----------------------------------------------------------------------------
# 圖表
# -----------------------------------------------------------------------------

get_chart_font <- function() {
  if (.Platform$OS.type == "windows") {
    windowsFonts(
      msjh = windowsFont("Microsoft JhengHei")
    )
    return("msjh")
  }

  "sans"
}


draw_tpass_chart <- function(
  result,
  county_name,
  output_file,
  label_offset = 2.0
) {

  chart_font <- get_chart_font()

  title_size <- 1.80
  axis_title_size <- 1.50
  axis_tick_size <- 1.10
  value_size <- 0.90

  ymin <- floor(
    min(result$TPASS比例) / 5
  ) * 5

  ymax <- ceiling(
    (max(result$TPASS比例) + label_offset) / 5
  ) * 5

  png(
    output_file,
    width = 15,
    height = 5,
    units = "in",
    res = 300
  )

  on.exit(
    dev.off(),
    add = TRUE
  )

  par(
    family = chart_font,
    mar = c(5.5, 5.5, 5, 3),
    mgp = c(3.1, 1, 0),
    bty = "l"
  )

  plot(
    seq_len(nrow(result)),
    result$TPASS比例,
    type = "n",
    xaxt = "n",
    yaxt = "n",
    xlab = "月份",
    ylab = "TPASS使用比例",
    main = paste0(
      "113年6月至115年6月",
      county_name,
      "市區客運TPASS使用比例折線圖"
    ),
    cex.main = title_size,
    cex.lab = axis_title_size,
    ylim = c(ymin, ymax)
  )

  abline(
    h = seq(ymin, ymax, by = 5),
    col = "grey60",
    lty = 3,
    lwd = 0.6
  )

  abline(
    v = seq(1, nrow(result), by = 4),
    col = "grey60",
    lty = 3,
    lwd = 0.6
  )

  point_x <- seq_len(nrow(result))

  incomplete <- result$資料完整性 != "完整"

  lines(
    x = point_x,
    y = result$TPASS比例,
    type = "l",
    lwd = 1,
    col = "grey30"
  )

  points(
    x = point_x[!incomplete],
    y = result$TPASS比例[!incomplete],
    pch = 16,
    cex = 0.75,
    col = "grey30"
  )

  if (any(incomplete)) {
    points(
      x = point_x[incomplete],
      y = result$TPASS比例[incomplete],
      pch = 2,
      cex = 1.15,
      lwd = 1.3,
      col = "grey30"
    )
  }

  x_ticks <- seq(
    1,
    nrow(result),
    by = 2
  )

  axis(
    side = 1,
    at = x_ticks,
    labels = result$月份[x_ticks],
    las = 1,
    cex.axis = axis_tick_size
  )

  axis(
    side = 2,
    at = seq(ymin, ymax, by = 5),
    las = 1,
    cex.axis = axis_tick_size
  )

  value_labels <- sprintf(
    "%.1f%%",
    result$TPASS比例
  )

  value_x <- seq_len(nrow(result))
  value_y <- result$TPASS比例 + label_offset

  value_width <- strwidth(
    value_labels,
    cex = value_size,
    units = "user"
  )

  value_height <- strheight(
    value_labels,
    cex = value_size,
    units = "user"
  )

  rect(
    xleft = value_x - value_width / 2 - 0.05,
    ybottom = value_y - value_height / 2 - 0.15,
    xright = value_x + value_width / 2 + 0.05,
    ytop = value_y + value_height / 2 + 0.15,
    col = "white",
    border = NA
  )

  text(
    x = value_x,
    y = value_y,
    labels = value_labels,
    cex = value_size
  )
}


# -----------------------------------------------------------------------------
# 主流程
# -----------------------------------------------------------------------------

main <- function() {

  paths <- resolve_paths()

  message("讀取花蓮資料：", paths$hualien_file)
  hualien_raw <- read_bus_data(paths$hualien_file)

  message("讀取臺東資料：", paths$taitung_file)
  taitung_raw <- read_bus_data(paths$taitung_file)

  hualien_raw <- classify_bus_records(
    hualien_raw,
    basename(paths$hualien_file),
    COUNTY_CONFIG$hualien
  )

  taitung_raw <- classify_bus_records(
    taitung_raw,
    basename(paths$taitung_file),
    COUNTY_CONFIG$taitung
  )

  cleaning_overview <- rbindlist(
    list(
      summarize_overview(hualien_raw),
      summarize_overview(taitung_raw)
    ),
    use.names = TRUE,
    fill = TRUE
  )

  excluded_summary <- rbindlist(
    list(
      summarize_excluded(hualien_raw),
      summarize_excluded(taitung_raw)
    ),
    use.names = TRUE,
    fill = TRUE
  )

  setorder(
    cleaning_overview,
    來源檔案,
    研究期間,
    資料分類,
    路線前綴
  )

  setorder(
    excluded_summary,
    來源檔案,
    研究期間,
    月份,
    路線前綴,
    搭乘路線代碼,
    票種次類型
  )

  hualien <- prepare_analysis_data(
    hualien_raw,
    COUNTY_CONFIG$hualien
  )

  taitung <- prepare_analysis_data(
    taitung_raw,
    COUNTY_CONFIG$taitung
  )

  hualien_rate <- calculate_monthly_rate(
    hualien,
    COUNTY_CONFIG$hualien$tpass_codes
  )

  hualien_rate <- mark_data_completeness(
    hualien_rate,
    COUNTY_CONFIG$hualien
  )

  taitung_rate <- calculate_monthly_rate(
    taitung,
    COUNTY_CONFIG$taitung$tpass_codes
  )

  taitung_rate <- mark_data_completeness(
    taitung_rate,
    COUNTY_CONFIG$taitung
  )

  validate_months(
    hualien_rate,
    COUNTY_CONFIG$hualien$county_name
  )

  validate_months(
    taitung_rate,
    COUNTY_CONFIG$taitung$county_name
  )

  hualien_coverage <- route_coverage(hualien)
  taitung_coverage <- route_coverage(taitung)

  fwrite(
    cleaning_overview,
    file.path(paths$output_dir, "資料清理總表.csv"),
    bom = TRUE
  )

  fwrite(
    excluded_summary,
    file.path(paths$output_dir, "排除資料明細摘要.csv"),
    bom = TRUE
  )

  fwrite(
    hualien_rate,
    file.path(paths$output_dir, "花蓮市區客運TPASS比例.csv"),
    bom = TRUE
  )

  fwrite(
    taitung_rate,
    file.path(paths$output_dir, "臺東市區客運TPASS比例.csv"),
    bom = TRUE
  )

  fwrite(
    hualien_coverage,
    file.path(paths$output_dir, "花蓮市區客運路線資料涵蓋情形.csv"),
    bom = TRUE
  )

  fwrite(
    taitung_coverage,
    file.path(paths$output_dir, "臺東市區客運路線資料涵蓋情形.csv"),
    bom = TRUE
  )

  draw_tpass_chart(
    hualien_rate,
    COUNTY_CONFIG$hualien$county_name,
    file.path(paths$output_dir, "花蓮市區客運TPASS比例.png")
  )

  draw_tpass_chart(
    taitung_rate,
    COUNTY_CONFIG$taitung$county_name,
    file.path(paths$output_dir, "臺東市區客運TPASS比例.png")
  )

  message("分析完成。輸出位置：", paths$output_dir)

  invisible(
    list(
      cleaning_overview = cleaning_overview,
      excluded_summary = excluded_summary,
      hualien_rate = hualien_rate,
      taitung_rate = taitung_rate
    )
  )
}


main()
