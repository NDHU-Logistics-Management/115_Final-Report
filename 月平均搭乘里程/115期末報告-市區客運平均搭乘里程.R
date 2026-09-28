# ==============================================================================
# 專案名稱：花東地區市區客運平均搭乘里程分析
# ==============================================================================

# 載入所需套件
library(data.table)
library(lubridate)
library(openxlsx)

# 設定字型
windowsFonts(msjh = windowsFont("Microsoft JhengHei"))

# =========================
# 1. 路徑與環境設定
# =========================
out_base_dir <- "C:/Users/a5014/OneDrive/Emma/115_Final-Report_Output"
FIG_DIR <- file.path(out_base_dir, "images")
TBL_DIR <- file.path(out_base_dir, "output_tables")
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(TBL_DIR, showWarnings = FALSE, recursive = TRUE)

# =========================
# 2. 特殊月份標記設定 (學長修訂 A1/A2/A6)
# =========================
# 業者未提交票證資料之月份：圖上該月資料點改為空心黑邊三角形
FLAG_MONTHS <- list(
  花蓮市區 = c("115/04", "115/05", "115/06")
)

# 花蓮市區 1123 路線改號為 311／311A 之時點：加垂直虛線
VLINE_MONTHS <- list(
  花蓮市區 = "113/11"
)

flag_points <- function(x, y, months, flag, cex = 1.3) {
  if (is.null(flag)) return(invisible(NULL))
  idx <- which(months %in% flag)
  if (length(idx) == 0) return(invisible(NULL))
  points(x[idx], y[idx], pch = 21, col = "white", bg = "white", cex = cex * 0.85)
  points(x[idx], y[idx], pch = 24, col = "black", bg = "white", cex = cex, lwd = 1.3)
  invisible(idx)
}

flag_vline <- function(months, vline) {
  if (is.null(vline)) return(invisible(NULL))
  idx <- which(months %in% vline)
  if (length(idx) == 0) return(invisible(NULL))
  abline(v = idx, lty = 2, lwd = 1.2, col = "grey30")
  invisible(idx)
}

# =========================
# 3. 讀取資料
# =========================
cat("正在讀取資料...\n")
花蓮市區 <- fread("C:/Users/a5014/OneDrive/Emma/原始資料/花蓮縣公車.txt")
臺東市區 <- fread("C:/Users/a5014/OneDrive/Emma/原始資料/臺東縣公車.txt")
花蓮市區站距 <- fread("C:/Users/a5014/OneDrive/Emma/原始資料/花蓮縣市區客運站間距離資料.csv")
臺東市區站距 <- fread("C:/Users/a5014/OneDrive/Emma/原始資料/臺東縣市區客運站間距離資料.csv")

# =========================
# 4. 市區客運異常資料清理 (學長邏輯)
# =========================
# 日期格式化
date_col_hua <- grep("資料代表日期", colnames(花蓮市區), value = TRUE)[1]
date_col_ttt <- grep("資料代表日期", colnames(臺東市區), value = TRUE)[1]
花蓮市區[, `資料代表日期(yyyy-MM-dd)` := as.Date(get(date_col_hua))]
臺東市區[, `資料代表日期(yyyy-MM-dd)` := as.Date(get(date_col_ttt))]

# 路線名稱格式化
花蓮市區[, 搭乘路線名稱 := trimws(as.character(搭乘路線名稱))]
臺東市區[, 搭乘路線名稱 := trimws(as.character(搭乘路線名稱))]

# 排除花蓮市區異常路線
hualien_abnormal_routes <- c("50", "51", "52", "53", "71", "72", "73", "81", "83", "綠線")
花蓮市區_clean <- 花蓮市區[!搭乘路線名稱 %in% hualien_abnormal_routes]

# 排除臺東市區異常日期
taitung_abnormal_dates <- as.Date(c("2026-05-24", "2026-05-25"))
taitung_normal_routes <- c("101", "201", "202", "203")
臺東市區_clean <- 臺東市區[!(`資料代表日期(yyyy-MM-dd)` %in% taitung_abnormal_dates) | 搭乘路線名稱 %in% taitung_normal_routes]

# TPASS 重複列去除 (學長修訂 A7)
remove_duplicate_rows <- function(x, label) {
  x <- copy(x)
  flag_col <- grep("^是否包含異常值", names(x), value = TRUE)
  if (length(flag_col) == 1) {
    x[, A7_key := paste(卡號, 搭乘路線代碼, 上車站牌代碼, as.character(刷卡上車時間), sep = "|")]
    ok_keys <- x[get(flag_col) == 0, unique(A7_key)]
    x[, A7_dup := !is.na(get(flag_col)) & get(flag_col) == 1 &
        substr(as.character(刷卡下車時間), 1, 4) == "1970" &
        !is.na(卡號) & 卡號 != "" & A7_key %chin% ok_keys]
    x <- x[A7_dup == FALSE]
    x[, c("A7_key", "A7_dup") := NULL]
  }
  x[]
}
花蓮市區_clean <- remove_duplicate_rows(花蓮市區_clean, "花蓮市區")
臺東市區_clean <- remove_duplicate_rows(臺東市區_clean, "臺東市區")

# =========================
# 5. 核心：里程計算函數 (嚴格依學長邏輯)
# =========================
normalize_stop_code = function(x) {
  x = trimws(toupper(as.character(x)))
  x = sub("^HUA", "", x)
  x[x %in% c("", "NA", "N/A", "NULL")] = NA_character_
  return(x)
}

build_distance_index = function(distance_data) {
  x = copy(as.data.table(distance_data))
  x[, 搭乘路線名稱 := trimws(as.character(搭乘路線名稱))]
  x[, 搭乘附屬路線名稱 := trimws(as.character(搭乘附屬路線名稱))]
  x[, 搭乘公車路線方向 := as.character(搭乘公車路線方向)]
  x[, 站牌代碼_標準 := normalize_stop_code(站牌代碼)]
  x[, 站序資料 := as.numeric(站序資料)]
  x[, 站間距離 := as.numeric(站間距離)]
  x[is.na(站間距離), 站間距離 := 0]
  setorder(x, 搭乘路線名稱, 搭乘附屬路線名稱, 搭乘公車路線方向, 站序資料)
  x[, 累積里程 := cumsum(站間距離), by = .(搭乘路線名稱, 搭乘附屬路線名稱, 搭乘公車路線方向)]
  x = x[, .(搭乘路線名稱, 搭乘附屬路線名稱, 搭乘公車路線方向, 站牌代碼_標準, 站序資料, 累積里程)]
  return(unique(x))
}

build_sequence_index = function(distance_index) {
  x = distance_index[!is.na(站序資料)]
  result = x[, .(累積里程種類數 = uniqueN(累積里程),
                 站序補配里程 = if (uniqueN(累積里程) == 1) first(累積里程) else NA_real_),
             by = .(搭乘路線名稱, 搭乘附屬路線名稱, 搭乘公車路線方向, 站序資料)]
  return(result)
}

match_one_side = function(ticket, distance_index, stop_col, seq_col, side_name) {
  t = ticket[, .(ticket_id, 搭乘路線名稱, 搭乘附屬路線名稱, 搭乘公車路線方向,
                 配對站牌代碼 = normalize_stop_code(get(stop_col)), 票證站序 = as.numeric(get(seq_col)))]
  d = copy(distance_index)
  setnames(d, old = c("站牌代碼_標準", "站序資料", "累積里程"), new = c("配對站牌代碼", "距離表站序", "候選累積里程"))
  
  stop_candidates = merge(t[!is.na(配對站牌代碼)], d[!is.na(配對站牌代碼)],
                          by = c("搭乘路線名稱", "搭乘附屬路線名稱", "搭乘公車路線方向", "配對站牌代碼"),
                          allow.cartesian = TRUE, sort = FALSE)
  stop_candidates[, 站序差 := fifelse(!is.na(票證站序) & 票證站序 != -99 & !is.na(距離表站序), abs(票證站序 - 距離表站序), NA_real_)]
  
  if (nrow(stop_candidates) > 0) {
    stop_match = stop_candidates[, {
      if (!is.na(票證站序[1]) && 票證站序[1] != -99) {
        min_diff = min(站序差, na.rm = TRUE)
        z = .SD[站序差 == min_diff]
        if (uniqueN(z$候選累積里程) == 1) {
          list(配對里程 = z$候選累積里程[1], 配對方法 = if (min_diff == 0) "站牌代碼+站序精確" else "站牌代碼+站序輔助", 配對站序差 = min_diff)
        } else {
          list(配對里程 = NA_real_, 配對方法 = "站牌代碼候選歧義", 配對站序差 = min_diff)
        }
      } else {
        if (uniqueN(候選累積里程) == 1) {
          list(配對里程 = first(候選累積里程), 配對方法 = "唯一站牌代碼", 配對站序差 = NA_real_)
        } else {
          list(配對里程 = NA_real_, 配對方法 = "站牌代碼候選歧義", 配對站序差 = NA_real_)
        }
      }
    }, by = ticket_id]
  } else {
    stop_match = data.table(ticket_id = integer(), 配對里程 = numeric(), 配對方法 = character(), 配對站序差 = numeric())
  }
  
  result = merge(t, stop_match, by = "ticket_id", all.x = TRUE, sort = FALSE)
  sequence_index = build_sequence_index(distance_index)
  seq_temp = result[is.na(配對里程) & !is.na(票證站序) & 票證站序 != -99,
                    .(ticket_id, 搭乘路線名稱, 搭乘附屬路線名稱, 搭乘公車路線方向, 站序資料 = 票證站序)]
  
  if (nrow(seq_temp) > 0) {
    seq_match = merge(seq_temp, sequence_index, by = c("搭乘路線名稱", "搭乘附屬路線名稱", "搭乘公車路線方向", "站序資料"), all.x = TRUE, sort = FALSE)
    result = merge(result, seq_match[, .(ticket_id, 站序補配里程)], by = "ticket_id", all.x = TRUE, sort = FALSE)
    result[is.na(配對里程) & !is.na(站序補配里程), `:=`(配對里程 = 站序補配里程, 配對方法 = "站序精確補配", 配對站序差 = 0)]
  } else {
    result[, 站序補配里程 := NA_real_]
  }
  
  result[is.na(配對里程) & is.na(配對方法), 配對方法 := "未配對"]
  output = result[, .(ticket_id, 配對里程, 配對方法, 配對站序差)]
  setnames(output, old = c("配對里程", "配對方法", "配對站序差"), new = c(paste0(side_name, "里程"), paste0(side_name, "配對方法"), paste0(side_name, "站序差")))
  return(output)
}

calculate_monthly_mileage = function(ticket_data, distance_index) {
  ticket = copy(as.data.table(ticket_data))
  ticket[, 搭乘路線名稱 := trimws(as.character(搭乘路線名稱))]
  ticket[, 搭乘附屬路線名稱 := trimws(as.character(搭乘附屬路線名稱))]
  ticket[, 搭乘公車路線方向 := as.character(搭乘公車路線方向)]
  ticket[, 上車站牌代碼 := as.character(上車站牌代碼)]
  ticket[, 下車站牌代碼 := as.character(下車站牌代碼)]
  ticket[, 上車計費站序資料 := as.numeric(上車計費站序資料)]
  ticket[, 下車站牌站序 := as.numeric(下車站牌站序)]
  ticket[, 原始票證筆數 := as.numeric(原始票證筆數)]
  
  # 統一使用這組日期名稱
  if ("資料代表日期(yyyy-MM-dd)" %in% names(ticket)) {
    ticket[, `資料代表日期(yyyy-MM-dd)` := as.Date(`資料代表日期(yyyy-MM-dd)`)]
  }
  
  ticket = ticket[`資料代表日期(yyyy-MM-dd)` >= as.Date("2024-06-01") & `資料代表日期(yyyy-MM-dd)` <= as.Date("2026-06-30")]
  ticket = ticket[!is.na(原始票證筆數) & 原始票證筆數 > 0]
  ticket[, ticket_id := .I]
  
  on_match = match_one_side(ticket, distance_index, "上車站牌代碼", "上車計費站序資料", "上車")
  off_match = match_one_side(ticket, distance_index, "下車站牌代碼", "下車站牌站序", "下車")
  
  ticket = merge(ticket, on_match, by = "ticket_id", all.x = TRUE, sort = FALSE)
  ticket = merge(ticket, off_match, by = "ticket_id", all.x = TRUE, sort = FALSE)
  setorder(ticket, ticket_id)
  
  ticket[, 里程_公尺 := fifelse(!is.na(上車里程) & !is.na(下車里程), abs(下車里程 - 上車里程), NA_real_)]
  ticket[, 月份 := sprintf("%03d/%02d", as.integer(format(`資料代表日期(yyyy-MM-dd)`, "%Y")) - 1911, as.integer(format(`資料代表日期(yyyy-MM-dd)`, "%m")))]
  
  result = ticket[!is.na(里程_公尺), .(搭乘人次 = .N, 總延人公里 = sum(里程_公尺, na.rm = TRUE) / 1000), by = 月份]
  result[, 平均搭乘里程_公里 := 總延人公里 / 搭乘人次]
  result[, `:=`(總延人公里 = round(總延人公里, 2), 平均搭乘里程_公里 = round(平均搭乘里程_公里, 2))]
  setorder(result, 月份)
  
  return(list(monthly = result))
}

# 執行計算
cat("建立站距索引...\n")
花蓮市區站距_index = build_distance_index(花蓮市區站距)
臺東市區站距_index = build_distance_index(臺東市區站距)

cat("計算月平均搭乘里程...\n")
花蓮市區_月平均搭乘里程 = calculate_monthly_mileage(花蓮市區_clean, 花蓮市區站距_index)$monthly
臺東市區_月平均搭乘里程 = calculate_monthly_mileage(臺東市區_clean, 臺東市區站距_index)$monthly

# =========================
# 6. 畫圖函數 (學長標準)
# =========================
plot_monthly_average_mileage = function(data, title_main, filename, flag = NULL, vline = NULL) {
  color = gray.colors(2)
  ymin = min(data$平均搭乘里程_公里, na.rm = TRUE)
  ymax = max(data$平均搭乘里程_公里, na.rm = TRUE)
  
  if (ymin == ymax) {
    y_lower = ymin * 0.9; y_upper = ymax * 1.1
  } else {
    y_lower = ymin * 0.9; y_upper = ymax * 1.2
  }
  if (!is.finite(y_lower) || !is.finite(y_upper) || y_lower == y_upper) {
    y_lower = 0; y_upper = max(1, ymax + 1)
  }
  
  png(filename = file.path(FIG_DIR, filename), width = 15, height = 5, units = "in", res = 300)
  par(family = "msjh", mar = c(5, 6, 4, 8), mgp = c(3.5, 0.8, 0), bty = "l", cex.lab = 1.5)
  
  plot(x = 1:nrow(data), y = data$平均搭乘里程_公里, type = "o", lwd = 2, pch = 16, col = color[1],
       xlab = "月份", ylab = "", ylim = c(y_lower, y_upper),
       cex.main = 2, cex.lab = 2, cex.axis = 1.5, cex = 1.5, xaxt = "n", yaxt = "n", bty = "n")
  
  title(main = title_main, cex.main = 2, adj = 0)
  axis(side = 1, at = 1:nrow(data), labels = data$月份, cex.axis = 1.25)
  
  y_ticks = pretty(c(y_lower, y_upper), n = 5)
  axis(side = 2, at = y_ticks, labels = round(y_ticks, 1), las = 1, cex.axis = 1.5)
  
  abline(h = y_ticks, col = "grey80", lty = 1, lwd = 0.8)
  abline(v = 1:nrow(data), col = "grey90", lty = 1, lwd = 0.6)
  
  flag_vline(data$月份, vline)
  lines(x = 1:nrow(data), y = data$平均搭乘里程_公里, type = "o", lwd = 2, pch = 16, col = color[1])
  flag_points(1:nrow(data), data$平均搭乘里程_公里, data$月份, flag, cex = 1.9)
  
  text(x = 1:nrow(data), y = data$平均搭乘里程_公里 + diff(range(c(y_lower, y_upper))) * 0.025,
       labels = sprintf("%.1f", data$平均搭乘里程_公里), pos = 3, cex = 1.1, col = "black")
  
  legend("topright", legend = "公里", col = color[1], lwd = 2, pch = 16, bty = "n", inset = c(-0.1, 0), xpd = TRUE, cex = 1.5)
  dev.off()
}

# =========================
# 7. 匯出圖表與 Excel
# =========================
cat("正在匯出圖表與Excel...\n")

# 輸出圖表
plot_monthly_average_mileage(
  花蓮市區_月平均搭乘里程,
  "113年6月至115年6月花蓮縣市區客運平均搭乘里程折線圖",
  "11306-11506花蓮縣市區客運平均搭乘里程折線圖_2.png",
  flag = FLAG_MONTHS$花蓮市區,
  vline = VLINE_MONTHS$花蓮市區
)

plot_monthly_average_mileage(
  臺東市區_月平均搭乘里程,
  "113年6月至115年6月臺東縣市區客運平均搭乘里程折線圖",
  "11306-11506臺東縣市區客運平均搭乘里程折線圖_2.png"
)

# 輸出 Excel
mileage_table_list = list(
  "花蓮市區" = 花蓮市區_月平均搭乘里程,
  "臺東市區" = 臺東市區_月平均搭乘里程
)
write.xlsx(mileage_table_list, file = file.path(TBL_DIR, "市區客運月平均搭乘里程.xlsx"), overwrite = TRUE)

cat("✅ 執行完畢！檔案已輸出至：", out_base_dir, "\n")