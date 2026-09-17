# -*- coding: utf-8 -*-
"""
把 實作測試/115期末.Rmd 的複本改成修訂版。每個替換都檢查命中次數，
不符預期就中止，避免靜默漏改。只做三類修改：
  P1 路徑改相對路徑、輸出改到 revise/output
  P2 C4：sum(原始票證筆數) → .N（一列一人次）
  P3 A1/A2/A6 圖上標記：空心三角形、113/11 垂直虛線

用法：先把 實作測試/115期末.Rmd 複製為 revise/code/115期末-修訂.Rmd，再
      python patch-rmd.py
"""
import re, sys, io
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding="utf-8")

SRC = "115期末-修訂.Rmd"
raw = open(SRC, "rb").read().decode("utf-8")
txt = raw.replace("\r\n", "\n")
log = []

def sub(pattern, repl, expect, label, literal=False):
    global txt
    if literal:
        n = txt.count(pattern)
        new = txt.replace(pattern, repl)
    else:
        new, n = re.subn(pattern, repl, txt)
    ok = (n == expect)
    log.append(f"{'OK ' if ok else 'FAIL'} {label}: {n} 處（預期 {expect}）")
    if not ok:
        print("\n".join(log)); sys.exit(f"中止：{label}")
    txt = new

# ------------------------------------------------------------------
# P1 路徑
# ------------------------------------------------------------------
P1_SETUP = '''# [修訂] 路徑改為相對於 repo 根目錄。run-all.R 會先定義 REPO_ROOT、FIG_DIR、TBL_DIR；
# 若直接在 RStudio 逐段執行，預設 repo 根目錄為本檔上兩層。
if (!exists("REPO_ROOT")) REPO_ROOT <- normalizePath(file.path(getwd(), "..", ".."))
if (!exists("FIG_DIR"))   FIG_DIR   <- file.path(REPO_ROOT, "revise", "output", "fig")
if (!exists("TBL_DIR"))   TBL_DIR   <- file.path(REPO_ROOT, "revise", "output", "tbl")
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(TBL_DIR, showWarnings = FALSE, recursive = TRUE)

data_path <- file.path(REPO_ROOT, "data")

# [修訂 A1/A2] 業者未提交票證資料之月份：圖上該月資料點改為空心黑邊三角形
FLAG_MONTHS <- list(
  臺東公路 = "114/11",                        # A1 東台灣客運（0816）
  花蓮市區 = c("115/04", "115/05", "115/06")  # A2 太魯閣客運（0412）
)

# [修訂 A6] 花蓮市區 1123 路線改號為 311／311A 之時點：圖 2.1.1 加垂直虛線
VLINE_MONTHS <- list(
  花蓮市區 = "113/11"
)

# 在既有折線上覆蓋空心三角形。先鋪一個白色圓點蓋住原本的實心點，再畫三角形。
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
}'''

sub('data_path <- "C:/Users/user/Desktop/115_Final-Report/data"', P1_SETUP,
    1, "P1 data_path 與全域設定", literal=True)

sub('library(data.table)\n\n# =========================\n# 路徑',
    'library(data.table)\nlibrary(lubridate)   # [修訂] days_in_month() 需要，原檔未載入\n\n# =========================\n# 路徑',
    1, "P1 library(lubridate)", literal=True)

sub(r'png\(\n    filename,\n    width = 15,',
    'png(\n    file.path(FIG_DIR, filename),\n    width = 15,',
    1, "P1 png 檔名（公路 TPASS）")
sub(r'filename = filename,',
    'filename = file.path(FIG_DIR, filename),',
    5, "P1 png 檔名（其餘 5 個作圖函數）")
sub(r'file\s*=\s*\n?\s*"([^"]+\.xlsx)",',
    r'file = file.path(TBL_DIR, "\1"),',
    4, "P1 xlsx 輸出路徑")

# ------------------------------------------------------------------
# P2 C4：一列一人次
# ------------------------------------------------------------------
sub(r'(總搭乘人次|TPASS搭乘人次) = sum\(\s*原始票證筆數,\s*na\.rm = TRUE\s*\)',
    r'\1 = .N',
    4, "P2 TPASS 比例：總搭乘人次／TPASS搭乘人次")
sub(r'搭乘人次 =\s*sum\(\s*原始票證筆數,\s*na\.rm = TRUE\s*\),\s*總延人公里 =\s*sum\(\s*里程_公尺 \*\s*原始票證筆數,\s*na\.rm = TRUE\s*\) / 1000',
    '搭乘人次 = .N,\n\n      總延人公里 = sum(里程_公尺, na.rm = TRUE) / 1000',
    1, "P2 里程：搭乘人次／總延人公里")
sub(r'原始載客人次 =\s*sum\(\s*原始票證筆數,\s*na\.rm = TRUE\s*\),',
    '原始載客人次 = .N,',
    1, "P2 里程檢查表：原始載客人次")
sub(r'成功配對載客人次 =\s*sum\(\s*原始票證筆數\[\s*!is\.na\(\s*里程_公尺\s*\)\s*\],\s*na\.rm = TRUE\s*\)',
    '成功配對載客人次 = sum(!is.na(里程_公尺))',
    1, "P2 里程檢查表：成功配對載客人次")
sub(r'載客人次 =\s*sum\(\s*原始票證筆數,\s*na\.rm = TRUE\s*\)',
    '載客人次 = .N',
    2, "P2 里程配對方法統計：載客人次")
sub(r'該月總使用次數 =\s*sum\(\s*原始票證筆數,\s*na\.rm = TRUE\s*\),',
    '該月總使用次數 = .N,',
    1, "P2 平均每日：該月總使用次數")
sub(r'每人每月使用次數 =\s*sum\(\s*原始票證筆數,\s*na\.rm = TRUE\s*\)',
    '每人每月使用次數 = .N',
    1, "P2 搭乘次數區間：每人每月使用次數")

# 註解同步（不影響計算）
sub('# 8. 依原始票證筆數加權',
    '# 8. 一列計為一人次（原始票證筆數為平臺重複計數，非搭乘次數，不加權）[修訂 C4]',
    1, "P2 註解：里程 8.", literal=True)
sub('# sum(每筆搭乘里程 × 原始票證筆數)', '# sum(每筆搭乘里程)', 1, "P2 註解：總延人公里", literal=True)
sub('# 載客人數 =\n# sum(原始票證筆數)', '# 載客人數 =\n# 紀錄筆數 .N', 1, "P2 註解：載客人數", literal=True)
sub('# 該月總使用次數 =\n# sum(原始票證筆數)', '# 該月總使用次數 =\n# 紀錄筆數 .N', 1, "P2 註解：該月總使用次數", literal=True)
sub('# 每位乘客每月使用次數 =\n# sum(原始票證筆數)', '# 每位乘客每月使用次數 =\n# 該卡該月紀錄筆數 .N', 1, "P2 註解：每位乘客每月", literal=True)

# ------------------------------------------------------------------
# P3 作圖函數加 flag / vline 參數
# ------------------------------------------------------------------
LINES_TPASS = '''  lines(
    x = 1:nrow(data),
    y = data$TPASS比例,
    type = "o",
    pch = 16,
    lwd = 1.5,
    cex = 1,
    col = "grey30"
  )'''
LINES_MILEAGE = '''  lines(
    x = 1:nrow(data),
    y = data$平均搭乘里程_公里,
    type = "o",
    lwd = 2,
    pch = 16,
    col = color[1]
  )'''
LINES_DAILY = '''  lines(
    x,
    y,
    type = "o",
    pch = 16,
    lwd = 1.5,
    cex = 1,
    col = "grey30"
  )'''
LINES_FREQ3 = '''  lines(
    x, y3,
    type = "o",
    pch = 16,
    lty = 1,
    lwd = 1.7,
    cex = 1,
    col = color[3]
  )'''
LINES_TRANSFER3 = '''  lines(
    x,
    y3,
    type = "o",
    pch = 16,
    lty = 1,
    lwd = 1.7,
    cex = 1,
    col = color[3]
  )'''
FLAG1 = "\n\n  # [修訂 A1/A2] 業者資料不完整月份\n  flag_points(%s, %s, data$月份, flag, cex = %s)"
FLAG3 = ("\n\n  # [修訂 A1/A2] 業者資料不完整月份（三條線都標）\n"
         "  flag_points(x, y1, data$月份, flag, cex = 1.3)\n"
         "  flag_points(x, y2, data$月份, flag, cex = 1.3)\n"
         "  flag_points(x, y3, data$月份, flag, cex = 1.3)")

sub('plot_tpass_ratio <- function(\n  data,\n  title,\n  filename\n) {',
    'plot_tpass_ratio <- function(\n  data,\n  title,\n  filename,\n  flag = NULL\n) {',
    2, "P3 plot_tpass_ratio 簽名（公路、市區兩版）", literal=True)
sub(LINES_TPASS, LINES_TPASS + FLAG1 % ("1:nrow(data)", "data$TPASS比例", "1.3"),
    2, "P3 plot_tpass_ratio 加三角形", literal=True)

sub('plot_monthly_average_mileage = function(\n  data,\n  title_main,\n  filename\n) {',
    'plot_monthly_average_mileage = function(\n  data,\n  title_main,\n  filename,\n  flag = NULL,\n  vline = NULL\n) {',
    1, "P3 plot_monthly_average_mileage 簽名", literal=True)
# 原檔此區塊的引數之間夾有空行，用正規式比對
MILEAGE_REDRAW = re.compile(
    r'  lines\(\s*x = 1:nrow\(data\),\s*y = data\$平均搭乘里程_公里,\s*type = "o",\s*lwd = 2,\s*pch = 16,\s*col = color\[1\]\s*\)')
n_m = len(MILEAGE_REDRAW.findall(txt))
if n_m != 1:
    log.append(f"FAIL P3 plot_monthly_average_mileage 加虛線與三角形: {n_m} 處（預期 1）"); print("\n".join(log)); sys.exit(1)
txt = MILEAGE_REDRAW.sub(
    lambda m: "  # [修訂 A6] 路線改號時點\n  flag_vline(data$月份, vline)\n\n" + LINES_MILEAGE
              + FLAG1 % ("1:nrow(data)", "data$平均搭乘里程_公里", "1.9"),
    txt)
log.append("OK  P3 plot_monthly_average_mileage 加虛線與三角形: 1 處（預期 1）")

sub('plot_avg_daily_use = function(\n  data,\n  title,\n  filename\n) {',
    'plot_avg_daily_use = function(\n  data,\n  title,\n  filename,\n  flag = NULL\n) {',
    1, "P3 plot_avg_daily_use 簽名", literal=True)
sub(LINES_DAILY, LINES_DAILY + FLAG1 % ("x", "y", "1.3"),
    1, "P3 plot_avg_daily_use 加三角形", literal=True)

sub('plot_frequency_count = function(\n  data,\n  title,\n  filename\n) {',
    'plot_frequency_count = function(\n  data,\n  title,\n  filename,\n  flag = NULL\n) {',
    1, "P3 plot_frequency_count 簽名", literal=True)
sub(LINES_FREQ3, LINES_FREQ3 + FLAG3, 1, "P3 plot_frequency_count 加三角形", literal=True)

sub('plot_average_transfer = function(\n  data,\n  title,\n  filename\n) {',
    'plot_average_transfer = function(\n  data,\n  title,\n  filename,\n  flag = NULL\n) {',
    1, "P3 plot_average_transfer 簽名", literal=True)
sub(LINES_TRANSFER3, LINES_TRANSFER3 + FLAG3, 1, "P3 plot_average_transfer 加三角形", literal=True)

# 呼叫端：在 png 檔名後補上 flag／vline
calls = {
    "臺東公路客運TPASS比例.png":                         "flag = FLAG_MONTHS$臺東公路",
    "花蓮市區客運TPASS比例.png":                         "flag = FLAG_MONTHS$花蓮市區",
    "臺東公路客運月平均搭乘里程.png":                    "flag = FLAG_MONTHS$臺東公路",
    "花蓮市區客運月平均搭乘里程.png":                    "flag = FLAG_MONTHS$花蓮市區,\n  vline = VLINE_MONTHS$花蓮市區",
    "臺東公路客運平均每日使用次數.png":                  "flag = FLAG_MONTHS$臺東公路",
    "花蓮市區客運平均每日使用次數.png":                  "flag = FLAG_MONTHS$花蓮市區",
    "臺東公路客運乘客各搭乘次數區間月人數變化.png":      "flag = FLAG_MONTHS$臺東公路",
    "花蓮市區客運乘客各搭乘次數區間月人數變化.png":      "flag = FLAG_MONTHS$花蓮市區",
    "臺東公路客運TPASS乘客各搭乘次數區間月人數變化.png": "flag = FLAG_MONTHS$臺東公路",
    "花蓮市區客運TPASS乘客各搭乘次數區間月人數變化.png": "flag = FLAG_MONTHS$花蓮市區",
    "臺東地區客運平均轉乘次數折線圖.png":                "flag = FLAG_MONTHS$臺東公路",
    "花蓮地區客運平均轉乘次數折線圖.png":                "flag = FLAG_MONTHS$花蓮市區",
}
for fname, arg in calls.items():
    pat = re.compile(r'("' + re.escape(fname) + r'")\s*\n\)')
    n = len(pat.findall(txt))
    if n != 1:
        log.append(f"FAIL P3 呼叫端 {fname}: {n} 處（預期 1）"); print("\n".join(log)); sys.exit(1)
    txt = pat.sub(lambda m: m.group(1) + ",\n  " + arg + "\n)", txt)
    log.append(f"OK  P3 呼叫端 {fname}: 1 處（預期 1）")

# ------------------------------------------------------------------
# P4 花蓮市區第 3 章改用替代指標（審查人 2026-09-12 裁示，甲案）
#   花蓮市區卡號逐日更新（資料等級 TO2A），無法計算標準「平均每日使用次數」，
#   改呈現「單日平均使用次數」= 該月總使用次數 ÷ 使用人合計日數；
#   搭乘次數區間圖表只在標題加「單日」，Y 軸維持「人數」，區間分法不變。
#   其餘三組資料的圖表、分頁完全不動。
# ------------------------------------------------------------------
# 4a. plot_avg_daily_use 加 ycol / ylab 參數（預設值 = 原行為）
sub('plot_avg_daily_use = function(\n  data,\n  title,\n  filename,\n  flag = NULL\n) {',
    'plot_avg_daily_use = function(\n  data,\n  title,\n  filename,\n  flag = NULL,\n  ycol = "平均每日使用次數",\n  ylab = "平均每日使用次數"\n) {',
    1, "P4 plot_avg_daily_use 簽名", literal=True)
sub('  y = data$平均每日使用次數\n', '  y = data[[ycol]]\n', 1, "P4 plot_avg_daily_use y", literal=True)
sub('    ylab = "平均每日使用次數",\n', '    ylab = ylab,\n', 1, "P4 plot_avg_daily_use ylab", literal=True)

# 4b. Y 軸刻度：標籤維持 2 位小數（審查人裁示）；刻度細到 0.005 時 2 位小數會重複，改取較疏的刻度
sub('''  y_ticks = pretty(
    c(
      y_lower,
      ymax
    ),
    n = 5
  )
''', '''  y_ticks = pretty(
    c(
      y_lower,
      ymax
    ),
    n = 5
  )

  # [修訂] 軸標籤維持 2 位小數；刻度若細到 2 位小數會重複（如 0.06、0.06），改取較疏的刻度
  if (anyDuplicated(sprintf("%.2f", y_ticks))) {
    y_ticks = pretty(c(y_lower, ymax), n = 3)
  }
''', 1, "P4 plot_avg_daily_use 刻度去重", literal=True)

# 4c. plot_frequency_count 加 ylab 參數
sub('plot_frequency_count = function(\n  data,\n  title,\n  filename,\n  flag = NULL\n) {',
    'plot_frequency_count = function(\n  data,\n  title,\n  filename,\n  flag = NULL,\n  ylab = "人數"\n) {',
    1, "P4 plot_frequency_count 簽名", literal=True)
sub('    ylab = "人數",\n', '    ylab = ylab,\n', 1, "P4 plot_frequency_count ylab", literal=True)

# 4d. 花蓮市區替代指標的計算
sub('''花蓮市區_平均每日使用次數 =
  calculate_avg_daily_use(
    花蓮市區_clean
  )
''', '''花蓮市區_平均每日使用次數 =
  calculate_avg_daily_use(
    花蓮市區_clean
  )


# [修訂 B2] 花蓮市區卡號逐日更新（資料等級 TO2A），「使用人數」實為使用人合計日數，
# 標準指標「平均每日使用次數」無法計算，改以「單日平均使用次數」呈現：
#   單日平均使用次數 = 該月總使用次數 ÷ 使用人合計日數（不重複的 卡號 × 日期）
calculate_per_person_day = function(data) {

  x = prepare_usage_data(data)

  monthly = x[
    ,
    .(
      該月總使用次數 = .N,
      使用人合計日數 = uniqueN(
        paste(卡號, `資料代表日期(yyyy-MM-dd)`)
      )
    ),
    by = 西元年月
  ]

  result = merge(
    copy(month_table)[, .(西元年月, 月份)],
    monthly,
    by = "西元年月",
    all.x = TRUE
  )

  result[
    ,
    單日平均使用次數 := round(
      該月總使用次數 / 使用人合計日數,
      2
    )
  ]

  result[
    ,
    .(
      月份,
      該月總使用次數,
      使用人合計日數,
      單日平均使用次數
    )
  ]
}

花蓮市區_單日平均使用次數 =
  calculate_per_person_day(
    花蓮市區_clean
  )

print(花蓮市區_單日平均使用次數)
''', 1, "P4 花蓮市區 每人日平均搭乘次數 計算", literal=True)

# 4e. 呼叫端：圖 3.1.1 / 3.1.2 / 3.1.3
sub('''plot_avg_daily_use(
  花蓮市區_平均每日使用次數,
  "113年6月至115年6月花蓮縣市區客運平均每日使用次數",
  "花蓮市區客運平均每日使用次數.png",
  flag = FLAG_MONTHS$花蓮市區
)''', '''plot_avg_daily_use(
  花蓮市區_單日平均使用次數,
  "113年6月至115年6月花蓮縣市區客運單日平均使用次數",
  "花蓮市區客運單日平均使用次數.png",
  flag = FLAG_MONTHS$花蓮市區,
  ycol = "單日平均使用次數",
  ylab = "單日平均使用次數"
)''', 1, "P4 圖 3.1.1 呼叫端", literal=True)
sub('''plot_frequency_count(
  花蓮市區_搭乘區間$table,
  "113年6月至115年6月花蓮縣市區客運乘客各搭乘次數區間月人數變化",
  "花蓮市區客運乘客各搭乘次數區間月人數變化.png",
  flag = FLAG_MONTHS$花蓮市區
)''', '''plot_frequency_count(
  花蓮市區_搭乘區間$table,
  "113年6月至115年6月花蓮縣市區客運乘客單日搭乘次數區間月人數變化",
  "花蓮市區客運乘客單日搭乘次數區間月人數變化.png",
  flag = FLAG_MONTHS$花蓮市區
)''', 1, "P4 圖 3.1.2 呼叫端", literal=True)
sub('''plot_frequency_count(
  花蓮市區_TPASS搭乘區間$table,
  "113年6月至115年6月花蓮縣市區客運TPASS乘客各搭乘次數區間月人數變化",
  "花蓮市區客運TPASS乘客各搭乘次數區間月人數變化.png",
  flag = FLAG_MONTHS$花蓮市區
)''', '''plot_frequency_count(
  花蓮市區_TPASS搭乘區間$table,
  "113年6月至115年6月花蓮縣市區客運TPASS乘客單日搭乘次數區間月人數變化",
  "花蓮市區客運TPASS乘客單日搭乘次數區間月人數變化.png",
  flag = FLAG_MONTHS$花蓮市區
)''', 1, "P4 圖 3.1.3 呼叫端", literal=True)

# 4f. xlsx 分頁：平均每日_花蓮市區 → 每人日_花蓮市區；區間兩分頁 使用人數 → 使用人日數
sub('''  "平均每日_花蓮市區" =
    花蓮市區_平均每日使用次數,
''', '''  "單日平均_花蓮市區" =
    花蓮市區_單日平均使用次數,
''', 1, "P4 分頁 平均每日_花蓮市區 → 單日平均_花蓮市區", literal=True)
sub('''  "全部乘客_花蓮市區" =
    clean_frequency_table(
      花蓮市區_搭乘區間$table
    ),
''', '''  "全部乘客_花蓮市區" =
    setnames(
      clean_frequency_table(
        花蓮市區_搭乘區間$table
      ),
      "使用人數",
      "使用人合計日數"
    ),
''', 1, "P4 分頁 全部乘客_花蓮市區 使用人數→使用人合計日數", literal=True)
sub('''  "TPASS_花蓮市區" =
    clean_frequency_table(
      花蓮市區_TPASS搭乘區間$table
    ),
''', '''  "TPASS_花蓮市區" =
    setnames(
      clean_frequency_table(
        花蓮市區_TPASS搭乘區間$table
      ),
      "使用人數",
      "使用人合計日數"
    ),
''', 1, "P4 分頁 TPASS_花蓮市區 使用人數→使用人合計日數", literal=True)

# ------------------------------------------------------------------
# P5 四張「平均每日／單日平均」折線圖取消資料點數值標籤（審查人 2026-09-13 裁示：圖下有表格）
#    只動 plot_avg_daily_use，其餘作圖函數不變。
# ------------------------------------------------------------------
DAILY_LABELS = re.compile(
    r'  text\(\s*x = x,\s*y =\s*y \+\s*y_range \* 0\.07,\s*labels = sprintf\(\s*"%\.2f",\s*y\s*\),\s*cex = 0\.9\s*\)')
n_l = len(DAILY_LABELS.findall(txt))
if n_l != 1:
    log.append(f"FAIL P5 plot_avg_daily_use 取消數值標籤: {n_l} 處（預期 1）"); print("\n".join(log)); sys.exit(1)
txt = DAILY_LABELS.sub("  # [修訂] 數值標籤取消：圖下方有表格列出各月數值", txt)
log.append("OK  P5 plot_avg_daily_use 取消數值標籤: 1 處（預期 1）")

# ------------------------------------------------------------------
# P6 主標題（審查人 2026-09-13 裁示）
#   6a. 所有圖主標題置左：里程圖原本就用 title(adj = 0)；其餘五個作圖函數改為
#       plot(main = "") 後另呼叫 title(main = title, adj = 0)，xlab/ylab 不受影響。
#   6b. 平均每日／單日平均、搭乘次數區間共 12 張圖的主標題補「折線圖」三字（檔名不變）。
# ------------------------------------------------------------------
# 各函數 plot() 引數間的空行數不同，用正規式比對
sub(r'main = title,\s*cex\.main = 1\.7,', 'main = "",\n    cex.main = 1.7,',
    5, "P6a plot() 主標題清空（TPASS×2、平均每日、區間、轉乘）")
TITLE_LEFT = "\n\n  # [修訂] 主標題置左\n  title(main = title, adj = 0, cex.main = 1.7)"
sub("  flag_points(1:nrow(data), data$TPASS比例, data$月份, flag, cex = 1.3)",
    "  flag_points(1:nrow(data), data$TPASS比例, data$月份, flag, cex = 1.3)" + TITLE_LEFT,
    2, "P6a TPASS 兩版加 title(adj = 0)", literal=True)
sub("  flag_points(x, y, data$月份, flag, cex = 1.3)",
    "  flag_points(x, y, data$月份, flag, cex = 1.3)" + TITLE_LEFT,
    1, "P6a 平均每日加 title(adj = 0)", literal=True)
sub("  flag_points(x, y3, data$月份, flag, cex = 1.3)",
    "  flag_points(x, y3, data$月份, flag, cex = 1.3)" + TITLE_LEFT,
    2, "P6a 區間、轉乘加 title(adj = 0)", literal=True)

sub(r'("113年6月至115年6月[^"\n]*?(平均每日使用次數|單日平均使用次數|區間月人數變化))",',
    r'\1折線圖",',
    12, "P6b 12 張圖主標題補「折線圖」")

# ------------------------------------------------------------------
# P7 里程 4 張圖主標題刪「月」字（審查人 2026-09-14 裁示）
#    「…客運月平均搭乘里程折線圖」→「…客運平均搭乘里程折線圖」；png 檔名與 xlsx 檔名不變。
# ------------------------------------------------------------------
sub(r'(客運)月(平均搭乘里程折線圖",)', r'\1\2',
    4, "P7 里程 4 張圖主標題刪「月」")

# ------------------------------------------------------------------
# P8 A7：TPASS 重複列去除（審查人 2026-09-17 裁示）
#    花蓮市區 114/08–114/10、臺東市區 114/09–114/10 的 TPASS 票證中約三成列為同一趟車的重複紀錄：
#    「是否包含異常值」= 1 且下車時間為 1970-01-01（未刷下車）的列，另有一筆旗標為 0 的正常列
#    與其 卡號、搭乘路線代碼、上車站牌代碼、刷卡上車時間 完全相同。
#    規則對四組資料全期間一律套用（資料驅動，不指定月份），實際只有上述五個月被實質改變。
#    插在「市區客運路線清理」chunk 末尾，四組資料（花蓮公路、臺東公路、花蓮市區_clean、臺東市區_clean）
#    都已建立、任何指標尚未計算之處；後續各章（含 花蓮地區／臺東地區 合併）都用這四個物件。
#    詳見 review/A-資料缺漏與異常/A7-TPASS重複列說明.md。
# ------------------------------------------------------------------
P8_DEDUP = '''

# =========================
# [修訂 A7] TPASS 重複列去除
# =========================
# 重複列：「是否包含異常值」= 1 且 刷卡下車時間 為 1970-01-01（未刷下車）的列，
# 若另有一筆旗標為 0 的正常列與其 卡號｜搭乘路線代碼｜上車站牌代碼｜刷卡上車時間 相同，
# 即為同一趟車的重複紀錄，刪除該旗標列；正常列保留，使用人數不變。
# 規則對四組資料全期間一律套用；實際只有 花蓮市區 114/08–114/10、臺東市區 114/09–114/10 被實質改變。

remove_duplicate_rows <- function(x, label) {
  x <- copy(x)
  flag_col <- grep("^是否包含異常值", names(x), value = TRUE)
  stopifnot(length(flag_col) == 1)
  x[, A7_key := paste(卡號, 搭乘路線代碼, 上車站牌代碼, as.character(刷卡上車時間), sep = "|")]
  ok_keys <- x[get(flag_col) == 0, unique(A7_key)]
  x[, A7_dup :=
      !is.na(get(flag_col)) & get(flag_col) == 1 &
      substr(as.character(刷卡下車時間), 1, 4) == "1970" &
      !is.na(卡號) & 卡號 != "" &
      A7_key %chin% ok_keys]
  by_month <- x[A7_dup == TRUE, .N, by = .(月 = format(as.Date(`資料代表日期(yyyy-MM-dd)`), "%Y-%m"))][order(月)]
  cat("[A7] ", label, "：去除重複列 ", sum(x$A7_dup), " 筆（", nrow(x), " → ", nrow(x) - sum(x$A7_dup), "）\\n", sep = "")
  if (nrow(by_month) > 0) print(by_month[N >= 10])
  x <- x[A7_dup == FALSE]
  x[, c("A7_key", "A7_dup") := NULL]
  x[]
}

花蓮公路      <- remove_duplicate_rows(花蓮公路,      "花蓮公路")
臺東公路      <- remove_duplicate_rows(臺東公路,      "臺東公路")
花蓮市區_clean <- remove_duplicate_rows(花蓮市區_clean, "花蓮市區")
臺東市區_clean <- remove_duplicate_rows(臺東市區_clean, "臺東市區")
'''
sub('''  "臺東市區排除筆數：",
  nrow(臺東市區) - nrow(臺東市區_clean),
  "\\n"
)
```''', '''  "臺東市區排除筆數：",
  nrow(臺東市區) - nrow(臺東市區_clean),
  "\\n"
)''' + P8_DEDUP + '''```''',
    1, "P8 A7 TPASS 重複列去除", literal=True)

# ------------------------------------------------------------------
# 收尾檢查：還有哪些 sum(原始票證筆數) 殘留（檢查用 chunk 可保留）
# ------------------------------------------------------------------
lines = txt.split("\n")
left = []
for i, l in enumerate(lines):
    if re.search(r'sum\(\s*原始票證筆數', l):
        left.append(f"  L{i+1}: {l.strip()}")
    elif re.search(r'sum\(\s*$', l) and i + 1 < len(lines) and '原始票證筆數' in lines[i + 1]:
        left.append(f"  L{i+1}: {l.strip()} {lines[i+1].strip()}")

open(SRC, "wb").write(txt.encode("utf-8"))
print("\n".join(log))
print(f"\n殘留 sum(原始票證筆數)：{len(left)} 處")
print("\n".join(left))
print(f"\n已寫回 {SRC}（{len(lines)} 行，LF 換行）")
