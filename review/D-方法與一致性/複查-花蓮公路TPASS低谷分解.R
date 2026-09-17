# ============================================================
# 複查：花蓮公路客運 TPASS 使用比例於 114/08、115/02 之低谷分解
#
# 問題：§1.2.1 指出兩年度 7–8 月與 115/02 出現明顯低谷，歸因於
#       「寒暑假及通學需求變化」。本腳本只回答資料上能確認的事：
#   1. 低谷來自分子（TPASS 人次）減少，還是分母（總人次）增加？
#   2. 減少的 TPASS 人次集中在哪個票種次類型、持卡身分、業者、路線？
#   3. 逐日序列是平滑的季節型態，還是某日起的斷檔（供料問題）？
#   4. 平日／假日拆開看，比例是否都下降？
#   5. 是「TPASS 持卡人數變少」還是「每人搭乘次數變少」？
#      （公路客運卡號為逐月雜湊，月內 distinct 卡號可用）
#
# 輸出：review/D-方法與一致性/output/TPASS低谷-*.csv
# 執行：Rscript 複查-花蓮公路TPASS低谷分解.R   （約 60 秒）
# ============================================================

suppressPackageStartupMessages(library(data.table))

REPO_ROOT <- "d:/Github/115_Final-Report"
data_path <- file.path(REPO_ROOT, "data")
out_dir   <- file.path(REPO_ROOT, "review", "D-方法與一致性", "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

start_date <- as.Date("2024-06-01")
end_date   <- as.Date("2026-06-30")

routes_hualien_THB <- c(
  "1121", "1122", "1123", "1125", "1126", "1128", "1129", "1130",
  "1132", "1133", "1135", "1136", "1137", "1139", "1140", "1141",
  "1142", "1143", "1145", "8119", "8161", "8173", "8181"
)
routes_taitung_THB <- c(
  "1145", "309", "8101", "8102", "8103", "8105", "8107", "8109",
  "8110", "8111", "8113", "8115", "8117", "8119", "8120", "8122",
  "8125", "8128", "8129", "8130", "8131", "8132", "8135", "8136",
  "8137", "8138", "8150", "8151", "8152", "8153", "8156", "8157",
  "8158", "8161", "8163", "8165", "8166", "8167", "8168", "8170",
  "8171", "8172", "8173", "8178", "8180", "8181"
)

TPASS_TYPES <- c("#HUA-199", "#HUA-399", "#TTT-299")

emit <- function(dt, filename, title) {
  cat("\n========================================\n", title, "\n",
      "========================================\n", sep = "")
  print(dt, nrows = 400)
  fwrite(dt, file.path(out_dir, filename), bom = TRUE)
  cat("已寫入：output/", filename, "\n", sep = "")
  invisible(dt)
}

# ------------------------------------------------------------
# 讀取（以欄位索引選取：1 業者編號 2 卡號 4 持卡身分 5 票種類型
#        6 票種次類型 8 搭乘路線名稱 14 上車站牌名稱 16 刷卡上車時間
#        18 下車站牌名稱 30 資料代表日期）
# ------------------------------------------------------------
t0 <- Sys.time()
thb <- fread(
  file.path(data_path, "公路客運2024_to_202606.csv"),
  select = c(1, 2, 4, 5, 6, 8, 14, 16, 18, 30),
  colClasses = list(character = c(1, 2, 4, 5, 6, 8, 14, 18)),
  showProgress = FALSE
)
setnames(thb, c("業者", "卡號", "持卡身分", "票種類型", "票種次類型",
                "路線", "上車站", "上車時間", "下車站", "日期"))
thb[, 日期 := as.Date(日期)]
thb[, 路線 := trimws(路線)]
thb <- thb[日期 >= start_date & 日期 <= end_date]
thb[, 月份 := sprintf("%03d/%02d",
                      as.integer(format(日期, "%Y")) - 1911,
                      as.integer(format(日期, "%m")))]
thb[, TPASS := 票種次類型 %in% TPASS_TYPES]
thb[, 星期 := as.integer(format(日期, "%u"))]          # 1 = 週一
thb[, 日型 := fifelse(星期 >= 6, "假日", "平日")]
thb[, 時 := as.integer(substr(上車時間, 12, 13))]
cat("讀取完成：", format(nrow(thb), big.mark = ","), "列，",
    round(difftime(Sys.time(), t0, units = "secs")), "秒\n")

hua <- thb[路線 %in% routes_hualien_THB]
ttt <- thb[路線 %in% routes_taitung_THB]
cat("花蓮公路列數：", format(nrow(hua), big.mark = ","), "\n")

# ------------------------------------------------------------
# 1. 月序列：分子／分母／比例，並附前後月變化
# ------------------------------------------------------------
monthly <- hua[, .(總人次 = .N, TPASS人次 = sum(TPASS),
                   TPASS卡數 = uniqueN(卡號[TPASS]),
                   非TPASS人次 = sum(!TPASS)), by = 月份][order(月份)]
monthly[, `TPASS比例%` := round(100 * TPASS人次 / 總人次, 1)]
monthly[, 每卡月均次 := round(TPASS人次 / TPASS卡數, 2)]
monthly[, `TPASS人次較前月%` := round(100 * (TPASS人次 / shift(TPASS人次) - 1), 1)]
monthly[, `總人次較前月%`    := round(100 * (總人次 / shift(總人次) - 1), 1)]
monthly[, `非TPASS較前月%`   := round(100 * (非TPASS人次 / shift(非TPASS人次) - 1), 1)]
monthly[, `TPASS卡數較前月%` := round(100 * (TPASS卡數 / shift(TPASS卡數) - 1), 1)]
emit(monthly, "TPASS低谷-1-花蓮公路月序列.csv",
     "1. 花蓮公路 月序列：分子、分母、TPASS 卡數、每卡月均次")

# 四組資料的比例並列（確認是否為共同型態）
ttt_m <- ttt[, .(臺東公路 = round(100 * mean(TPASS), 1)), by = 月份]
four <- merge(monthly[, .(月份, 花蓮公路 = `TPASS比例%`)], ttt_m, by = "月份")
emit(four, "TPASS低谷-1b-兩縣公路比例並列.csv", "1b. 兩縣公路客運 TPASS 比例並列")

# ------------------------------------------------------------
# 2. 低谷月與鄰月的分解：票種次類型、持卡身分、業者、路線
# ------------------------------------------------------------
focus <- list(
  暑假 = c("114/06", "114/07", "114/08", "114/09", "114/10"),
  寒假 = c("114/12", "115/01", "115/02", "115/03", "115/04"),
  前年暑假 = c("113/06", "113/07", "113/08", "113/09", "113/10"),
  前年寒假 = c("113/12", "114/01", "114/02", "114/03", "114/04")
)
all_focus <- unlist(focus)

decompose <- function(col, label, file) {
  d <- hua[月份 %in% all_focus & TPASS,
           .(TPASS人次 = .N), by = c("月份", col)]
  w <- dcast(d, get(col) ~ 月份, value.var = "TPASS人次", fill = 0L)
  setnames(w, "col", col)
  setorderv(w, "115/03", order = -1L)
  emit(w, file, paste0("2. TPASS 人次依", label, "分解（低谷月與鄰月）"))
}
tpass_by_type <- decompose("票種次類型", "票種次類型", "TPASS低谷-2a-依票種次類型.csv")
tpass_by_id   <- decompose("持卡身分",   "持卡身分",   "TPASS低谷-2b-依持卡身分.csv")
tpass_by_op   <- decompose("業者",       "業者",       "TPASS低谷-2c-依業者.csv")
tpass_by_rt   <- decompose("路線",       "路線",       "TPASS低谷-2d-依路線.csv")

# 非 TPASS 人次依持卡身分（看學生族群是否同步減少）
nont <- hua[月份 %in% all_focus & !TPASS, .(人次 = .N), by = .(月份, 持卡身分)]
nont_w <- dcast(nont, 持卡身分 ~ 月份, value.var = "人次", fill = 0L)
emit(nont_w, "TPASS低谷-2e-非TPASS依持卡身分.csv", "2e. 非 TPASS 人次依持卡身分")

# 全期間：TPASS 列的持卡身分分布（確認 TPASS 列是否能辨識學生）
id_all <- hua[(TPASS), .(TPASS人次 = .N), by = 持卡身分][, `占比%` := round(100 * TPASS人次 / sum(TPASS人次), 1)][order(-TPASS人次)]
emit(id_all, "TPASS低谷-2f-TPASS列持卡身分全期分布.csv", "2f. TPASS 列的持卡身分全期分布")

# 業者×月：總人次與 TPASS 人次（看是否有業者整月 TPASS 為 0）
op_m <- hua[, .(總人次 = .N, TPASS人次 = sum(TPASS)), by = .(業者, 月份)]
op_m[, `TPASS比例%` := round(100 * TPASS人次 / 總人次, 1)]
op_ratio <- dcast(op_m, 月份 ~ 業者, value.var = "TPASS比例%")
emit(op_ratio, "TPASS低谷-2g-業者逐月TPASS比例.csv", "2g. 各業者逐月 TPASS 比例")
op_n <- dcast(op_m, 月份 ~ 業者, value.var = "TPASS人次", fill = 0L)
emit(op_n, "TPASS低谷-2h-業者逐月TPASS人次.csv", "2h. 各業者逐月 TPASS 人次")

# 路線×月 TPASS 人次貢獻：低谷月相對於前後月平均的減少量，依路線排序
rt_m <- hua[, .(總人次 = .N, TPASS人次 = sum(TPASS)), by = .(路線, 月份)]
route_drop <- function(low, before, after, tag) {
  x <- dcast(rt_m[月份 %in% c(low, before, after)], 路線 ~ 月份,
             value.var = "TPASS人次", fill = 0L)
  x[, 鄰月平均 := round((get(before) + get(after)) / 2, 0)]
  x[, 減少量 := 鄰月平均 - get(low)]
  x[, `減少幅度%` := round(100 * 減少量 / 鄰月平均, 1)]
  x[, `占總減少%` := round(100 * 減少量 / sum(減少量), 1)]
  x[, 低谷 := tag]
  setorder(x, -減少量)
  x[]
}
rd1 <- route_drop("114/08", "114/07", "114/09", "114/08")
rd2 <- route_drop("115/02", "115/01", "115/03", "115/02")
emit(rd1, "TPASS低谷-2i-路線減少量-11408.csv", "2i. 114/08 各路線 TPASS 人次較鄰月平均之減少量")
emit(rd2, "TPASS低谷-2j-路線減少量-11502.csv", "2j. 115/02 各路線 TPASS 人次較鄰月平均之減少量")

# ------------------------------------------------------------
# 3. 逐日序列（斷檔 vs 季節）
# ------------------------------------------------------------
daily <- hua[日期 >= as.Date("2025-06-15") & 日期 <= as.Date("2025-10-15") |
             日期 >= as.Date("2025-12-15") & 日期 <= as.Date("2026-04-15"),
             .(總人次 = .N, TPASS人次 = sum(TPASS)), by = .(日期, 星期, 日型)][order(日期)]
daily[, `TPASS比例%` := round(100 * TPASS人次 / 總人次, 1)]
emit(daily, "TPASS低谷-3-逐日序列.csv", "3. 逐日序列（2025/06/15–10/15、2025/12/15–2026/04/15）")

# 逐週（週一起算）比例，較易看出轉折點
hua[, 週起 := 日期 - (星期 - 1)]
weekly <- hua[日期 >= as.Date("2025-06-01") & 日期 <= as.Date("2026-04-30"),
              .(總人次 = .N, TPASS人次 = sum(TPASS), 天數 = uniqueN(日期)), by = 週起][order(週起)]
weekly[, `TPASS比例%` := round(100 * TPASS人次 / 總人次, 1)]
emit(weekly, "TPASS低谷-3b-逐週序列.csv", "3b. 逐週序列（2025/06–2026/04）")

# 逐日 TPASS 為 0 的日子（供料斷檔訊號）
zero_days <- hua[, .(總人次 = .N, TPASS人次 = sum(TPASS)), by = 日期][TPASS人次 == 0 & 總人次 > 0][order(日期)]
emit(zero_days, "TPASS低谷-3c-有搭乘但TPASS為0之日.csv", "3c. 有搭乘紀錄但 TPASS 人次為 0 之日")

# ------------------------------------------------------------
# 4. 平日／假日拆分
# ------------------------------------------------------------
dtype <- hua[月份 %in% all_focus, .(總人次 = .N, TPASS人次 = sum(TPASS), 天數 = uniqueN(日期)),
             by = .(月份, 日型)][order(月份, 日型)]
dtype[, `TPASS比例%` := round(100 * TPASS人次 / 總人次, 1)]
dtype[, 每日TPASS := round(TPASS人次 / 天數, 0)]
dtype[, 每日總人次 := round(總人次 / 天數, 0)]
emit(dtype, "TPASS低谷-4-平日假日拆分.csv", "4. 平日／假日拆分（低谷月與鄰月）")

# 上車時段：平日 TPASS 人次依時段（06–08 通勤通學尖峰 vs 其他）
hour_band <- hua[月份 %in% all_focus & 日型 == "平日" & !is.na(時),
                 .(TPASS人次 = sum(TPASS), 總人次 = .N),
                 by = .(月份, 時段 = fifelse(時 %in% 6:8, "06–08", fifelse(時 %in% 15:18, "15–18", "其他")))]
hour_w <- dcast(hour_band, 月份 ~ 時段, value.var = "TPASS人次", fill = 0L)
emit(hour_w, "TPASS低谷-4b-平日TPASS依上車時段.csv", "4b. 平日 TPASS 人次依上車時段")

# ------------------------------------------------------------
# 5. 每卡搭乘次數分布（持卡人少了，還是每人搭少了）
# ------------------------------------------------------------
per_card <- hua[月份 %in% all_focus & TPASS, .(次數 = .N), by = .(月份, 卡號)]
per_card[, 區間 := cut(次數, c(0, 1, 4, 9, 19, Inf),
                      labels = c("1", "2–4", "5–9", "10–19", "20+"))]
pc_w <- dcast(per_card[, .(卡數 = .N), by = .(月份, 區間)], 月份 ~ 區間, value.var = "卡數", fill = 0L)
pc_w <- merge(pc_w, per_card[, .(卡數 = .N, 中位數 = as.numeric(median(次數)),
                                 平均 = round(mean(次數), 2)), by = 月份], by = "月份")
emit(pc_w, "TPASS低谷-5-每卡月搭乘次數分布.csv", "5. TPASS 每卡月搭乘次數分布")

# ------------------------------------------------------------
# 6. 臺東公路同型分解（對照）
# ------------------------------------------------------------
ttt_dtype <- ttt[月份 %in% all_focus, .(總人次 = .N, TPASS人次 = sum(TPASS)), by = .(月份, 日型)][order(月份, 日型)]
ttt_dtype[, `TPASS比例%` := round(100 * TPASS人次 / 總人次, 1)]
emit(ttt_dtype, "TPASS低谷-6-臺東公路平日假日對照.csv", "6. 臺東公路 平日／假日對照")

# ------------------------------------------------------------
# 7. 站牌：TPASS 人次減少集中在哪些站？（上車站或下車站）
#    只描述資料事實，不做成因推論
# ------------------------------------------------------------
hua[, 東華 := grepl("東華", 上車站) | grepl("東華", 下車站)]
dh <- hua[月份 %in% all_focus, .(總人次 = .N, TPASS人次 = sum(TPASS),
                                 TPASS_東華 = sum(TPASS & 東華),
                                 非TPASS_東華 = sum(!TPASS & 東華)), by = 月份][order(月份)]
dh[, `東華占TPASS%` := round(100 * TPASS_東華 / TPASS人次, 1)]
dh[, TPASS_非東華 := TPASS人次 - TPASS_東華]
emit(dh, "TPASS低谷-7-東華站牌TPASS人次.csv", "7. 上下車站含「東華」之 TPASS 人次")

stop_drop <- function(low, before, after) {
  x <- hua[月份 %in% c(low, before, after) & TPASS,
           .(TPASS人次 = .N), by = .(月份, 站 = 上車站)]
  w <- dcast(x, 站 ~ 月份, value.var = "TPASS人次", fill = 0L)
  w[, 鄰月平均 := round((get(before) + get(after)) / 2, 0)]
  w[, 減少量 := 鄰月平均 - get(low)]
  w[, `占總減少%` := round(100 * 減少量 / sum(減少量), 1)]
  setorder(w, -減少量)
  w[, 低谷 := low]
  w[]
}
sd1 <- stop_drop("114/08", "114/07", "114/09")
sd2 <- stop_drop("115/02", "115/01", "115/03")
emit(head(sd1, 15), "TPASS低谷-7b-上車站減少量-11408.csv", "7b. 114/08 上車站 TPASS 減少量前 15")
emit(head(sd2, 15), "TPASS低谷-7c-上車站減少量-11502.csv", "7c. 115/02 上車站 TPASS 減少量前 15")

# 2026-02-03 總人次僅 63：各業者當日列數
d0203 <- hua[日期 %in% as.Date(c("2026-02-02", "2026-02-03", "2026-02-04")),
             .(列數 = .N), by = .(日期, 業者)][order(日期, 業者)]
emit(d0203, "TPASS低谷-7d-20260203各業者列數.csv", "7d. 2026-02-02～04 各業者列數")

cat("\n完成。總耗時", round(difftime(Sys.time(), t0, units = "secs")), "秒\n")
