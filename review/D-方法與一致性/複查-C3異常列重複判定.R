# 四組資料全部異常旗標列：是否與某筆正常列為同一趟車（同卡號、同路線、同上車站、同上車時間）
#
# 旗標列分三類，各用對應鍵比對旗標為 0 的正常列：
#   未刷下車（下車時間 1970）        鍵 = 卡號｜路線代碼｜上車站代碼｜上車時間
#   未刷上車（上車時間 1970）        鍵 = 卡號｜路線代碼｜下車站代碼｜下車時間
#   兩端時間正常，僅站序 -99         上車鍵或下車鍵任一命中即算重複
# 另附寬鬆鍵（卡號｜上車時間，不看路線與站牌）的命中率，供判斷是否有「同人同時但路線／站牌不同」的近似重複。
#
# 輸出：
#   output/C3-異常列重複判定-逐月.csv   四組 × 月 × 類別：旗標列數、嚴格鍵命中、寬鬆鍵命中
#   output/C3-異常列重複判定-摘要.csv   四組 × 類別 全期間合計
#   output/C3-異常列重複判定-TPASS.csv  四組 × 月：TPASS 與非 TPASS 分開的未刷下車重複率
#   output/C3-異常列重複判定-TPASS去重重算.csv  四組 × 月：TPASS 搭乘次數區間去除重複列前後
# 資料範圍與整併版 Rmd 相同（113/06～115/06、花蓮市區排除異常路線、臺東市區 2026-05-24／25 只留 101／201／202／203）。
suppressPackageStartupMessages(library(data.table))
options(width = 220)
dp <- "d:/Github/115_Final-Report/data"
od <- "d:/Github/115_Final-Report/review/D-方法與一致性/output"
start <- as.Date("2024-06-01"); end <- as.Date("2026-06-30")
routes_hua <- c("1121","1122","1123","1125","1126","1128","1129","1130","1132","1133","1135","1136","1137","1139","1140","1141","1142","1143","1145","8119","8161","8173","8181")
routes_ttt <- c("1145","309","8101","8102","8103","8105","8107","8109","8110","8111","8113","8115","8117","8119","8120","8122","8125","8128","8129","8130","8131","8132","8135","8136","8137","8138","8150","8151","8152","8153","8156","8157","8158","8161","8163","8165","8166","8167","8168","8170","8171","8172","8173","8178","8180","8181")
hua_bad <- c("50","51","52","53","71","72","73","81","83","綠線")
ttt_bad_d <- as.Date(c("2026-05-24","2026-05-25")); ttt_ok <- c("101","201","202","203")
tpass_types <- c("#HUA-199", "#HUA-399", "#TTT-299")

rd <- function(f) {
  d <- fread(file.path(dp, f), select = c(2, 6, 7, 8, 13, 15, 16, 17, 19, 20, 26, 30),
             colClasses = "character", showProgress = FALSE)
  setnames(d, c("卡號", "票種次類型", "路線代碼", "路線", "上車站代碼", "上車站序", "上車時間",
                "下車站代碼", "下車站序", "下車時間", "旗標", "日期"))
  d[, 日期 := as.Date(日期)]
  d[, `:=`(路線 = trimws(路線), 旗標 = trimws(旗標))]
  d[日期 >= start & 日期 <= end]
}

judge <- function(d, label) {
  d[, 月 := sprintf("%03d/%02d", as.integer(format(日期, "%Y")) - 1911, as.integer(format(日期, "%m")))]
  d[, TPASS := 票種次類型 %chin% tpass_types]
  d[, 類別 := fifelse(旗標 != "1", "正常",
              fifelse(grepl("^1970", 下車時間), "未刷下車",
              fifelse(grepl("^1970", 上車時間), "未刷上車", "兩端時間正常僅站序-99")))]
  d[, k_on  := paste(卡號, 路線代碼, 上車站代碼, 上車時間, sep = "|")]
  d[, k_off := paste(卡號, 路線代碼, 下車站代碼, 下車時間, sep = "|")]
  d[, k_loose := paste(卡號, 上車時間, sep = "|")]
  ok <- d[類別 == "正常"]
  ok_on <- unique(ok$k_on); ok_off <- unique(ok$k_off); ok_loose <- unique(ok$k_loose)
  fl <- d[類別 != "正常"]; fl[, 資料 := label]
  fl[, 嚴格命中 := fcase(
    類別 == "未刷下車", k_on %chin% ok_on,
    類別 == "未刷上車", k_off %chin% ok_off,
    default = (k_on %chin% ok_on) | (k_off %chin% ok_off))]
  fl[, 寬鬆命中 := fcase(
    類別 == "未刷上車", paste(卡號, 下車時間, sep = "|") %chin% unique(ok[, paste(卡號, 下車時間, sep = "|")]),
    default = k_loose %chin% ok_loose)]
  # 嚴格命中的旗標列，對應正常列是否也只有一筆（1:1）
  cnt_on <- ok[, .N, by = k_on]; setnames(cnt_on, "N", "n_on")
  fl <- merge(fl, cnt_on, by = "k_on", all.x = TRUE)
  monthly <- fl[, .(旗標列 = .N, 嚴格鍵命中 = sum(嚴格命中), `嚴格%` = round(100 * mean(嚴格命中), 1),
                    寬鬆鍵命中 = sum(寬鬆命中), `寬鬆%` = round(100 * mean(寬鬆命中), 1)),
                by = .(資料, 月, 類別)][order(月, 類別)]
  summ <- fl[, .(旗標列 = .N, 嚴格鍵命中 = sum(嚴格命中), `嚴格%` = round(100 * mean(嚴格命中), 1),
                 寬鬆鍵命中 = sum(寬鬆命中), `寬鬆%` = round(100 * mean(寬鬆命中), 1),
                 嚴格命中且正常列僅一筆 = sum(嚴格命中 & !is.na(n_on) & n_on == 1)),
             by = .(資料, 類別)][order(類別)]
  tp <- fl[類別 == "未刷下車", .(旗標未刷下車列 = .N, 嚴格鍵命中 = sum(嚴格命中),
                                 `嚴格%` = round(100 * mean(嚴格命中), 1)),
           by = .(資料, 月, TPASS)][order(月, -TPASS)]
  # TPASS 搭乘次數區間：去除嚴格鍵命中的未刷下車列前後重算（區間定義同整併版 Rmd）
  d[, 重複 := 類別 == "未刷下車" & k_on %chin% ok_on]
  bucket <- function(x) x[, .(n = .N), by = .(月, 卡號)][
    , .(人數 = .N, `僅一次%` = round(100 * mean(n == 1), 1), `五次以上%` = round(100 * mean(n >= 5), 1), 人次 = sum(n)), by = 月]
  rc <- merge(bucket(d[TPASS == TRUE]), bucket(d[TPASS == TRUE & 重複 == FALSE]), by = "月", suffixes = c("_原", "_去重"))
  rc[, 資料 := label]; setcolorder(rc, "資料")
  list(monthly = monthly, summ = summ, tp = tp, rc = rc)
}

hua_c <- rd("花蓮縣公車.csv"); r1 <- judge(hua_c[!路線 %chin% hua_bad], "花蓮市區"); rm(hua_c); invisible(gc())
ttt_c <- rd("臺東縣公車.csv"); r2 <- judge(ttt_c[!(日期 %in% ttt_bad_d) | 路線 %chin% ttt_ok], "臺東市區"); rm(ttt_c); invisible(gc())
thb <- rd("公路客運2024_to_202606.csv")
r3 <- judge(thb[路線 %chin% routes_hua], "花蓮公路")
r4 <- judge(thb[路線 %chin% routes_ttt], "臺東公路"); rm(thb); invisible(gc())

monthly <- rbindlist(list(r1$monthly, r2$monthly, r3$monthly, r4$monthly))
summ <- rbindlist(list(r1$summ, r2$summ, r3$summ, r4$summ))
tp <- rbindlist(list(r1$tp, r2$tp, r3$tp, r4$tp))
rc <- rbindlist(list(r1$rc, r2$rc, r3$rc, r4$rc))

cat("=== 全期間摘要：四組 × 類別 ===\n")
print(summ)
cat("\n=== 四組合計 ===\n")
print(summ[, .(旗標列 = sum(旗標列), 嚴格鍵命中 = sum(嚴格鍵命中), `嚴格%` = round(100 * sum(嚴格鍵命中) / sum(旗標列), 1),
               寬鬆鍵命中 = sum(寬鬆鍵命中), `寬鬆%` = round(100 * sum(寬鬆鍵命中) / sum(旗標列), 1)), by = 類別])
cat("\n=== 逐月：嚴格鍵命中率%（未刷下車）===\n")
print(dcast(monthly[類別 == "未刷下車"], 月 ~ 資料, value.var = "嚴格%"), nrow = 40)
cat("\n=== 逐月：嚴格鍵命中列數（未刷下車）===\n")
print(dcast(monthly[類別 == "未刷下車"], 月 ~ 資料, value.var = "嚴格鍵命中"), nrow = 40)
cat("\n=== 逐月：嚴格鍵命中率%（未刷上車）===\n")
print(dcast(monthly[類別 == "未刷上車"], 月 ~ 資料, value.var = "嚴格%"), nrow = 40)
cat("\n=== 逐月：嚴格鍵命中率%（兩端時間正常僅站序-99）===\n")
print(dcast(monthly[類別 == "兩端時間正常僅站序-99"], 月 ~ 資料, value.var = "嚴格%"), nrow = 40)
cat("\n=== 嚴格鍵命中率 ≥ 10% 的 資料×月×類別 ===\n")
print(monthly[`嚴格%` >= 10][order(-`嚴格%`)], nrow = 100)
cat("\n=== 未刷下車：TPASS vs 非 TPASS 逐月嚴格鍵命中率（只列有命中的列）===\n")
print(tp[嚴格鍵命中 > 0][order(資料, 月)], nrow = 200)

cat("
=== TPASS 搭乘次數區間：去重前後有差異的 資料×月 ===
")
print(rc[人次_原 != 人次_去重][order(資料, 月)], nrow = 100)
fwrite(rc, file.path(od, "C3-異常列重複判定-TPASS去重重算.csv"), bom = TRUE)
fwrite(monthly, file.path(od, "C3-異常列重複判定-逐月.csv"), bom = TRUE)
fwrite(summ, file.path(od, "C3-異常列重複判定-摘要.csv"), bom = TRUE)
fwrite(tp, file.path(od, "C3-異常列重複判定-TPASS.csv"), bom = TRUE)
cat("\n完成\n")
