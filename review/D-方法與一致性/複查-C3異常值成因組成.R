# 兩件事：
# (1) 四組資料的旗標成因組成：是「單端未刷卡（時間 1970）」還是「站牌無法辨識（站序 -99）」
# (2) 花蓮市區 114/08–114/10 的旗標高峰，是否就是 TPASS 五次以上占比異常的同一批列
suppressPackageStartupMessages(library(data.table))
options(width = 200)
dp <- "d:/Github/115_Final-Report/data"
start <- as.Date("2024-06-01"); end <- as.Date("2026-06-30")
tpass <- c("#HUA-199", "#HUA-399", "#TTT-299")
routes_hua <- c("1121","1122","1123","1125","1126","1128","1129","1130","1132","1133","1135","1136","1137","1139","1140","1141","1142","1143","1145","8119","8161","8173","8181")
routes_ttt <- c("1145","309","8101","8102","8103","8105","8107","8109","8110","8111","8113","8115","8117","8119","8120","8122","8125","8128","8129","8130","8131","8132","8135","8136","8137","8138","8150","8151","8152","8153","8156","8157","8158","8161","8163","8165","8166","8167","8168","8170","8171","8172","8173","8178","8180","8181")
hua_bad <- c("50","51","52","53","71","72","73","81","83","綠線")
ttt_bad_d <- as.Date(c("2026-05-24","2026-05-25")); ttt_ok <- c("101","201","202","203")

rd <- function(f) {
  d <- fread(file.path(dp, f), select = c(2, 6, 8, 15, 16, 19, 20, 26, 30),
             colClasses = list(character = c(1, 2, 3, 8)), showProgress = FALSE)
  setnames(d, c("卡號", "票種", "路線", "上序", "上時", "下序", "下時", "旗標", "日期"))
  d[, 日期 := as.Date(日期)]
  d[, `:=`(票種 = trimws(票種), 路線 = trimws(路線), 旗標 = trimws(旗標))]
  d[日期 >= start & 日期 <= end]
}

compose <- function(d, label) {
  f <- d[旗標 == "1"]
  if (nrow(f) == 0) return(NULL)
  f[, 類型 := fcase(
    (grepl("^1970", 上時) | grepl("^1970", 下時)) & (上序 == -99 | 下序 == -99), "單端未刷卡（時間 1970 且該端站序 -99）",
    grepl("^1970", 上時) | grepl("^1970", 下時), "時間 1970 但站序正常",
    上序 == -99 | 下序 == -99, "兩端時間正常，但有站序 -99",
    default = "其他")]
  t <- f[, .(列數 = .N), by = 類型][, `占比%` := round(100 * 列數 / sum(列數), 1)][order(-列數)]
  t[, 資料 := label][]
}

hua_c <- rd("花蓮縣公車.csv"); HC <- hua_c[!路線 %chin% hua_bad]; c1 <- compose(HC, "花蓮市區")
# (2) 花蓮市區 TPASS 五次以上：含／不含旗標列
HC[, 月 := sprintf("%03d/%02d", as.integer(format(日期, "%Y")) - 1911, as.integer(format(日期, "%m")))]
tp <- HC[票種 %chin% tpass & 卡號 != ""]
band <- function(x) {
  pm <- x[, .(n = .N), by = .(月, 卡號)]
  pm[, g := fcase(n == 1, "僅一次", n <= 4, "二至四次", default = "五次以上")]
  pm[, .(使用人合計日數 = .N, 五次以上 = sum(g == "五次以上"),
         `五次以上%` = round(100 * mean(g == "五次以上"), 2)), by = 月][order(月)]
}
a <- band(tp); b <- band(tp[旗標 == "0"])
cmp <- merge(a, b, by = "月", suffixes = c("_含異常", "_刪異常"))
rm(hua_c); invisible(gc())

ttt_c <- rd("臺東縣公車.csv"); c2 <- compose(ttt_c[!(日期 %in% ttt_bad_d) | 路線 %chin% ttt_ok], "臺東市區"); rm(ttt_c); invisible(gc())
thb <- rd("公路客運2024_to_202606.csv")
c3 <- compose(thb[路線 %chin% routes_hua], "花蓮公路"); c4 <- compose(thb[路線 %chin% routes_ttt], "臺東公路"); rm(thb); invisible(gc())

cat("=== (1) 旗標為 1 的成因組成 ===\n")
print(rbindlist(list(c1, c2, c3, c4))[, .(資料, 類型, 列數, `占比%`)])
cat("\n=== (2) 花蓮市區 TPASS 乘客「五次以上」占比：含異常 vs 刪異常 ===\n")
print(cmp)
