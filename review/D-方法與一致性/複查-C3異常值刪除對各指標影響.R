# 量化：刪除「是否包含異常值 = 1」的列，對各指標分別有多少影響
# 對象：臺東市區（獨立腳本唯一有過濾者）、花蓮市區、花蓮公路、臺東公路
# 指標：TPASS 比例、總使用次數／使用人數／平均每日、搭乘次數區間、轉乘率分母、里程可配對性
suppressPackageStartupMessages(library(data.table))
options(width = 210)
dp <- "d:/Github/115_Final-Report/data"
start <- as.Date("2024-06-01"); end <- as.Date("2026-06-30")
tpass <- c("#HUA-199", "#HUA-399", "#TTT-299")
routes_hua <- c("1121","1122","1123","1125","1126","1128","1129","1130","1132","1133","1135","1136","1137","1139","1140","1141","1142","1143","1145","8119","8161","8173","8181")
routes_ttt <- c("1145","309","8101","8102","8103","8105","8107","8109","8110","8111","8113","8115","8117","8119","8120","8122","8125","8128","8129","8130","8131","8132","8135","8136","8137","8138","8150","8151","8152","8153","8156","8157","8158","8161","8163","8165","8166","8167","8168","8170","8171","8172","8173","8178","8180","8181")
hua_bad <- c("50","51","52","53","71","72","73","81","83","綠線")
ttt_bad_d <- as.Date(c("2026-05-24","2026-05-25")); ttt_ok <- c("101","201","202","203")

rd <- function(f) {
  d <- fread(file.path(dp, f), select = c(2, 6, 8, 15, 19, 26, 30),
             colClasses = list(character = c(1, 2, 3, 6)), showProgress = FALSE)
  setnames(d, c("卡號", "票種", "路線", "上車站序", "下車站序", "旗標", "日期"))
  d[, 日期 := as.Date(日期)]
  d[, `:=`(票種 = trimws(票種), 路線 = trimws(路線), 旗標 = trimws(旗標))]
  d[日期 >= start & 日期 <= end]
}

report <- function(d, label) {
  d <- d[!is.na(卡號) & 卡號 != ""]
  d[, 月 := sprintf("%03d/%02d", as.integer(format(日期, "%Y")) - 1911, as.integer(format(日期, "%m")))]
  keep <- d[旗標 == "0"]
  nf <- nrow(d) - nrow(keep)
  cat("\n==========", label, "==========\n")
  cat("全部列數", format(nrow(d), big.mark = ","), "；旗標為 1 者", format(nf, big.mark = ","),
      sprintf("（%.2f%%）", 100 * nf / nrow(d)), "\n")
  if (nf == 0) { cat("無旗標列，各指標不受影響。\n"); return(invisible(NULL)) }

  # 1. TPASS 比例（全期）
  r1 <- 100 * sum(d$票種 %chin% tpass) / nrow(d)
  r2 <- 100 * sum(keep$票種 %chin% tpass) / nrow(keep)
  cat(sprintf("TPASS 比例（全期）　　　含異常 %.2f%%  刪異常 %.2f%%  差 %+.2f 個百分點\n", r1, r2, r2 - r1))

  # 2. 總使用次數與使用人數（全期）
  cat(sprintf("總使用次數　　　　　　　含異常 %s  刪異常 %s  差 %+.2f%%\n",
              format(nrow(d), big.mark = ","), format(nrow(keep), big.mark = ","),
              100 * (nrow(keep) - nrow(d)) / nrow(d)))
  u1 <- d[, uniqueN(paste(月, 卡號))]; u2 <- keep[, uniqueN(paste(月, 卡號))]
  cat(sprintf("使用人數（人月合計）　　含異常 %s  刪異常 %s  差 %+.2f%%\n",
              format(u1, big.mark = ","), format(u2, big.mark = ","), 100 * (u2 - u1) / u1))

  # 3. 搭乘次數區間（全期人月）
  band <- function(x) {
    pm <- x[, .(n = .N), by = .(月, 卡號)]
    pm[, g := fcase(n == 1, "僅一次", n <= 4, "二至四次", default = "五次以上")]
    t <- pm[, .N, by = g]; setkey(t, g); 100 * t[c("僅一次","二至四次","五次以上"), N] / nrow(pm)
  }
  b1 <- band(d); b2 <- band(keep)
  cat(sprintf("搭乘次數區間　　　　　　含異常 %.1f/%.1f/%.1f  刪異常 %.1f/%.1f/%.1f  差 %+.1f/%+.1f/%+.1f 個百分點\n",
              b1[1], b1[2], b1[3], b2[1], b2[2], b2[3], b2[1]-b1[1], b2[2]-b1[2], b2[3]-b1[3]))

  # 4. 轉乘分母（總搭乘紀錄數）
  cat(sprintf("轉乘率分母　　　　　　　含異常 %s  刪異常 %s  → 轉乘率會被抬高 %.2f%%（分子幾乎不變）\n",
              format(nrow(d), big.mark = ","), format(nrow(keep), big.mark = ","),
              100 * (nrow(d) / nrow(keep) - 1)))

  # 5. 里程：旗標列是否本來就配不到
  cat(sprintf("里程可配對性　　　　　　旗標列中站序任一為 -99 者 %s / %s（%.1f%%）→ 配對必然失敗，刪不刪結果相同\n",
              format(d[旗標 == "1" & (上車站序 == -99 | 下車站序 == -99), .N], big.mark = ","),
              format(nf, big.mark = ","),
              100 * d[旗標 == "1" & (上車站序 == -99 | 下車站序 == -99), .N] / nf))
  invisible(NULL)
}

hua_c <- rd("花蓮縣公車.csv"); report(hua_c[!路線 %chin% hua_bad], "花蓮市區")
rm(hua_c); invisible(gc())
ttt_c <- rd("臺東縣公車.csv")
report(ttt_c[!(日期 %in% ttt_bad_d) | 路線 %chin% ttt_ok], "臺東市區")
rm(ttt_c); invisible(gc())
thb <- rd("公路客運2024_to_202606.csv")
report(thb[路線 %chin% routes_hua], "花蓮公路")
report(thb[路線 %chin% routes_ttt], "臺東公路")
cat("\n完成\n")
