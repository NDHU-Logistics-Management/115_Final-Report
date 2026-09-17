# ============================================================
# 一鍵重出 revise/output 下的全部圖表，並對帳
#
# 用法（在任何工作目錄）：
#   Rscript "revise/code/run-all.R"
#
# 步驟：
#   1. 把 115期末-修訂.Rmd 以 knitr::purl() 抽成純 R 檔後 source，
#      輸出到 revise/output/fig（22 張 png）與 revise/output/tbl（4 份 xlsx），約 5 分鐘。
#      實作測試/ 內的原始輸出不動，留作對帳基準。
#   2. 對帳：新 xlsx vs 實作測試/ 舊 xlsx 逐格比對（寫 數值對帳.csv、數值對帳-摘要.csv），
#      再與 review/A-資料缺漏與異常/output 的獨立重算交叉比對（應完全一致）。
#
# 要改計算或圖表，直接改 115期末-修訂.Rmd。它與原稿的差異：
#   git diff --no-index 實作測試/115期末.Rmd revise/code/115期末-修訂.Rmd
# ============================================================
suppressPackageStartupMessages({ library(data.table); library(openxlsx) })

this_file <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
if (length(this_file) == 0) this_file <- "run-all.R"   # 在 RStudio 內以 source() 執行時
code_dir  <- normalizePath(dirname(this_file))

REPO_ROOT <- normalizePath(file.path(code_dir, "..", ".."))
FIG_DIR   <- file.path(REPO_ROOT, "revise", "output", "fig")
TBL_DIR   <- file.path(REPO_ROOT, "revise", "output", "tbl")

cat("REPO_ROOT:", REPO_ROOT, "\nFIG_DIR:  ", FIG_DIR, "\nTBL_DIR:  ", TBL_DIR, "\n\n")

# ------------------------------------------------------------
# 1. 重出
# ------------------------------------------------------------
# 輸出 xlsx 若正被 Excel 開啟（會留下 ~$ 開頭的鎖定檔），write.xlsx 只會警告不會停，
# 結果是舊檔留在原地。這裡直接中止，避免圖新表舊。
locks <- list.files(TBL_DIR, pattern = "^~[$]", all.files = TRUE)
if (length(locks) > 0) {
  stop("請先關閉 Excel 中開啟的檔案再執行：", paste(sub("^~[$]", "", locks), collapse = "、"))
}

rmd <- file.path(code_dir, "115期末-修訂.Rmd")
r   <- file.path(code_dir, "115期末-修訂.R")

knitr::purl(rmd, output = r, documentation = 0, quiet = TRUE)
cat("已抽出：", r, "\n")

t0 <- Sys.time()
setwd(code_dir)
source(r, encoding = "UTF-8", echo = FALSE)

cat("\n============================================================\n")
cat("重出完成。耗時：", format(round(difftime(Sys.time(), t0, units = "mins"), 1)), "\n")
cat("fig：", length(list.files(FIG_DIR, pattern = "[.]png$")), "張\n")
cat("tbl：", length(list.files(TBL_DIR, pattern = "[.]xlsx$")), "份\n")
cat("============================================================\n")

# ------------------------------------------------------------
# 2. 對帳
# ------------------------------------------------------------
old_dir <- file.path(REPO_ROOT, "實作測試")
new_dir <- TBL_DIR
rev_dir <- file.path(REPO_ROOT, "review", "A-資料缺漏與異常", "output")
out_dir <- file.path(REPO_ROOT, "revise", "output")

files <- c("TPASS使用比例.xlsx", "月平均搭乘里程.xlsx",
           "平均每日使用次數及其族群分布.xlsx", "平均轉乘次數.xlsx")

diffs <- list(); summ <- list()

# 花蓮市區第 3 章改用替代指標（B2、甲案）：分頁與欄位改名，對帳時對回舊名稱比較
sheet_map <- c("平均每日_花蓮市區" = "單日平均_花蓮市區")
col_map <- list(
  "單日平均_花蓮市區" = c(使用人合計日數 = "該月使用人數", 單日平均使用次數 = "每人每月使用次數"),
  "全部乘客_花蓮市區" = c(使用人合計日數 = "使用人數"),
  "TPASS_花蓮市區"    = c(使用人合計日數 = "使用人數")
)

for (f in files) {
  for (s in getSheetNames(file.path(old_dir, f))) {
    o <- as.data.table(read.xlsx(file.path(old_dir, f), sheet = s))
    s_new <- if (s %in% names(sheet_map)) sheet_map[[s]] else s
    n <- as.data.table(read.xlsx(file.path(new_dir, f), sheet = s_new))
    if (s_new %in% names(col_map)) { cm <- col_map[[s_new]]; setnames(n, names(cm), unname(cm)) }
    stopifnot(nrow(o) == nrow(n))
    dropped <- setdiff(names(o), names(n)); added <- setdiff(names(n), names(o))
    if (length(dropped) || length(added))
      cat(sprintf("[結構差異] %s / %s → %s：舊有新無 %s；新有舊無 %s\n", f, s, s_new,
                  paste(dropped, collapse = ","), paste(added, collapse = ",")))
    cols <- setdiff(intersect(names(o), names(n)), "月份")
    d <- rbindlist(lapply(cols, function(cn) {
      ov <- o[[cn]]; nv <- n[[cn]]
      chg <- which(!(is.na(ov) & is.na(nv)) & (is.na(ov) != is.na(nv) | abs(ov - nv) > 1e-9))
      if (length(chg) == 0) return(NULL)
      data.table(檔案 = f, 分頁 = s, 月份 = o$月份[chg], 欄位 = cn,
                 舊值 = ov[chg], 新值 = nv[chg],
                 差異 = nv[chg] - ov[chg],
                 `差異%` = round(100 * (nv[chg] - ov[chg]) / ov[chg], 2))
    }))
    diffs[[paste(f, s)]] <- d
    summ[[paste(f, s)]] <- data.table(檔案 = f, 分頁 = s,
                                      差異儲存格數 = if (is.null(d)) 0L else nrow(d),
                                      差異月份 = if (is.null(d)) "" else paste(unique(d$月份), collapse = " "))
  }
}

diffs <- rbindlist(diffs); summ <- rbindlist(summ)
fwrite(diffs, file.path(out_dir, "數值對帳.csv"), bom = TRUE)
fwrite(summ,  file.path(out_dir, "數值對帳-摘要.csv"), bom = TRUE)

cat("\n===== 新舊 xlsx 差異摘要 =====\n"); print(summ)
cat("\n===== 差異超過 0.5% 的儲存格 =====\n")
print(diffs[abs(`差異%`) > 0.5][order(檔案, 分頁, 月份)])

# 與 review 稽核腳本的獨立重算交叉比對（應完全一致）
cat("\n===== 與 review 獨立重算交叉比對 =====\n")
chk <- function(label, new_dt, rev_dt, key_new, key_rev, pairs) {
  # 兩邊同名欄位（如 使用人數）merge 後會變 .x/.y，先把 review 側改名避免撞名
  rev_dt <- copy(rev_dt)
  for (p in names(pairs)) if (pairs[[p]] %in% names(new_dt) && pairs[[p]] != key_rev) {
    setnames(rev_dt, pairs[[p]], paste0("rev_", pairs[[p]])); pairs[[p]] <- paste0("rev_", pairs[[p]])
  }
  m <- merge(new_dt, rev_dt, by.x = key_new, by.y = key_rev)
  bad <- 0L
  for (p in names(pairs)) {
    dlt <- abs(m[[p]] - m[[pairs[[p]]]])
    nbad <- sum(dlt > 1e-6, na.rm = TRUE); bad <- bad + nbad
    cat(sprintf("  %-40s %-28s 最大差 %.4g  不一致 %d/%d\n", label, paste(p, "vs", pairs[[p]]), max(dlt, na.rm = TRUE), nbad, nrow(m)))
  }
  invisible(bad)
}
rd <- function(f) fread(file.path(rev_dir, f), encoding = "UTF-8")

# 花蓮公路：對 A4 列數基準（A4 為去重前值；A7 去重使花蓮公路少 20 列，人次差 ≤3、區間 ±1 屬預期）
n <- as.data.table(read.xlsx(file.path(new_dir, "平均每日使用次數及其族群分布.xlsx"), sheet = "平均每日_花蓮公路"))
chk("平均每日_花蓮公路 vs A4", n, rd("A4-列數基準_花蓮公路平均每日使用次數.csv"), "月份", "民國月份",
    list(該月總使用次數 = "總使用次數_列數", 該月使用人數 = "使用人數", 平均每日使用次數 = "平均每日_列數基準"))
n <- as.data.table(read.xlsx(file.path(new_dir, "平均每日使用次數及其族群分布.xlsx"), sheet = "全部乘客_花蓮公路"))
chk("全部乘客_花蓮公路 vs A4", n, rd("A4-列數基準_花蓮公路搭乘次數區間.csv"), "月份", "民國月份",
    list(僅一次_人數 = "僅一次", 二至四次_人數 = "二至四次", 五次以上_人數 = "五次以上", 使用人數 = "使用人數"))

# 市區兩組（A7 去重後）：對 A7 腳本的去重重算（版本 = 去重）。A3／A4 列數基準為去重前值，不再比對。
a7 <- rd("A7-指標影響-使用次數與區間.csv")[版本 == "去重"]
for (g in c("花蓮市區", "臺東市區")) {
  r7 <- a7[資料 == g]
  n <- as.data.table(read.xlsx(file.path(new_dir, "TPASS使用比例.xlsx"), sheet = g))
  chk(paste0("TPASS比例_", g, " vs A7去重"), n, r7, "月份", "月",
      list(總搭乘人次 = "總使用次數", TPASS搭乘人次 = "TPASS人次"))
  for (pre in c("全部", "TPASS")) {
    s <- if (pre == "全部") paste0("全部乘客_", g) else paste0("TPASS_", g)
    n <- as.data.table(read.xlsx(file.path(new_dir, "平均每日使用次數及其族群分布.xlsx"), sheet = s))
    chk(paste0(s, " vs A7去重"), n, r7, "月份", "月",
        list(僅一次_人數 = paste0(pre, "_僅一次"), 二至四次_人數 = paste0(pre, "_二至四次"), 五次以上_人數 = paste0(pre, "_五次以上")))
  }
}
n <- as.data.table(read.xlsx(file.path(new_dir, "平均每日使用次數及其族群分布.xlsx"), sheet = "平均每日_臺東市區"))
chk("平均每日_臺東市區 vs A7去重", n, a7[資料 == "臺東市區"], "月份", "月",
    list(該月總使用次數 = "總使用次數", 該月使用人數 = "使用人數", 平均每日使用次數 = "平均每日使用次數"))
n <- as.data.table(read.xlsx(file.path(new_dir, "平均每日使用次數及其族群分布.xlsx"), sheet = "單日平均_花蓮市區"))
chk("單日平均_花蓮市區 vs A7去重", n, a7[資料 == "花蓮市區"], "月份", "月",
    list(該月總使用次數 = "總使用次數", 使用人合計日數 = "使用人合計日數", 單日平均使用次數 = "單日平均使用次數"))
# 花蓮市區里程 vs A6（A6 為加權版，115/04 允許小差）
n <- as.data.table(read.xlsx(file.path(new_dir, "月平均搭乘里程.xlsx"), sheet = "花蓮市區"))
chk("花蓮市區里程 vs A6（加權版，僅供參考）", n, rd("A6-311稀釋效果.csv"), "月份", "民國月份",
    list(平均搭乘里程_公里 = "全部_里程"))

cat("\n已寫出：", file.path(out_dir, "數值對帳.csv"), "\n")
