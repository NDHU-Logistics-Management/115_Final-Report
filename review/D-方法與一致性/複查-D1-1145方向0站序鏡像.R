# ============================================================
# D1／C5 複查（續五）：1145 方向 0 的站序補配把站序套到從另一端起算的表上
#
# 資料事實（複查-D1公路站序補配與反向樣本.log、本腳本）：
#   - 花蓮公路距離表 1145 方向 0 與方向 1 兩張表完全相同（126 站、同代碼同順序），站序 1 = 成功、126 = 花蓮端。
#   - 票證方向 1 的站序與表一致（成功 = 1 → 花蓮轉運站 = 126），代碼 99.1% 在表中。
#   - 票證方向 0 的站序從花蓮端起算（花蓮轉運站 = 1、中華路 = 5、海洋公園 = 23、豐濱 = 64、成功 = 126），
#     代碼只有 1.2% 在 1145 表中（93.8% 出現在其他路線，即對向站牌）。
#   - 同名站在兩種編號下滿足 票證站序 k ↔ 表站序 127 − k（中華路 5↔122、海洋公園 23↔104、豐濱 64↔63、成功 126↔1）。
#   整併版因 C1 走站序精確補配，把方向 0 票證的站序 k 直接對到表站序 k（從成功端數），
#   例如「花蓮轉運站→海洋公園」被算成表站序 1→23（成功→田組）= 26.88 公里。
#
# 本腳本計算：方向 0 票證在「整併版現行（站序直接對）」與「鏡像（站序 127 − k）」兩種對法下的里程，
# 以及若改用鏡像，花蓮公路月平均變動多少。鏡像只是依站名對應關係推得的另一種對法，不是經核對的正確值。
#
# 輸出：output/D1-1145方向0-鏡像對照.csv、output/D1-1145方向0-月平均影響.csv、同名 .log
# 耗時：約 1.5 分鐘
# ============================================================

suppressPackageStartupMessages({ library(data.table); library(openxlsx) })

this_file <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
if (length(this_file) == 0) this_file <- "複查-D1-1145方向0站序鏡像.R"
code_dir  <- normalizePath(dirname(this_file))
REPO_ROOT <- normalizePath(file.path(code_dir, "..", ".."))
source(file.path(REPO_ROOT, "review", "A-資料缺漏與異常", "00-setup.R"))
out_dir <- file.path(code_dir, "output")
t0 <- Sys.time()

KEY <- c("搭乘路線名稱", "搭乘附屬路線名稱", "搭乘公車路線方向")
cm <- fread(file.path(data_path, "公車站間距離資料", "花蓮縣公路客運站間距離資料.csv"), colClasses = list(character = 1:5))
strip_bom(cm)
cm[, 站序資料 := as.numeric(站序資料)]; cm[, 站間距離 := as.numeric(站間距離)]; cm[is.na(站間距離), 站間距離 := 0]
setorder(cm, 搭乘路線名稱, 搭乘附屬路線名稱, 搭乘公車路線方向, 站序資料)
cm[, 累積里程 := cumsum(站間距離), by = KEY]

tk <- in_window(read_tickets("公路客運2024_to_202606.csv",
  c("搭乘路線名稱", "搭乘附屬路線名稱", "搭乘公車路線方向", "上車站牌名稱", "下車站牌名稱",
    "上車計費站序資料", "下車站牌站序", "原始票證筆數")))
tk <- tk[搭乘路線名稱 %in% routes_hualien_THB & !is.na(原始票證筆數) & 原始票證筆數 > 0]
tk[, 搭乘附屬路線名稱 := trimws(as.character(搭乘附屬路線名稱))]
tk[, 搭乘公車路線方向 := as.character(搭乘公車路線方向)]
tk[, 上序 := suppressWarnings(as.numeric(上車計費站序資料))]
tk[, 下序 := suppressWarnings(as.numeric(下車站牌站序))]
tk[, 月份 := 民國月份]
tk[, tid := .I]

# 整併版現行：站序精確補配（同鍵同站序累積里程唯一才用）
sq <- cm[, .(n = uniqueN(累積里程), 里程 = if (uniqueN(累積里程) == 1) 累積里程[1] else NA_real_), by = c(KEY, "站序資料")]
up  <- merge(tk[!is.na(上序) & 上序 != -99, .(tid, 搭乘路線名稱, 搭乘附屬路線名稱, 搭乘公車路線方向, 站序資料 = 上序)],
             sq[, c(KEY, "站序資料", "里程"), with = FALSE], by = c(KEY, "站序資料"))[!is.na(里程), .(tid, 上里程 = 里程)]
dn  <- merge(tk[!is.na(下序) & 下序 != -99, .(tid, 搭乘路線名稱, 搭乘附屬路線名稱, 搭乘公車路線方向, 站序資料 = 下序)],
             sq[, c(KEY, "站序資料", "里程"), with = FALSE], by = c(KEY, "站序資料"))[!is.na(里程), .(tid, 下里程 = 里程)]
x <- merge(merge(tk, up, by = "tid"), dn, by = "tid")
x[, 里程_現行 := abs(下里程 - 上里程)]

# 對帳：與整併版 xlsx 一致
E <- as.data.table(read.xlsx(file.path(REPO_ROOT, "revise/output/tbl/月平均搭乘里程.xlsx"), sheet = "花蓮公路"))
chk <- merge(E[, .(月份, E = 平均搭乘里程_公里)], x[, .(現行 = round(sum(里程_現行) / .N / 1000, 2)), by = 月份], by = "月份")
cat("站序補配重現整併版 xlsx：最大差", max(abs(chk$E - chk$現行)), "公里\n")

# 1145 方向 0：鏡像對法
t1145 <- cm[搭乘附屬路線名稱 == "1145" & 搭乘公車路線方向 == "0", .(站序資料, 累積里程)]
N <- nrow(t1145)
y <- x[搭乘附屬路線名稱 == "1145" & 搭乘公車路線方向 == "0"]
y[, 上鏡 := t1145[match(N + 1 - 上序, 站序資料), 累積里程]]
y[, 下鏡 := t1145[match(N + 1 - 下序, 站序資料), 累積里程]]
y[, 里程_鏡像 := abs(下鏡 - 上鏡)]
cat(sprintf("\n1145 方向 0 票證（站序補配配到者）%d 筆：現行平均 %.2f 公里、鏡像平均 %.2f 公里；兩者相等者 %.1f%%\n",
            nrow(y), mean(y$里程_現行) / 1000, mean(y$里程_鏡像, na.rm = TRUE) / 1000,
            100 * mean(abs(y$里程_現行 - y$里程_鏡像) < 1, na.rm = TRUE)))
od <- y[, .(票證數 = .N, 上序 = 上序[1], 下序 = 下序[1],
            現行_公里 = round(里程_現行[1] / 1000, 2), 鏡像_公里 = round(里程_鏡像[1] / 1000, 2)),
        by = .(上車站牌名稱, 下車站牌名稱)][order(-票證數)]
cat("\n前 12 組起迄：\n"); print(head(od, 12))
fwrite(od, file.path(out_dir, "D1-1145方向0-鏡像對照.csv"), bom = TRUE)

# 若 1145 方向 0 改用鏡像，花蓮公路月平均
x[, 里程_鏡像版 := 里程_現行]
x[y[, .(tid, 里程_鏡像)], 里程_鏡像版 := i.里程_鏡像, on = "tid"]
m <- x[!is.na(里程_鏡像版), .(現行 = round(sum(里程_現行) / .N / 1000, 2), 鏡像版 = round(sum(里程_鏡像版) / .N / 1000, 2), 人次 = .N), by = 月份][order(月份)]
m[, 差 := round(現行 - 鏡像版, 2)]
cat("\n花蓮公路月平均：現行 vs 1145 方向 0 改鏡像\n"); print(m)
cat(sprintf("現行 − 鏡像版：平均 %.2f、最大 %.2f（%s）\n", mean(m$差), max(m$差), m[which.max(差), 月份]))
fwrite(m, file.path(out_dir, "D1-1145方向0-月平均影響.csv"), bom = TRUE)
cat("耗時：", format(round(difftime(Sys.time(), t0, units = "mins"), 1)), "\n")
