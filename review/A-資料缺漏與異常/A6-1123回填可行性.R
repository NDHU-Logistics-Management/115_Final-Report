# ============================================================
# A6（附）　以 311／311A 站間距離回填 1123 之可行性評估
#
# 問題：1123 無站間距離資料，能否借用其改號後繼路線 311／311A 的
#       站間距離，回推 113/11 以前 1123 的搭乘里程？
#
# 本腳本做五件事：
#   1. 由票證資料重建 1123 的站序表（代碼、站名、順序）
#   2. 整理 311／311A 的站序與累積里程，並補上站名
#   3. 比對兩者站牌集合：共用／1123 獨有／311 獨有
#   4. 逐一檢查 1123 的 27 個站間區間能否由 311／311A 直接提供距離
#   5. 依可提供距離的連續路段切成區塊，將 1123 旅次分類：
#        起訖同區塊（可回填）／跨越改線中段／觸及獨有站
#      並比較可回填與不可回填旅次的平均跨站數（判斷偏誤方向）
#   6.（選擇性）查其他 repo 的站點里程檔，看 1123 獨有站是否有座標
#
# 結論（2026-09-12）：不可行。詳見 A6-1123回填可行性評估.md
#
# 執行方式：
#   將工作目錄設到本腳本所在資料夾後 source("A6-1123回填可行性.R")
#   或直接執行 run-all.R
# ============================================================

if (!exists("emit")) source("00-setup.R")

norm <- normalize_stop_code_fixed


# ------------------------------------------------------------
# 1. 票證資料：1123 與 311 的上下車站
# ------------------------------------------------------------

hua <- read_tickets(
  "花蓮縣公車.csv",
  c("搭乘路線名稱", "搭乘公車路線方向",
    "上車站牌代碼", "上車站牌名稱", "上車計費站序資料",
    "下車站牌代碼", "下車站牌名稱", "下車站牌站序")
)
hua <- hua[搭乘路線名稱 %in% c("1123", "311")]

taps <- rbind(
  hua[, .(路線 = 搭乘路線名稱, 方向 = as.character(搭乘公車路線方向),
          站序 = as.integer(上車計費站序資料),
          代碼 = norm(上車站牌代碼), 站名 = 上車站牌名稱)],
  hua[, .(路線 = 搭乘路線名稱, 方向 = as.character(搭乘公車路線方向),
          站序 = as.integer(下車站牌站序),
          代碼 = norm(下車站牌代碼), 站名 = 下車站牌名稱)]
)
taps <- taps[!is.na(代碼) & !is.na(站序) & 站序 > 0]

# 1123 站序表：每個站序取刷卡次數最多的代碼，過濾雜訊
seq1123 <- taps[路線 == "1123", .(刷卡次數 = .N), by = .(方向, 站序, 代碼, 站名)]
seq1123 <- seq1123[, .SD[which.max(刷卡次數)], by = .(方向, 站序)][order(方向, 站序)]

emit(seq1123, "A6b-1123站序表.csv", "1123 站序表（由票證資料重建）")


# ------------------------------------------------------------
# 2. 311／311A 站序、累積里程、站名
# ------------------------------------------------------------

dist <- fread(
  file.path(data_path, "公車站間距離資料", "花蓮縣市區客運站間距離資料.csv"),
  showProgress = FALSE
)
strip_bom(dist)
dist <- dist[trimws(as.character(搭乘路線名稱)) == "311"]
dist[, `:=`(
  附屬 = trimws(as.character(搭乘附屬路線名稱)),
  方向 = as.character(搭乘公車路線方向),
  站序 = as.integer(站序資料),
  代碼 = norm(站牌代碼),
  站間距離 = as.numeric(站間距離)
)]
dist[is.na(站間距離), 站間距離 := 0]
setorder(dist, 附屬, 方向, 站序)
dist[, `:=`(pos = seq_len(.N), 累積 = cumsum(站間距離)), by = .(附屬, 方向)]

names311 <- taps[路線 == "311", .N, by = .(代碼, 站名)][, .SD[which.max(N)], by = 代碼]
dist311 <- merge(dist, names311[, .(代碼, 站名)], by = "代碼", all.x = TRUE, sort = FALSE)
setorder(dist311, 附屬, 方向, 站序)

emit(
  dist311[, .(附屬, 方向, 站序, 代碼, 站名, 站間距離, 累積)],
  "A6b-311站序與距離表.csv",
  "311／311A 站序、站間距離、累積里程"
)


# ------------------------------------------------------------
# 3. 站牌集合比對
# ------------------------------------------------------------

codes1123 <- unique(seq1123$代碼)
codes311  <- unique(dist311$代碼)

allnames <- rbind(seq1123[, .(代碼, 站名)], names311[, .(代碼, 站名)])
allnames <- allnames[, .SD[1], by = 代碼]

sets <- rbind(
  data.table(類別 = "共用",         代碼 = intersect(codes1123, codes311)),
  data.table(類別 = "1123 獨有",    代碼 = setdiff(codes1123, codes311)),
  data.table(類別 = "311/311A 獨有", 代碼 = setdiff(codes311, codes1123))
)
sets <- merge(sets, allnames, by = "代碼", all.x = TRUE, sort = FALSE)
sets <- merge(sets, seq1123[方向 == "0", .(代碼, `1123站序` = 站序)], by = "代碼", all.x = TRUE, sort = FALSE)
setorder(sets, 類別, `1123站序`, na.last = TRUE)

emit(sets, "A6b-站牌集合比對.csv", "站牌集合：共用／1123 獨有／311 獨有")

cat(sprintf("\n1123 站數 %d、311∪311A 站數 %d、共用 %d\n",
            length(codes1123), length(codes311), length(intersect(codes1123, codes311))))


# ------------------------------------------------------------
# 4. 1123 的每個站間區間能否由 311／311A 提供距離
#
#    對每個區間 (起, 迄)，在 311／311A 各 (附屬, 方向) 序列中尋找
#    兩站皆存在且迄在起之後者，取跨站數最少的一組：
#      跨站數 = 1 → 直接對應
#      跨站數 > 1 → 跨站加總（311/311A 在兩站之間多了新站）
#    找不到 → 該區間無法由 311／311A 提供
# ------------------------------------------------------------

lookup <- dist311[, .(附屬, 方向, 代碼, pos, 累積)]

match_interval <- function(a, b) {
  x <- merge(
    lookup[代碼 == a, .(附屬, 方向, pa = pos, ca = 累積)],
    lookup[代碼 == b, .(附屬, 方向, pb = pos, cb = 累積)],
    by = c("附屬", "方向")
  )
  x <- x[pb > pa]
  if (nrow(x) == 0) return(list(NA_real_, NA_character_, NA_integer_))
  x <- x[order(pb - pa)][1]
  list(x$cb - x$ca, paste0(x$附屬, " 方向", x$方向), as.integer(x$pb - x$pa))
}

s <- seq1123[方向 == "0"][order(站序)]
iv <- data.table(
  起站序 = s$站序[-nrow(s)], 起 = s$代碼[-nrow(s)], 起站名 = s$站名[-nrow(s)],
  迄站序 = s$站序[-1],       迄 = s$代碼[-1],       迄站名 = s$站名[-1]
)
iv[, c("距離_公尺", "來源", "跨站數") := match_interval(起, 迄), by = seq_len(nrow(iv))]
iv[, 可套用 := !is.na(距離_公尺)]
iv[, 類型 := fcase(
  is.na(跨站數), "無法提供",
  跨站數 == 1L, "直接對應",
  default = "跨站加總"
)]

emit(iv, "A6b-1123站間區間可套用性.csv", "1123 的 27 個站間區間：能否由 311／311A 提供距離")

cat(sprintf("\n27 個區間中可由 311/311A 提供距離者：%d（直接 %d、跨站加總 %d）；無法提供：%d\n",
            sum(iv$可套用), sum(iv$類型 == "直接對應"), sum(iv$類型 == "跨站加總"), sum(!iv$可套用)))


# ------------------------------------------------------------
# 5. 區塊切分與旅次分類
#
#    連續可套用的區間構成一個「區塊」；旅次起訖在同一區塊內
#    才能完整以 311／311A 距離回填。
# ------------------------------------------------------------

blk <- integer(nrow(s)); blk[1] <- 1L
for (i in seq_len(nrow(iv))) blk[i + 1] <- if (iv$可套用[i]) blk[i] else blk[i] + 1L
blocks <- data.table(站序 = s$站序, 代碼 = s$代碼, 站名 = s$站名, 區塊 = blk)
blocks[, 區塊內站數 := .N, by = 區塊]
blocks[, 區塊可用 := 區塊內站數 > 1]

emit(blocks, "A6b-1123可回填區塊.csv", "1123 站序依可套用區間切成的區塊（區塊內站數 > 1 者可回填）")

seq_of <- setNames(s$站序, s$代碼)
blk_of <- setNames(blocks$區塊, blocks$代碼)
usable_blk <- blocks[區塊可用 == TRUE, unique(區塊)]

trips <- hua[搭乘路線名稱 == "1123", .(on = norm(上車站牌代碼), off = norm(下車站牌代碼))]
trips <- trips[!is.na(on) & !is.na(off)]
trips[, `:=`(a = seq_of[on], b = seq_of[off], ba = blk_of[on], bb = blk_of[off])]
trips <- trips[!is.na(a) & !is.na(b)]
trips[, 跨站數 := abs(b - a)]
trips[, 分類 := fcase(
  !(on %in% codes311) | !(off %in% codes311), "至少一端為 1123 獨有站（改線中段）",
  ba == bb & ba %in% usable_blk,              "起訖同一可回填區塊",
  default =                                    "起訖皆為共用站，但路徑跨越改線中段"
)]

cls <- trips[, .(旅次數 = .N, 平均跨站數 = round(mean(跨站數), 1)), by = 分類]
cls[, `占比%` := round(100 * 旅次數 / sum(旅次數), 1)]
setorder(cls, -旅次數)

emit(cls, "A6b-1123旅次可回填分類.csv", "1123 旅次依可回填性分類（含平均跨站數）")


# ------------------------------------------------------------
# 6. 1123 獨有站是否有座標（查其他 repo 的 TDX 站點里程檔）
# ------------------------------------------------------------

if (!exists("MIDTERM_ROOT")) MIDTERM_ROOT <- "d:/Github/115_Midterm-Report"
coord_files <- c(
  file.path(MIDTERM_ROOT, "區域分析code", "花蓮市區公車站點里程.csv"),
  file.path(MIDTERM_ROOT, "區域分析code", "公路客運站點里程.csv")
)
coord_files <- coord_files[file.exists(coord_files)]

only1123 <- sets[類別 == "1123 獨有"]
only1123[, `:=`(有座標 = FALSE, 來源檔 = NA_character_, 經度 = NA_real_, 緯度 = NA_real_)]

for (f in coord_files) {
  cf <- fread(f, showProgress = FALSE)
  strip_bom(cf)
  if (!all(c("StopUID", "PositionLon", "PositionLat") %in% names(cf))) next
  cf[, 代碼 := norm(StopUID)]
  hit <- cf[代碼 %in% only1123$代碼, .SD[1], by = 代碼]
  if (nrow(hit)) {
    only1123[hit, on = "代碼", `:=`(
      有座標 = TRUE, 來源檔 = basename(f),
      經度 = i.PositionLon, 緯度 = i.PositionLat
    )]
  }
}

emit(
  only1123[, .(代碼, 站名, `1123站序`, 有座標, 來源檔, 經度, 緯度)],
  "A6b-1123獨有站座標可得性.csv",
  if (length(coord_files)) "1123 獨有站在既有站點里程檔中的座標可得性"
  else "（找不到站點里程檔，略過座標查詢）"
)


cat("\n[A6b] 完成。\n")
