# R/theme_olympic.R -----------------------------------------------------------
# 全図で共通のテーマ・配色・保存ヘルパ。
#
# 配色の根拠（docs/preprocessing.md「配色」節も参照）:
#   - カテゴリ配色は固定順で使う。系列が増えても色を循環させない。
#   - 8 スロットは隣接ペア基準で色覚多様性（CVD）検証済み。
#     散布図など「全ペアが隣り合う」形式では先頭 3 スロットまでに抑える。
#   - メダルは Gold > Silver > Bronze の順序尺度なので、
#     カテゴリ色ではなく単一色相の順序ランプ（濃→淡）で表す。

suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
})

PAL <- list(
  surface = "#fcfcfb",
  ink = "#0b0b0b",
  ink_2 = "#52514e",
  muted = "#898781",
  grid = "#e1e0d9",
  axis = "#c3c2b7",

  # カテゴリ（固定順・循環禁止）
  cat = c(
    "#2a78d6", # 1 blue
    "#eb6834", # 2 orange
    "#1baf7a", # 3 aqua
    "#eda100", # 4 yellow
    "#e87ba4", # 5 magenta
    "#008300", # 6 green
    "#4a3aa7", # 7 violet
    "#e34948"  # 8 red
  ),
  # 散布図・バブルなど全ペアが比較される形式での上限
  cat_all_pairs_max = 3L,

  # メダル（順序ランプ・単一色相）
  medal = c(Gold = "#184f95", Silver = "#3987e5", Bronze = "#86b6ef"),

  # 連続量（単一色相 light -> dark）
  seq = c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf", "#184f95", "#0d366b")
)

# --- フォント ----------------------------------------------------------------
# 図のラベルは日本語を含む。systemfonts の既定は Arial 等の欧文フォントで、
# 日本語はグリフフォールバックに頼ることになる。フォールバックは環境によって
# 効いたり効かなかったりする（CI の Linux ランナーには日本語フォントが
# 入っていないため豆腐文字になる）ので、使う書体を明示して解決する。
#
# 見つからなければ黙って豆腐文字を出さずに落とす。
resolve_jp_family <- function() {
  candidates <- c(
    "Noto Sans JP", "Noto Sans CJK JP", "Source Han Sans JP",  # Linux / 共通
    "Yu Gothic", "Meiryo", "BIZ UDPGothic", "MS Gothic",       # Windows
    "Hiragino Sans", "Hiragino Kaku Gothic ProN"               # macOS
  )
  available <- unique(systemfonts::system_fonts()$family)
  hit <- candidates[candidates %in% available]
  if (length(hit) == 0) {
    stop(
      "図のラベルに使う日本語フォントが見つかりません。\n",
      "  Debian/Ubuntu: sudo apt-get install -y fonts-noto-cjk\n",
      "  macOS/Windows: 通常は標準搭載。systemfonts::system_fonts() で確認してください。",
      call. = FALSE
    )
  }
  hit[1]
}

BASE_FAMILY <- resolve_jp_family()

# geom_text / annotate("text") はテーマの書体を継承せずデバイス既定を使うため、
# 図中の注記も日本語が出るよう geom の既定値ごと差し替える。
update_geom_defaults("text", list(family = BASE_FAMILY))
update_geom_defaults("label", list(family = BASE_FAMILY))

theme_olympic <- function(base_size = 12, base_family = BASE_FAMILY) {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background = element_rect(fill = PAL$surface, colour = NA),
      panel.background = element_rect(fill = PAL$surface, colour = NA),
      # グリッドは後退させる。値の読み取りを助ける方向にだけ引く。
      panel.grid.major = element_line(colour = PAL$grid, linewidth = 0.3),
      panel.grid.minor = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_text(colour = PAL$muted, size = rel(0.85)),
      axis.title = element_text(colour = PAL$ink_2, size = rel(0.9)),
      plot.title = element_text(colour = PAL$ink, face = "bold",
                                size = rel(1.15), margin = margin(b = 4)),
      plot.subtitle = element_text(colour = PAL$ink_2, size = rel(0.9),
                                   margin = margin(b = 10)),
      plot.caption = element_text(colour = PAL$muted, size = rel(0.75),
                                  hjust = 0, margin = margin(t = 12)),
      plot.caption.position = "plot",
      plot.title.position = "plot",
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_text(colour = PAL$ink_2, size = rel(0.85)),
      legend.text = element_text(colour = PAL$ink_2, size = rel(0.85)),
      legend.key = element_blank(),
      strip.text = element_text(colour = PAL$ink, face = "bold", size = rel(0.9)),
      plot.margin = margin(14, 18, 10, 14)
    )
}

scale_colour_olympic <- function(...) {
  discrete_scale("colour", palette = function(n) {
    stopifnot(n <= length(PAL$cat))
    PAL$cat[seq_len(n)]
  }, ...)
}

scale_fill_olympic <- function(...) {
  discrete_scale("fill", palette = function(n) {
    stopifnot(n <= length(PAL$cat))
    PAL$cat[seq_len(n)]
  }, ...)
}

scale_fill_medal <- function(...) {
  scale_fill_manual(values = PAL$medal, ...)
}

scale_colour_medal <- function(...) {
  scale_colour_manual(values = PAL$medal, ...)
}

# 出典を全図に固定で入れる
SOURCE_CAPTION <- paste(
  "Data: moderndive/olympicAthletes (CRAN) — 原典 Olympedia.org",
  "| github.com/gghatano/olympic-athletes-eda",
  sep = " "
)

save_fig <- function(plot, file, width = 9, height = 5.5, dpi = 150) {
  path <- file.path(FIG_DIR, file)
  ggsave(
    filename = path, plot = plot,
    width = width, height = height, dpi = dpi,
    units = "in", bg = PAL$surface,
    device = ragg::agg_png
  )
  message("  図を保存: figures/", file)
  invisible(path)
}
