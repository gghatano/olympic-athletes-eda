# analysis/10_body_medalist_yearly.R -----------------------------------------
# 「時代とともに背が伸びる」影響を、年度内の引き算で落としてから
# メダリストの身長優位を測る。
#
# 05_body_medalist_gap.R は同じ問い（同じ種目の中で背は効くか）を
# 種目 × 性別 × 年代のセル内 z 化で扱った。ここではもっと素朴で直感的な、
# しかし交絡に強い別の道を通る。
#
#   1. **同じ大会・同じ種目・同じ性別**の中だけで、
#      メダリストの平均身長 − 非メダリストの平均身長 を出す（= 1 セル 1 値）。
#      同じ年に競った相手との差なので、時代による大型化はそのまま相殺される。
#      同じ種目に閉じているので、種目構成の偏り（走高跳と重量挙げの混在）も入らない。
#   2. その差の値を **競技ごと**に集め、箱ひげ図で分布として見る。
#      点推定 1 個ではなく「大会をまたいでどれくらい安定して差が出るか」まで見える。
#
# 有意性の判定は、セル平均どうしの素朴な t 検定ではなく、
# 選手行を単位にした固定効果回帰で行う（下の注参照）。
# 身長が概ね揃う 1960-2016 に限定する（BODY_YEARS）。

source("R/setup.R")
ensure_packages()
source("R/theme_olympic.R")
source("R/prepare_data.R")

message("10_body_medalist_yearly: 実行中")

# 記録ミス由来の外れ値を落とす（身長のみで判定。127-226 の外に実データは無い）。
HEIGHT_RANGE <- c(120, 230)

MIN_MED   <- 3L    # セルに必要なメダリスト数（金銀銅の最小構成）
MIN_NON   <- 5L    # セルに必要な非メダリスト数
MIN_CELLS <- 30L   # 競技を箱ひげ図に載せる最小セル数

body <- athletes |>
  filter(
    year >= BODY_YEARS[1], year <= BODY_YEARS[2],
    !is.na(height), height >= HEIGHT_RANGE[1], height <= HEIGHT_RANGE[2]
  )

# --- 1 セル = 大会 × 種目 × 性別 の中でのメダリスト身長差 --------------------
cells <- body |>
  group_by(sport, event, games, year, season, sex) |>
  summarise(
    n_med = sum(is_medalist),
    n_non = sum(!is_medalist),
    h_med = mean(height[is_medalist]),
    h_non = mean(height[!is_medalist]),
    .groups = "drop"
  ) |>
  filter(n_med >= MIN_MED, n_non >= MIN_NON) |>
  mutate(gap = h_med - h_non)

message("  対象セル: ", format(nrow(cells), big.mark = ","), " / ",
        n_distinct(cells$sport), " 競技（メダリスト", MIN_MED,
        "人以上・非メダリスト", MIN_NON, "人以上のセル）")

# --- 競技ごとの有意性: セル固定効果 + 選手クラスタ頑健標準誤差 ---------------
# セル平均どうしを素朴に t 検定すると独立性を仮定してしまうが、同じ選手が
# 複数のセルに跨がる（メダリスト行の約半数が複数回メダルの選手）ため、
# 実際にはセル間に相関がある。そこで選手行を単位に、身長を (大会 × 種目 × 性別)
# のセル内で除去（= 年度内の引き算に相当）したうえで is_medalist に回帰し、
# 選手 id をクラスタとした頑健標準誤差 (CR1) で評価する。
# 05 が団体競技のチーム相関をクラスタで扱ったのと同じ発想を、選手の反復出場に当てる。
fe_cluster_gap <- function(x) {
  x <- x |>
    group_by(cell) |>
    mutate(hy = height - mean(height), mx = m - mean(m)) |>
    ungroup()
  denom <- sum(x$mx^2)
  if (denom <= 0) return(tibble(est = NA_real_, se = NA_real_))
  beta <- sum(x$hy * x$mx) / denom
  resid <- x$hy - beta * x$mx
  meat <- 0
  for (ix in split(seq_len(nrow(x)), x$id)) {
    meat <- meat + sum(x$mx[ix] * resid[ix])^2
  }
  G <- n_distinct(x$id); n <- nrow(x); k <- 1 + n_distinct(x$cell)
  adj <- (G / (G - 1)) * ((n - 1) / max(n - k, 1))
  tibble(est = beta, se = sqrt(adj * meat / denom^2))
}

# 箱ひげに載せる競技（セル数 >= MIN_CELLS）と、その中央値（並び順に使う）
sport_cells <- cells |>
  group_by(sport) |>
  summarise(gap_median = median(gap), n_cells = n(), .groups = "drop") |>
  filter(n_cells >= MIN_CELLS)

# 対象セルに属する選手行だけを取り出して固定効果回帰にかける
rows <- body |>
  semi_join(cells, by = c("sport", "event", "games", "year", "season", "sex")) |>
  semi_join(sport_cells, by = "sport") |>
  mutate(cell = paste(event, games, sex), m = as.numeric(is_medalist))

sport_stats <- rows |>
  group_by(sport) |>
  group_modify(~ fe_cluster_gap(.x)) |>
  ungroup() |>
  left_join(sport_cells, by = "sport") |>
  mutate(
    lo = est - 1.96 * se,
    hi = est + 1.96 * se,
    verdict = case_when(
      lo > 0 ~ "背が高いほうが有利",
      hi < 0 ~ "背が低いほうが有利",
      TRUE   ~ "差がはっきりしない"
    ),
    verdict = factor(verdict, levels = c("背が高いほうが有利", "背が低いほうが有利",
                                         "差がはっきりしない"))
  )

n_pos <- sum(sport_stats$lo > 0, na.rm = TRUE)
n_neg <- sum(sport_stats$hi < 0, na.rm = TRUE)
message("  箱ひげ対象 ", nrow(sport_stats), " 競技 / 高身長が有利 ", n_pos,
        " / 低身長が有利 ", n_neg, "（選手idクラスタ頑健SE）")

# --- 図1: 競技ごとの身長差の分布（箱ひげ） -----------------------------------
plot_cells <- cells |>
  semi_join(sport_stats, by = "sport") |>
  left_join(select(sport_stats, sport, gap_median, verdict), by = "sport") |>
  mutate(sport = fct_reorder(sport, gap_median))

p1 <- ggplot(plot_cells, aes(gap, sport, fill = verdict)) +
  geom_vline(xintercept = 0, colour = PAL$axis, linewidth = 0.5) +
  geom_boxplot(outlier.size = 0.35, outlier.alpha = 0.18,
               linewidth = 0.35, colour = PAL$ink_2) +
  scale_fill_manual(
    values = c("背が高いほうが有利" = PAL$cat[1],
               "背が低いほうが有利" = PAL$cat[2],
               "差がはっきりしない" = PAL$muted),
    name = NULL
  ) +
  scale_x_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "cm")) +
  coord_cartesian(xlim = c(-8, 10)) +
  labs(
    title = sprintf("同じ大会の中で測っても、%d 競技でメダリストは有意に背が高い", n_pos),
    subtitle = paste0(
      "1 点 = ある大会・ある種目・ある性別での（メダリストの平均身長 − 非メダリストの平均身長）。\n",
      "同じ大会の中の差なので時代による大型化は相殺され、同じ種目に閉じるので種目構成も入らない。\n",
      BODY_YEARS[1], "-", BODY_YEARS[2], "、", nrow(sport_stats), " 競技・",
      format(nrow(plot_cells), big.mark = ","), " セル"
    ),
    x = "身長差（メダリスト − 非メダリスト、大会 × 種目 × 性別ごと）", y = NULL,
    caption = paste0(
      SOURCE_CAPTION,
      "\n色は各競技の身長差の 95% 信頼区間が 0 を跨ぐかで決めている",
      "（大会 × 種目 × 性別を固定効果、選手 id をクラスタとした頑健標準誤差）。箱は競技内のセル分布。",
      "\nこれは「その種目に出られた選手の中での傾向」であり因果ではない。体格に恵まれない選手はそもそも出場していない。"
    )
  ) +
  theme_olympic()

save_fig(p1, "body_medalist_yearly_gap.png", height = 7.5)

# --- 図2: 背は伸びたが、大会内の差は変わらない -------------------------------
# 方法の肝（年度内で引けば時代が消える）を 1 枚で示す。
# 身長は男女で 13cm ほど違い、女子種目の増加で出場者の男女比も変わるため、
# 男女を混ぜると伸びが相殺されて見える。性別で分けて描く。
# 夏冬も競技構成が違い 1994 年以降は開催年もずれるので、必ず分ける。
HEIGHT_LABEL <- "選手の身長（中央値, cm）"
GAP_LABEL    <- "メダリストの身長差（大会内・中央値, cm）"

height_by_year <- body |>
  group_by(season, year, sex) |>
  summarise(value = median(height), .groups = "drop") |>
  mutate(metric = HEIGHT_LABEL)

gap_by_year <- cells |>
  group_by(season, year, sex) |>
  summarise(value = median(gap), .groups = "drop") |>
  mutate(metric = GAP_LABEL)

metric_levels <- c(HEIGHT_LABEL, GAP_LABEL)
trend <- bind_rows(height_by_year, gap_by_year) |>
  mutate(metric = factor(metric, levels = metric_levels))

# 差の側にだけ 0 の基準線を引く（facet_grid は欠けた面変数を全パネルに描く）
zero_ref <- tibble(metric = factor(GAP_LABEL, levels = metric_levels), y = 0)

# 見出し用: 夏季で身長が男女それぞれ何 cm 伸びたか（端点の大会差）
gain <- height_by_year |>
  filter(season == "Summer") |>
  group_by(sex) |>
  summarise(g = value[which.max(year)] - value[which.min(year)], .groups = "drop")
gain_txt <- paste(sprintf("%s %+.0f cm",
                          ifelse(gain$sex == "Women", "女性", "男性"), gain$g),
                  collapse = " / ")

p2 <- ggplot(trend, aes(year, value, colour = sex)) +
  geom_hline(data = zero_ref, aes(yintercept = y), inherit.aes = FALSE,
             colour = PAL$axis, linewidth = 0.5) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.1) +
  facet_grid(metric ~ season, scales = "free_y", switch = "y") +
  scale_colour_olympic(name = NULL) +
  scale_x_continuous(breaks = seq(1960, 2016, 16)) +
  labs(
    title = "選手の身長は男女とも伸びたが、同じ大会の中でのメダリストの高さは変わらない",
    subtitle = paste0(
      "上段＝出場選手の身長の中央値（夏季で ", BODY_YEARS[1], "→", BODY_YEARS[2],
      " に ", gain_txt, "）。\n",
      "下段＝各大会内で測ったメダリストの身長差の中央値。時代とともに全員が大型化しても、",
      "その中の相対的な差は一定に留まる。\nだから大会内で引き算をすれば、時代の影響を落として優位だけを取り出せる。"
    ),
    x = NULL, y = NULL,
    caption = paste0(
      SOURCE_CAPTION,
      "\n身長は男女で大きく違い出場者の男女比も時代で変わるため性別で分ける。",
      "夏季と冬季も競技構成が違い、1994 年以降は開催年もずれるため分けて描いている。"
    )
  ) +
  theme_olympic() +
  theme(panel.spacing = unit(1.0, "lines"),
        strip.placement = "outside")

save_fig(p2, "body_height_vs_gap_time.png", width = 10, height = 6.5)

message("10_body_medalist_yearly: 完了")
