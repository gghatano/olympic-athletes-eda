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

# --- 競技ごとの要約（箱ひげに載せる競技を絞る） -----------------------------
# 各セルを 1 標本とみなし、差の平均が 0 と離れているかを t 検定で見る。
# セルは (大会 × 種目 × 性別) で互いに重なりが無いので、素の t 検定で足りる。
sport_stats <- cells |>
  group_by(sport) |>
  filter(n() >= MIN_CELLS) |>
  group_modify(~ {
    ci <- tryCatch(t.test(.x$gap)$conf.int, error = function(e) c(NA_real_, NA_real_))
    tibble(
      n_cells = nrow(.x),
      gap_median = median(.x$gap),
      gap_mean = mean(.x$gap),
      ci_lo = ci[1], ci_hi = ci[2]
    )
  }) |>
  ungroup() |>
  mutate(
    verdict = case_when(
      ci_lo > 0 ~ "背が高いほうが有利",
      ci_hi < 0 ~ "背が低いほうが有利",
      TRUE      ~ "差がはっきりしない"
    ),
    verdict = factor(verdict, levels = c("背が高いほうが有利", "背が低いほうが有利",
                                         "差がはっきりしない"))
  )

n_pos <- sum(sport_stats$ci_lo > 0, na.rm = TRUE)
n_neg <- sum(sport_stats$ci_hi < 0, na.rm = TRUE)
message("  箱ひげ対象 ", nrow(sport_stats), " 競技 / 高身長が有利 ", n_pos,
        " / 低身長が有利 ", n_neg)

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
      "\n色は各競技のセル平均の 95% 信頼区間が 0 を跨ぐかで決めている。箱は競技内のセル分布（中央値・四分位）。",
      "\nこれは「その種目に出られた選手の中での傾向」であり因果ではない。体格に恵まれない選手はそもそも出場していない。"
    )
  ) +
  theme_olympic()

save_fig(p1, "body_medalist_yearly_gap.png", height = 7.5)

# --- 図2: 背は伸びたが、大会内の差は変わらない -------------------------------
# 方法の肝（年度内で引けば時代が消える）を 1 枚で示す。
# 夏冬は競技構成が違い 1994 年以降は開催年もずれるので、必ず分ける。
height_by_year <- body |>
  group_by(season, year) |>
  summarise(value = median(height), .groups = "drop") |>
  mutate(metric = "選手全体の身長（中央値, cm）")

gap_by_year <- cells |>
  group_by(season, year) |>
  summarise(value = median(gap), .groups = "drop") |>
  mutate(metric = "メダリストの身長差（大会内・中央値, cm）")

metric_levels <- c("選手全体の身長（中央値, cm）",
                   "メダリストの身長差（大会内・中央値, cm）")
trend <- bind_rows(height_by_year, gap_by_year) |>
  mutate(metric = factor(metric, levels = metric_levels))

# 差の側にだけ 0 の基準線を引く（facet_grid は欠けた面変数を全パネルに描く）
zero_ref <- tibble(metric = factor(metric_levels[2], levels = metric_levels), y = 0)

# 見出し用: 夏季で身長が何 cm 伸びたか（端点の大会差）
summer_h <- filter(height_by_year, season == "Summer")
h_gain <- summer_h$value[which.max(summer_h$year)] -
  summer_h$value[which.min(summer_h$year)]

p2 <- ggplot(trend, aes(year, value, colour = season)) +
  geom_hline(data = zero_ref, aes(yintercept = y), inherit.aes = FALSE,
             colour = PAL$axis, linewidth = 0.5) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.1) +
  facet_grid(metric ~ season, scales = "free_y", switch = "y") +
  scale_colour_olympic(guide = "none") +
  scale_x_continuous(breaks = seq(1960, 2016, 16)) +
  labs(
    title = "選手の身長は伸びたが、同じ大会の中でのメダリストの高さは変わらない",
    subtitle = paste0(
      "上段＝出場選手全体の身長の中央値（夏季で ", BODY_YEARS[1], "→", BODY_YEARS[2],
      " に約 ", sprintf("%+.0f", h_gain), " cm）。\n",
      "下段＝各大会内で測ったメダリストの身長差の中央値。時代とともに全員が大型化しても、",
      "その中の相対的な差は一定に留まる。\nだから大会内で引き算をすれば、時代の影響を落として優位だけを取り出せる。"
    ),
    x = NULL, y = NULL,
    caption = paste0(
      SOURCE_CAPTION,
      "\n夏季と冬季は競技構成が違い、1994 年以降は開催年もずれるため分けて描いている。"
    )
  ) +
  theme_olympic() +
  theme(panel.spacing = unit(1.0, "lines"),
        strip.placement = "outside")

save_fig(p2, "body_height_vs_gap_time.png", width = 10, height = 6.5)

message("10_body_medalist_yearly: 完了")
