# analysis/05_extras.R --------------------------------------------------------
# 興味深い切り口: 年齢、開催国優位、メダル獲得国の広がり。
#
# docs/analysis-ideas.md に挙げた問いのうち、まず 3 つを図にしたもの。

source("R/setup.R")
ensure_packages()
source("R/theme_olympic.R")
source("R/prepare_data.R")

message("05_extras: 実行中")

# --- fig13: 競技ごとの年齢分布 -----------------------------------------------
# 年齢は 1960 年以降ほぼ欠損なし。全期間だと初期大会の高齢層に引っ張られるので
# 1960 年以降に限定する。
age_by_sport <- athletes |>
  filter(year >= 1960, !is.na(age)) |>
  group_by(sport) |>
  summarise(
    n = n(),
    q25 = quantile(age, 0.25),
    median = median(age),
    q75 = quantile(age, 0.75),
    .groups = "drop"
  ) |>
  filter(n >= 1500)

# 年齢は整数なので中央値が同値の競技が多い。slice_* は既定で同値を全て返すため、
# 実際の表示件数は 8 + 8 より多くなる。切り捨てると恣意的な選択になるので同値は残す。
extremes <- bind_rows(
  slice_min(age_by_sport, median, n = 8) |> mutate(grp = "若い競技"),
  slice_max(age_by_sport, median, n = 8) |> mutate(grp = "年齢が高い競技")
) |>
  mutate(sport = fct_reorder(sport, median))

youngest <- slice_min(extremes, median, n = 1, with_ties = FALSE)
oldest <- slice_max(extremes, median, n = 1, with_ties = FALSE)

p13 <- ggplot(extremes, aes(median, sport, colour = grp)) +
  geom_segment(aes(x = q25, xend = q75, yend = sport), linewidth = 1.6, alpha = 0.35) +
  geom_point(size = 2.8) +
  geom_text(aes(label = round(median, 1)), colour = PAL$ink_2,
            size = 2.9, vjust = -1.2, show.legend = FALSE) +
  scale_colour_olympic(name = NULL) +
  scale_x_continuous(breaks = seq(14, 40, 2)) +
  # 最上段のラベルが枠外に出ないよう上に余白を足す
  scale_y_discrete(expand = expansion(add = c(0.6, 1.1))) +
  labs(
    title = sprintf("%s と %s で選手の年齢は %d 歳離れる",
                    youngest$sport, oldest$sport,
                    round(oldest$median - youngest$median)),
    subtitle = paste0("競技ごとの年齢中央値（帯は四分位範囲）。1960 年以降、1,500 行以上の競技から上下 8 競技\n",
                      "中央値が同値の競技も残しているため計 ", nrow(extremes), " 競技"),
    x = "年齢", y = NULL,
    caption = paste0(SOURCE_CAPTION,
                     "\n馬術・射撃・セーリングは道具や馬の扱いが効くため選手寿命が長い。")
  ) +
  theme_olympic()

save_fig(p13, "fig13_age_by_sport.png", height = 7)

# --- fig14: 開催国優位 -------------------------------------------------------
# 開催国は自国開催の大会でメダルシェアを伸ばすか。
# 「その大会でのシェア」を「前後の大会での自国の平均シェア」と比べる。
edition_totals <- medals_official |>
  group_by(games, year, season) |>
  summarise(all_medals = sum(total), .groups = "drop")

shares <- medals_official |>
  left_join(edition_totals, by = c("games", "year", "season")) |>
  mutate(share = 100 * total / all_medals) |>
  select(noc, year, season, share)

host_effect <- host_noc |>
  filter(!is.na(host_noc)) |>
  rowwise() |>
  mutate(
    home = {
      v <- shares$share[shares$noc == host_noc & shares$year == year &
                          shares$season == season]
      if (length(v) == 0) NA_real_ else v[1]
    },
    # 同一シーズンの、前後 2 大会（自国開催を除く）を基準線にする
    baseline = {
      v <- shares$share[shares$noc == host_noc & shares$season == season &
                          shares$year != year &
                          abs(shares$year - year) <= 9]
      if (length(v) == 0) NA_real_ else mean(v)
    }
  ) |>
  ungroup() |>
  filter(!is.na(home), !is.na(baseline)) |>
  mutate(
    lift = home - baseline,
    label = paste0(host_country, " ", year)
  )

message("  開催国優位: ", nrow(host_effect), " 大会で比較, ",
        "上振れした割合 ", round(100 * mean(host_effect$lift > 0)), "%")

# 散布図（基準線 vs 自国開催時）にすると 1904 セントルイスの 89% が
# スケールを支配して他の 56 大会が潰れる。差分そのものを時系列で見る。
share_of_positive <- round(100 * mean(host_effect$lift > 0))

p14 <- ggplot(host_effect, aes(year, lift)) +
  geom_hline(yintercept = 0, colour = PAL$axis, linewidth = 0.5) +
  geom_segment(aes(xend = year, yend = 0), colour = PAL$grid, linewidth = 0.6) +
  geom_point(aes(colour = season), size = 2.6, alpha = 0.9) +
  geom_text(
    data = slice_max(host_effect, lift, n = 5),
    aes(label = label), colour = PAL$ink, size = 2.9, vjust = -1
  ) +
  scale_colour_olympic(name = NULL) +
  scale_x_continuous(breaks = seq(1900, 2020, 20), expand = expansion(mult = 0.06)) +
  scale_y_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "pt")) +
  labs(
    title = paste0("開催国は ", nrow(host_effect), " 大会中 ", share_of_positive,
                   "% でメダルシェアを伸ばした"),
    subtitle = "自国開催時のメダルシェア − 前後 2 大会（同シーズン）の平均シェア。上振れ上位 5 大会にラベル",
    x = NULL, y = "シェアの差（ポイント）",
    caption = paste0(
      SOURCE_CAPTION,
      "\n因果ではない。出場枠の優遇・強化投資・実施種目の選択など複数の経路が混ざる。",
      "\n初期大会は参加国が少なく開催国の比率が構造的に高い（1904 セントルイスは +72pt）。"
    )
  ) +
  theme_olympic()

save_fig(p14, "fig14_host_advantage.png", width = 9.5, height = 5.5)

# --- fig15: メダルを取った国の広がり -----------------------------------------
first_medal <- medals_official |>
  filter(total > 0) |>
  group_by(noc, country) |>
  summarise(first_year = min(year), .groups = "drop")

spread <- tibble(year = sort(unique(medals_official$year))) |>
  rowwise() |>
  mutate(
    n_ever = sum(first_medal$first_year <= year),
    n_this = length(unique(medals_official$noc[medals_official$year == year &
                                                 medals_official$total > 0]))
  ) |>
  ungroup() |>
  pivot_longer(c(n_ever, n_this), names_to = "metric", values_to = "n") |>
  mutate(metric = factor(
    metric, levels = c("n_ever", "n_this"),
    labels = c("これまでに 1 度でも獲得した国の累計", "その年に獲得した国")
  ))

p15 <- ggplot(spread, aes(year, n, colour = metric)) +
  geom_line(linewidth = 0.9) +
  scale_colour_olympic(name = NULL) +
  scale_x_continuous(breaks = seq(1900, 2020, 20)) +
  labs(
    title = "メダルを取る国は増え続けている",
    subtitle = "累計 150 か国以上が 1 度はメダルを獲得。1 大会あたりでも 90 か国前後が表彰台に乗る",
    x = NULL, y = "国・地域数 (NOC)",
    caption = paste0(SOURCE_CAPTION,
                     "\n夏季・冬季を合算。消滅した NOC（URS・GDR 等）も別個に数えている。")
  ) +
  theme_olympic()

save_fig(p15, "fig15_medal_spread.png")

message("05_extras: 完了")
