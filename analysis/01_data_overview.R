# analysis/01_data_overview.R -------------------------------------------------
# データの全体像: 大会ごとの規模と、欠損の年代分布。
#
# ここは「何が分析できて、何ができないか」を決める図。
# 特に fig02 は、身長体重とメダルの分析可能な期間を規定する。

source("R/setup.R")
ensure_packages()
source("R/theme_olympic.R")
source("R/prepare_data.R")

message("01_data_overview: 実行中")

# --- fig01: 大会規模の推移 ---------------------------------------------------
participation <- athletes |>
  count(year, season, name = "rows") |>
  arrange(year)

cancelled <- editions_clean |>
  filter(is.na(participants)) |>
  select(year, season)

p1 <- ggplot(participation, aes(year, rows, colour = season)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.6) +
  geom_vline(data = cancelled, aes(xintercept = year),
             colour = PAL$muted, linetype = "dotted", linewidth = 0.4) +
  annotate("text", x = 1930, y = 15500, label = "点線 = 中止大会\n(1916 / 1940 / 1944)",
           colour = PAL$muted, size = 3, hjust = 0.5, lineheight = 1.1) +
  scale_colour_olympic(name = NULL) +
  scale_x_continuous(breaks = seq(1900, 2020, 20)) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "大会規模は 1990 年代以降ほぼ横ばい",
    subtitle = "1 大会あたりの「選手 × 種目」行数。IOC の種目数上限により近年は頭打ち",
    x = NULL, y = "選手 × 種目の行数",
    caption = SOURCE_CAPTION
  ) +
  theme_olympic()

save_fig(p1, "fig01_participation.png")

# --- fig02: 欠損率の年代推移 -------------------------------------------------
# 分析設計を左右する図。
#  - 身長体重: 1950 年代以前は 7〜9 割欠損。2018 年以降も 4〜7 割に逆戻り。
#  - noc: 2018 年以降のみ欠損が出る（1 割弱）。
missing_by_decade <- athletes |>
  group_by(decade) |>
  summarise(
    across(c(height, weight, age, noc), \(x) 100 * mean(is.na(x))),
    .groups = "drop"
  ) |>
  pivot_longer(-decade, names_to = "variable", values_to = "pct_na") |>
  mutate(variable = factor(variable, levels = c("height", "weight", "age", "noc")))

p2 <- ggplot(missing_by_decade, aes(decade, pct_na, colour = variable)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.6) +
  annotate("rect", xmin = 1955, xmax = 2015, ymin = -Inf, ymax = Inf,
           fill = PAL$cat[1], alpha = 0.05) +
  annotate("text", x = 1985, y = 95, label = "身体データが使える期間",
           colour = PAL$ink_2, size = 3.2) +
  scale_colour_olympic(name = NULL) +
  scale_x_continuous(breaks = seq(1890, 2020, 20)) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 100)) +
  labs(
    title = "身長・体重の欠損は 1960 年代に解消し、2018 年以降にぶり返す",
    subtitle = "2020 年代の身長欠損は 5 割超。最近の大会ほど選手個票の整備が追いついていない",
    x = NULL, y = "欠損率",
    caption = SOURCE_CAPTION
  ) +
  theme_olympic()

save_fig(p2, "fig02_missingness.png")

# --- fig03: 選手データのメダル網羅性の検証 -----------------------------------
# 選手データから種目単位に集計したメダル数と、公式メダル表の総数を突き合わせる。
# 2018 年以降は選手データ側が明確に足りない。
coverage <- event_medals |>
  count(games, year, season, name = "derived") |>
  left_join(
    medals_official |> group_by(games) |> summarise(official = sum(total), .groups = "drop"),
    by = "games"
  ) |>
  mutate(coverage = 100 * derived / official)

p3 <- ggplot(coverage, aes(year, coverage, colour = season)) +
  geom_hline(yintercept = 100, colour = PAL$axis, linewidth = 0.4) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.6) +
  geom_vline(xintercept = MEDAL_ATHLETE_COMPLETE_MAX_YEAR + 1,
             colour = PAL$cat[8], linetype = "dashed", linewidth = 0.5) +
  annotate("text", x = 2016, y = 55, label = "2016 年まで\n選手データで集計可",
           colour = PAL$ink_2, size = 3, hjust = 1, lineheight = 1.1) +
  scale_colour_olympic(name = NULL) +
  scale_x_continuous(breaks = seq(1900, 2020, 20)) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 110)) +
  labs(
    title = "選手データからのメダル集計は 2016 年までしか信用できない",
    subtitle = "公式メダル表の総数を 100% としたときの、選手データ由来の種目メダル数の割合",
    x = NULL, y = "公式総数に対する網羅率",
    caption = paste0(SOURCE_CAPTION,
                     "\n国別メダル数の分析には medal_table（公式集計）を使うこと。")
  ) +
  theme_olympic()

save_fig(p3, "fig03_medal_coverage.png")

message("01_data_overview: 完了")
