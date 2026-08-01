# analysis/03_sport_gender.R --------------------------------------------------
# 競技と性別: 女性参加の拡大、競技ごとの偏り、種目構成の変化。
#
# 参加（sex 列）は全期間で欠損ゼロなので、メダルと違い 2018 年以降も使える。

source("R/setup.R")
ensure_packages()
source("R/theme_olympic.R")
source("R/prepare_data.R")

message("03_sport_gender: 実行中")

# --- fig07: 女性比率の推移 ---------------------------------------------------
gender_trend <- athletes |>
  group_by(year, season) |>
  summarise(
    share_women = 100 * mean(sex == "Women"),
    n = n(),
    .groups = "drop"
  )

p7 <- ggplot(gender_trend, aes(year, share_women, colour = season)) +
  geom_hline(yintercept = 50, colour = PAL$axis, linewidth = 0.4,
             linetype = "dashed") +
  annotate("text", x = 1900, y = 52.5, label = "男女同数",
           colour = PAL$muted, size = 3, hjust = 0) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  scale_colour_olympic(name = NULL) +
  scale_x_continuous(breaks = seq(1900, 2020, 20)) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 60)) +
  labs(
    title = "女性の参加比率は 120 年かけて 0% から 5 割弱へ",
    subtitle = "選手 × 種目行に占める女性の割合。2024 年パリ大会は 48.3%（冬季は夏季をやや下回る）",
    x = NULL, y = "女性の割合",
    caption = paste0(SOURCE_CAPTION,
                     "\n1896 年アテネ大会に女性選手はいない。1900 年パリ大会が初参加。")
  ) +
  theme_olympic()

save_fig(p7, "fig07_gender_trend.png")

# --- fig08: 競技ごとの男女比 -------------------------------------------------
# 直近 3 大会（夏季 2016-2024）に限る。過去を含めると、
# 女子種目が後から追加された競技が一律に低く出てしまう。
recent_sports <- athletes |>
  filter(season == "Summer", year >= 2016) |>
  group_by(sport) |>
  summarise(n = n(), share_women = 100 * mean(sex == "Women"), .groups = "drop") |>
  filter(n >= 300) |>
  mutate(sport = fct_reorder(sport, share_women))

p8 <- ggplot(recent_sports, aes(share_women, sport)) +
  geom_vline(xintercept = 50, colour = PAL$axis, linewidth = 0.4,
             linetype = "dashed") +
  geom_segment(aes(x = 50, xend = share_women, yend = sport),
               colour = PAL$grid, linewidth = 1.2) +
  geom_point(aes(colour = share_women >= 50), size = 3) +
  scale_colour_manual(
    values = c(`TRUE` = PAL$cat[1], `FALSE` = PAL$cat[2]),
    labels = c(`TRUE` = "女性が多い", `FALSE` = "男性が多い"),
    name = NULL, breaks = c("TRUE", "FALSE")
  ) +
  scale_x_continuous(labels = label_percent(scale = 1), limits = c(0, 103)) +
  labs(
    title = sprintf(
      "全体では 5 割弱でも、競技別では %.1f%% 〜 %.1f%% に散らばる",
      min(recent_sports$share_women),
      max(recent_sports$share_women[recent_sports$sport != "Synchronized Swimming"])
    ),
    subtitle = paste(
      "夏季 2016-2024 の選手 × 種目行に占める女性の割合。選手 300 行以上の 30 競技。",
      "アーティスティックスイミングは女性のみの競技なので 100%",
      sep = "\n"
    ),
    x = "女性の割合", y = NULL,
    caption = paste0(
      SOURCE_CAPTION,
      "\n5 割を超えるのは女性のみ競技を除くと 3 競技だけ（飛込 50.5% / トライアスロン 50.4% / 卓球 50.3%）。",
      "\n下位はボクシング・レスリング・馬術で、階級数や種目数の差が効いている。"
    )
  ) +
  theme_olympic()

save_fig(p8, "fig08_sport_gender_balance.png", height = 7)

# --- fig09: 種目構成の変化 ---------------------------------------------------
# 「女子種目が増えた」のか「男子種目が減った」のかを分ける。
events_by_gender <- athletes |>
  filter(!is.na(event_gender)) |>
  distinct(year, season, event, event_gender) |>
  count(year, season, event_gender, name = "n_events")

p9 <- ggplot(events_by_gender, aes(year, n_events, fill = event_gender)) +
  geom_area(colour = PAL$surface, linewidth = 0.3) +
  facet_wrap(~season, nrow = 1, scales = "free_x") +
  scale_fill_olympic(name = NULL) +
  scale_x_continuous(breaks = seq(1900, 2020, 40)) +
  labs(
    title = "女子種目の増設で総種目数が伸びた。男子種目はほぼ横ばい",
    subtitle = "実施種目数（種目名から性別区分を判定）。男子種目を削ったのではなく女子種目を足してきた",
    x = NULL, y = "種目数",
    caption = paste0(SOURCE_CAPTION,
                     "\n種目名に性別区分を含まない約 1 割の種目は除外。混合種目は 2010 年代以降に増加。")
  ) +
  theme_olympic()

save_fig(p9, "fig09_events_by_gender.png", width = 10)

message("03_sport_gender: 完了")
