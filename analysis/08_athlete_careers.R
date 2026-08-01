# analysis/08_athlete_careers.R -----------------------------------------------
# 選手個人の時間軸。年齢と、五輪に出続けられる年数。
#
# 年齢は 1960 年以降ほぼ欠損なし。それ以前は初期大会の高齢層に引っ張られるので
# 1960 年以降に限定する。出場歴（id）は全期間で欠損がないため制限しない。

source("R/setup.R")
ensure_packages()
source("R/theme_olympic.R")
source("R/prepare_data.R")

message("08_athlete_careers: 実行中")

# --- 図1: 競技ごとの年齢分布 -------------------------------------------------
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

p1 <- ggplot(extremes, aes(median, sport, colour = grp)) +
  geom_segment(aes(x = q25, xend = q75, yend = sport), linewidth = 1.6, alpha = 0.35) +
  geom_point(size = 2.8) +
  geom_text(aes(label = round(median, 1)), colour = PAL$ink_2,
            size = 2.9, vjust = -1.2, show.legend = FALSE) +
  scale_colour_olympic(name = NULL) +
  scale_x_continuous(breaks = seq(14, 40, 2)) +
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

save_fig(p1, "athletes_age_by_sport.png", height = 7)

# --- 図2: 何回オリンピックに出られるか ---------------------------------------
careers <- athletes |>
  group_by(id, name) |>
  summarise(
    n_games = n_distinct(games),
    first_year = min(year),
    last_year = max(year),
    span = max(year) - min(year),
    main_sport = names(sort(table(sport), decreasing = TRUE))[1],
    .groups = "drop"
  )

message("  1 大会のみの選手: ",
        sprintf("%.0f%%", 100 * mean(careers$n_games == 1)),
        " / 5 大会以上: ", sum(careers$n_games >= 5))

# 競技ごとの「出場を重ねやすさ」= 2 大会以上出た選手の割合
repeatability <- athletes |>
  left_join(select(careers, id, n_games), by = "id") |>
  distinct(id, sport, n_games) |>
  group_by(sport) |>
  filter(n() >= 1000) |>
  summarise(
    n_athletes = n(),
    pct_repeat = 100 * mean(n_games >= 2),
    .groups = "drop"
  )

# 競技の主たるシーズン。上位が冬季競技で埋まるかどうかがこの図の見どころ。
sport_season <- athletes |>
  count(sport, season) |>
  group_by(sport) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(sport, season)

repeat_plot <- repeatability |>
  left_join(sport_season, by = "sport") |>
  mutate(sport = fct_reorder(sport, pct_repeat))

shown <- bind_rows(
  slice_max(repeat_plot, pct_repeat, n = 10),
  slice_min(repeat_plot, pct_repeat, n = 10)
)
best <- slice_max(shown, pct_repeat, n = 1, with_ties = FALSE)
worst <- slice_min(shown, pct_repeat, n = 1, with_ties = FALSE)

season_avg <- repeat_plot |>
  group_by(season) |>
  summarise(m = mean(pct_repeat), .groups = "drop")
message("  2 大会以上に出た選手の割合 平均: ",
        paste(sprintf("%s %.0f%%", season_avg$season, season_avg$m), collapse = " / "))

p2 <- ggplot(shown, aes(pct_repeat, sport, colour = season)) +
  geom_segment(aes(x = 0, xend = pct_repeat, yend = sport),
               colour = PAL$grid, linewidth = 1.1) +
  geom_point(size = 3.2) +
  geom_text(aes(label = sprintf("  %.0f%%", pct_repeat)),
            hjust = 0, size = 2.8, colour = PAL$ink_2, show.legend = FALSE) +
  scale_colour_olympic(name = NULL) +
  scale_x_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "五輪に 2 回以上出られるかは、競技よりシーズンで決まる",
    subtitle = sprintf(
      "2 大会以上に出場した選手の割合（上位 10 と下位 10）。%s %.0f%% に対し %s %.0f%%",
      best$sport, best$pct_repeat, worst$sport, worst$pct_repeat
    ),
    x = "2 大会以上に出場した選手の割合", y = NULL,
    caption = paste0(
      SOURCE_CAPTION,
      sprintf("\n全競技の平均は冬季 %.0f%% に対し夏季 %.0f%%。冬季は競技人口が少なく代表の座を維持しやすい。",
              season_avg$m[season_avg$season == "Winter"],
              season_avg$m[season_avg$season == "Summer"]),
      "\n選手は最も多く出場した競技に割り当てている。直近の大会の選手は「次の機会」がまだ残っているため構造的に低く出る。"
    )
  ) +
  theme_olympic()

save_fig(p2, "athletes_careers.png", height = 6.5)

message("08_athlete_careers: 完了")
