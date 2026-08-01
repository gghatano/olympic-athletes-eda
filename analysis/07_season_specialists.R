# analysis/07_season_specialists.R --------------------------------------------
# 「その国は夏に稼いでいるのか、冬に稼いでいるのか」
#
# 通算メダル数の順位表は夏季の物量に支配される。冬季メダルは全体の 1 割強しか
# ないので、冬季だけで強い国は総合順位からは見えない。
# 国ごとに「自国のメダルのうち冬季が占める割合」を出すと、これが一目で分かる。

source("R/setup.R")
ensure_packages()
source("R/theme_olympic.R")
source("R/prepare_data.R")

message("07_season_specialists: 実行中")

MIN_MEDALS <- 120L

by_country <- medals_official |>
  mutate(entity = coalesce(lineage_label, country)) |>
  group_by(entity, season) |>
  summarise(medals = sum(total), .groups = "drop") |>
  pivot_wider(names_from = season, values_from = medals, values_fill = 0) |>
  mutate(
    total = Summer + Winter,
    winter_share = 100 * Winter / total
  )

# 全体で冬季が占める割合。これが「普通の国」の基準線になる。
global_winter_share <- with(
  summarise(medals_official, w = sum(total[season == "Winter"]), a = sum(total)),
  100 * w / a
)
message("  全メダルに占める冬季の割合: ", sprintf("%.1f%%", global_winter_share))

plot_dat <- by_country |>
  filter(total >= MIN_MEDALS) |>
  mutate(
    entity = fct_reorder(entity, winter_share),
    side = if_else(winter_share >= global_winter_share, "冬季に偏る", "夏季に偏る")
  )

message("  対象国: ", nrow(plot_dat), " (通算 ", MIN_MEDALS, " 個以上)")
extremes <- plot_dat |> arrange(desc(winter_share))
message("  最も冬季寄り: ", extremes$entity[1], " ",
        sprintf("%.0f%%", extremes$winter_share[1]))

p1 <- ggplot(plot_dat, aes(winter_share, entity, colour = side)) +
  geom_vline(xintercept = global_winter_share, colour = PAL$axis,
             linetype = "dashed", linewidth = 0.5) +
  # 破線の説明は annotate ではなくサブタイトルに置く。
  # 離散軸の y に annotate で値を渡すと、数値なら「連続量」と誤判定されて落ち、
  # 水準名（文字列）なら因子の並び順が失われてアルファベット順に戻ってしまう。
  geom_segment(aes(x = global_winter_share, xend = winter_share, yend = entity),
               colour = PAL$grid, linewidth = 1.1) +
  geom_point(size = 2.6) +
  geom_text(
    data = filter(plot_dat, winter_share > 55 | winter_share < 0.5),
    aes(label = sprintf("%.0f%% (冬%d / 夏%d)", winter_share, Winter, Summer)),
    hjust = -0.15, size = 2.7, colour = PAL$ink_2, show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c("冬季に偏る" = PAL$cat[2], "夏季に偏る" = PAL$cat[1]),
    name = NULL, breaks = c("冬季に偏る", "夏季に偏る")
  ) +
  scale_x_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0.02, 0.22))) +
  labs(
    title = "冬季に全てを賭けている国と、冬季が存在しない国",
    subtitle = sprintf(
      "その国の通算メダルのうち冬季が占める割合。通算 %d 個以上の %d 主体。\n破線は全メダルに占める冬季の割合 %.0f%%（この線より右なら冬季寄り）",
      MIN_MEDALS, nrow(plot_dat), global_winter_share
    ),
    x = "自国メダルに占める冬季の割合", y = NULL,
    caption = paste0(
      SOURCE_CAPTION,
      "\n継承関係でまとめた主体を含む（ソ連/ロシア・ドイツ系など）。",
      "\n冬季偏重は気候と地形の制約をほぼそのまま映す。冬季五輪は 1924 年開始で、夏季より 28 年短い点にも注意。"
    )
  ) +
  theme_olympic()

save_fig(p1, "medals_season_specialists.png", height = 7)

message("07_season_specialists: 完了")
