# analysis/09_sports_history.R ------------------------------------------------
# 競技の栄枯盛衰。オリンピックで何が実施され、何が消えたか。
#
# 芸術競技（1912-1948）は絵画・彫刻・文学・音楽・建築でメダルが出ていた。
# 綱引きも 1900-1920 は正式競技だった。
# 一方でスケートボード・サーフィン・ブレイキンは 2020 年代に入ってきたばかり。

source("R/setup.R")
ensure_packages()
source("R/theme_olympic.R")
source("R/prepare_data.R")

message("09_sports_history: 実行中")

# 各シーズンの最終開催年。これ以前に終わっていれば「消えた競技」。
last_edition <- athletes |>
  group_by(season) |>
  summarise(last_year = max(year), .groups = "drop")

sport_years <- athletes |>
  distinct(sport, season, year) |>
  # 同じ競技名が夏冬にまたがることは無い前提だが、念のため主たるシーズンに寄せる
  add_count(sport, season, name = "n_years") |>
  group_by(sport) |>
  filter(season == season[which.max(n_years)]) |>
  ungroup()

sport_span <- sport_years |>
  group_by(sport, season) |>
  summarise(
    first_year = min(year),
    final_year = max(year),
    n_editions = n_distinct(year),
    .groups = "drop"
  ) |>
  left_join(last_edition, by = "season") |>
  mutate(
    status = case_when(
      final_year == last_year ~ "現在も実施",
      TRUE ~ "実施されなくなった"
    ),
    status = factor(status, levels = c("現在も実施", "実施されなくなった")),
    sport = fct_reorder(sport, first_year, .desc = TRUE)
  )

n_gone <- sum(sport_span$status == "実施されなくなった")
message("  競技数 ", nrow(sport_span), " / 消えた競技 ", n_gone)

# 消えた競技のうち、実施回数が多かったもの＝一時代を築いたもの
notable_gone <- sport_span |>
  filter(status == "実施されなくなった") |>
  slice_max(n_editions, n = 4)

p1 <- ggplot(sport_span, aes(y = sport, colour = status)) +
  geom_segment(aes(x = first_year, xend = final_year, yend = sport),
               linewidth = 1.5, alpha = 0.75) +
  geom_point(data = sport_years |>
               left_join(select(sport_span, sport, status), by = "sport") |>
               mutate(sport = factor(sport, levels = levels(sport_span$sport))),
             aes(x = year), size = 0.5, alpha = 0.5) +
  geom_text(
    data = notable_gone,
    aes(x = final_year, label = sprintf("  %d年まで・%d大会", final_year, n_editions)),
    hjust = 0, size = 2.7, show.legend = FALSE
  ) +
  facet_wrap(~season, scales = "free_y", ncol = 2) +
  scale_colour_manual(
    values = c("現在も実施" = PAL$cat[1], "実施されなくなった" = PAL$cat[2]),
    name = NULL
  ) +
  # 右端の注記（Baseball / Softball は 2020 年）が切れないよう右に大きく余白を取る
  scale_x_continuous(breaks = seq(1900, 2020, 40),
                     expand = expansion(mult = c(0.03, 0.40))) +
  labs(
    title = sprintf("%d 競技のうち %d 競技はもう実施されていない", nrow(sport_span), n_gone),
    subtitle = paste0(
      "各競技の初出場から最終出場まで。点は実際に実施された大会。\n",
      "夏季と冬季は種目の入れ替わり方がまったく違うので分けて描く"
    ),
    x = NULL, y = NULL,
    caption = paste0(
      SOURCE_CAPTION,
      "\n芸術競技（Art Competitions）は 1912-1948 に絵画・彫刻・文学・音楽・建築でメダルを出していた。",
      "\n線が途切れず見えても、戦争による中止（1916 / 1940 / 1944）で実際には間隔が空いている。"
    )
  ) +
  theme_olympic() +
  theme(
    axis.text.y = element_text(size = rel(0.62)),
    panel.spacing = unit(1.2, "lines")
  )

save_fig(p1, "sports_lifespan.png", width = 11, height = 8)

message("09_sports_history: 完了")
