# analysis/06_host_privilege.R ------------------------------------------------
# 開催国はメダルを増やす。これ自体は驚きではない（開催国には出場枠が
# 自動的に与えられ、実施種目の選定にも関与できる）。
# 面白いのは「その特権をどれだけ使い切ったか」の差のほうなので、
# 直近 30 年に絞って順位をつける。
#
# 比較は必ず同じシーズンの中で行う。冬季は参加国も種目数も夏季と桁が違うため、
# 夏冬をまたいで基準線を引くと意味がなくなる。

source("R/setup.R")
ensure_packages()
source("R/theme_olympic.R")
source("R/prepare_data.R")

message("06_host_privilege: 実行中")

RECENT_FROM <- 1996L  # 直近 30 年

# 大会ごとの総メダル数（シェアの分母）
edition_totals <- medals_official |>
  group_by(games, year, season) |>
  summarise(all_medals = sum(total), .groups = "drop")

shares <- medals_official |>
  left_join(edition_totals, by = c("games", "year", "season")) |>
  mutate(share = 100 * total / all_medals) |>
  select(noc, year, season, share, total)

# 自国開催時のシェアを、同じシーズンの前後 2 大会（自国開催を除く）と比べる
host_effect <- host_noc |>
  filter(!is.na(host_noc)) |>
  rowwise() |>
  mutate(
    home = {
      v <- shares$share[shares$noc == host_noc & shares$year == year &
                          shares$season == season]
      if (length(v) == 0) NA_real_ else v[1]
    },
    home_medals = {
      v <- shares$total[shares$noc == host_noc & shares$year == year &
                          shares$season == season]
      if (length(v) == 0) NA_integer_ else v[1]
    },
    baseline = {
      v <- shares$share[shares$noc == host_noc & shares$season == season &
                          shares$year != year & abs(shares$year - year) <= 9]
      if (length(v) == 0) NA_real_ else mean(v)
    }
  ) |>
  ungroup() |>
  filter(!is.na(home), !is.na(baseline)) |>
  mutate(
    lift = home - baseline,
    ratio = home / baseline,
    label = paste0(host_country, " ", year)
  )

recent <- host_effect |>
  filter(year >= RECENT_FROM) |>
  arrange(desc(lift)) |>
  mutate(rank = row_number())

top5 <- head(recent, 5)
message("  直近30年の開催: ", nrow(recent), " 大会")
message("  ベスト5: ", paste(top5$label, collapse = " / "))

# --- 図1: 直近 30 年の開催国、特権の使いっぷり -------------------------------
# 基準線 -> 自国開催時のシェアを線で結ぶ（ダンベル）。
plot_recent <- recent |>
  mutate(
    label = fct_reorder(label, lift),
    highlight = rank <= 5
  )

p1 <- ggplot(plot_recent, aes(y = label)) +
  geom_segment(aes(x = baseline, xend = home, colour = season),
               linewidth = 1.4, alpha = 0.35,
               arrow = arrow(length = unit(0.14, "cm"), type = "closed")) +
  geom_point(aes(x = baseline), colour = PAL$muted, size = 2) +
  geom_point(aes(x = home, colour = season), size = 3) +
  geom_text(
    data = filter(plot_recent, highlight),
    aes(x = home, label = sprintf("  %+.1fpt (%.1f倍・%d個)", lift, ratio, home_medals)),
    hjust = 0, size = 2.9, colour = PAL$ink
  ) +
  scale_colour_olympic(name = NULL) +
  scale_x_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0.04, 0.34))) +
  labs(
    title = sprintf("開催国特権を最も使い切ったのは %s", top5$label[1]),
    subtitle = paste0(
      "灰色の点=前後 2 大会（同シーズン）の平均メダルシェア、色付きの点=自国開催時。\n",
      RECENT_FROM, " 年以降の ", nrow(recent), " 大会。上位 5 大会に伸び幅・倍率・実際のメダル数を表示"
    ),
    x = "その大会の全メダルに占める割合", y = NULL,
    caption = paste0(
      SOURCE_CAPTION,
      "\n冬季は参加国も種目数も夏季より少ないため、同じ +1pt でも意味が違う。基準線は必ず同じシーズンで取っている。",
      "\n因果ではない。開催国は出場枠が優遇され、実施種目の選定にも関与できる。強化投資の効果とは分離していない。"
    )
  ) +
  theme_olympic()

save_fig(p1, "host_top5_recent.png", width = 10, height = 6.5)

# --- 図2: 全期間の文脈 -------------------------------------------------------
# 初期大会は参加国が少なく開催国比率が構造的に高い。直近だけ見ると見落とす。
share_positive <- round(100 * mean(host_effect$lift > 0))

p2 <- ggplot(host_effect, aes(year, lift)) +
  geom_hline(yintercept = 0, colour = PAL$axis, linewidth = 0.5) +
  annotate("rect", xmin = RECENT_FROM - 2, xmax = max(host_effect$year) + 2,
           ymin = -Inf, ymax = Inf, fill = PAL$cat[1], alpha = 0.05) +
  annotate("text", x = RECENT_FROM + 14, y = 60, label = "上の図の範囲",
           colour = PAL$ink_2, size = 3) +
  geom_segment(aes(xend = year, yend = 0), colour = PAL$grid, linewidth = 0.6) +
  geom_point(aes(colour = season), size = 2.4, alpha = 0.9) +
  geom_text(
    data = slice_max(host_effect, lift, n = 3),
    aes(label = label), colour = PAL$ink, size = 2.9, vjust = -1
  ) +
  scale_colour_olympic(name = NULL) +
  scale_x_continuous(breaks = seq(1900, 2020, 20), expand = expansion(mult = 0.06)) +
  scale_y_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "pt")) +
  labs(
    title = sprintf("開催国は %d 大会中 %d%% でシェアを伸ばしたが、伸び幅は年々小さい",
                    nrow(host_effect), share_positive),
    subtitle = "自国開催時のメダルシェア − 前後 2 大会（同シーズン）の平均シェア",
    x = NULL, y = "シェアの差（ポイント）",
    caption = paste0(
      SOURCE_CAPTION,
      "\n初期大会は参加国が少なく開催国の比率が構造的に高い。1904 セントルイスの +72pt は",
      "参加国が実質 3 か国だったことによる。"
    )
  ) +
  theme_olympic()

save_fig(p2, "host_timeline.png", width = 9.5, height = 5.5)

message("06_host_privilege: 完了")
