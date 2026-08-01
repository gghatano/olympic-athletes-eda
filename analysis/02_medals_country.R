# analysis/02_medals_country.R ------------------------------------------------
# 国とメダル。
#
# 夏季と冬季は必ず分けて描く。参加国数も種目数も桁が違うため、
# 合算すると「冬季に強い国」が夏季の物量に埋もれ、
# 年代ごとのシェアも分母が混ざって意味を失う。
#
# 国別メダル数は公式集計（medal_table）だけを使う。
# 選手データ由来のメダルは 2018 年以降が不完全なため。

source("R/setup.R")
ensure_packages()
source("R/theme_olympic.R")
source("R/prepare_data.R")

message("02_medals_country: 実行中")

# 分裂・統合した国は系列が途切れるので、継承関係でまとめた「主体」を作る
medals_entity <- medals_official |>
  mutate(entity = coalesce(lineage_label, country))

medals_entity_long <- medals_official_long |>
  mutate(entity = coalesce(lineage_label, country))

# --- 図1: シーズンごとの通算メダル上位 ---------------------------------------
# 合算して 1 本の棒にすると、冬季だけの強豪（ノルウェー・オーストリア）が
# 夏季の物量に埋もれる。シーズンごとに別々の順位表として描く。
top_by_season <- medals_entity |>
  group_by(season, entity) |>
  summarise(total = sum(total), .groups = "drop") |>
  group_by(season) |>
  slice_max(total, n = 12) |>
  ungroup() |>
  # facet ごとに独立して並べ替えるため、シーズン名を接頭辞に付けた行 ID を作る
  # （表示時にラベルから接頭辞を落とす）
  mutate(row = fct_reorder(paste(season, entity), total))

p1 <- ggplot(top_by_season, aes(total, row, fill = season)) +
  geom_col(width = 0.72) +
  facet_wrap(~season, scales = "free", nrow = 1) +
  scale_y_discrete(labels = function(x) sub("^(Summer|Winter) ", "", x)) +
  scale_fill_olympic(guide = "none") +
  scale_x_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "夏季と冬季ではメダル上位の顔ぶれが入れ替わる",
    subtitle = "通算メダル数の上位 12 主体。旧ソ連・東西ドイツ・チェコスロバキア・ユーゴは継承関係でまとめた",
    x = "メダル数（公式集計）", y = NULL,
    caption = paste0(
      SOURCE_CAPTION,
      "\n団体競技も 1 種目 1 メダルとして数える IOC 方式。AIN（中立選手）はどの主体にも含めていない。"
    )
  ) +
  theme_olympic() +
  theme(panel.spacing = unit(1.4, "lines"))

save_fig(p1, "medals_top_countries.png", width = 10, height = 5.5)

# --- 図2: 年代ごとの勢力図（シーズン別） -------------------------------------
# 実数ではなくシェアで見る。種目数が時代とともに増えるため。
# 分母は「その年代・そのシーズンの全メダル」。
decade_total <- medals_entity |>
  group_by(season, decade) |>
  summarise(all_medals = sum(total), .groups = "drop")

era_top <- medals_entity |>
  group_by(entity) |>
  summarise(total = sum(total), .groups = "drop") |>
  slice_max(total, n = 6) |>
  pull(entity)

era_share <- medals_entity |>
  filter(entity %in% era_top) |>
  group_by(season, decade, entity) |>
  summarise(medals = sum(total), .groups = "drop") |>
  right_join(
    tidyr::expand_grid(
      distinct(decade_total, season, decade),
      entity = era_top
    ),
    by = c("season", "decade", "entity")
  ) |>
  left_join(decade_total, by = c("season", "decade")) |>
  mutate(share = 100 * coalesce(medals, 0) / all_medals)

p2 <- ggplot(era_share, aes(decade, share, colour = season)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~entity, nrow = 2) +
  scale_colour_olympic(name = NULL) +
  scale_x_continuous(breaks = seq(1900, 2020, 40)) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "アメリカの一強は 1900 年代まで。冬季ではまったく別の物語が動く",
    subtitle = paste(
      "各年代・各シーズンに配られた全メダルに占める割合。実数ではなくシェアで見る（種目数が時代とともに増えるため）",
      sep = "\n"
    ),
    x = NULL, y = "その年代・シーズンのメダルに占める割合",
    caption = paste0(
      SOURCE_CAPTION,
      "\nソ連の 1950 年代以前と、ドイツの 1940 年代（1948 年大会から除外）の 0% は不参加によるもの。",
      "\n重要: Germany (all) の 1970-80 年代の山は西ドイツと東ドイツを足したもので、単一国の成績ではない",
      "\n（1968-1988 の 10 大会で両者は別々に出場している）。同様に 1996 年以降はチェコとスロバキアの合計。"
    )
  ) +
  theme_olympic() +
  theme(panel.spacing = unit(1.1, "lines"))

save_fig(p2, "medals_era_share.png", width = 10, height = 6)

# --- 図3: メダルの色構成 -----------------------------------------------------
# 「金を取り切る国」と「表彰台には乗るが金は少ない国」を分ける。
top10_per_season <- medals_entity_long |>
  group_by(season, entity) |>
  summarise(entity_total = sum(n), .groups = "drop") |>
  group_by(season) |>
  slice_max(entity_total, n = 10) |>
  ungroup() |>
  select(season, entity)

composition <- medals_entity_long |>
  semi_join(top10_per_season, by = c("season", "entity")) |>
  group_by(season, entity, medal) |>
  summarise(n = sum(n), .groups = "drop") |>
  group_by(season, entity) |>
  mutate(share = 100 * n / sum(n),
         gold_share = share[medal == "Gold"]) |>
  ungroup() |>
  mutate(row = fct_reorder(paste(season, entity), gold_share))

p3 <- ggplot(composition, aes(share, row, fill = medal)) +
  geom_col(colour = PAL$surface, linewidth = 0.7, width = 0.72,
           position = position_stack(reverse = TRUE)) +
  geom_text(
    data = filter(composition, medal == "Gold"),
    aes(x = share / 2, label = paste0(round(share), "%")),
    colour = "white", size = 2.8, fontface = "bold", show.legend = FALSE
  ) +
  facet_wrap(~season, scales = "free_y", nrow = 1) +
  scale_y_discrete(labels = function(x) sub("^(Summer|Winter) ", "", x)) +
  scale_fill_medal(name = NULL, breaks = MEDAL_LEVELS) +
  scale_x_continuous(labels = label_percent(scale = 1), expand = expansion(mult = 0)) +
  labs(
    title = "金メダル比率は国によって 10 ポイント以上違う",
    subtitle = "各シーズンの通算メダル上位 10 主体の色構成。数字は金の割合。3 分の 1 が均衡点",
    x = "構成比", y = NULL,
    caption = paste0(
      SOURCE_CAPTION,
      "\n柔道・ボクシング等は銅を 2 個授与するため、構造的に銅の比率が上がる点に注意。"
    )
  ) +
  theme_olympic() +
  theme(panel.spacing = unit(1.4, "lines"))

save_fig(p3, "medals_composition.png", width = 10, height = 5.5)

# --- 図4: メダルを取った国の広がり -------------------------------------------
spread <- medals_official |>
  filter(total > 0) |>
  group_by(season, year) |>
  summarise(n_nocs = n_distinct(noc), .groups = "drop")

p4 <- ggplot(spread, aes(year, n_nocs, colour = season)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  scale_colour_olympic(name = NULL) +
  scale_x_continuous(breaks = seq(1900, 2020, 20)) +
  labs(
    title = "表彰台に乗る国は増え続けている。ただし冬季は 40 か国前後で頭打ち",
    subtitle = "1 大会でメダルを 1 つ以上獲得した国・地域（NOC）の数",
    x = NULL, y = "メダルを獲得した NOC 数",
    caption = paste0(
      SOURCE_CAPTION,
      "\n冬季は雪と氷の設備が要るため参加国自体が限られる。夏冬を合算するとこの違いが消える。"
    )
  ) +
  theme_olympic()

save_fig(p4, "medals_country_spread.png")

message("02_medals_country: 完了")
