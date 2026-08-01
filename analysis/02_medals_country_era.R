# analysis/02_medals_country_era.R --------------------------------------------
# 国とメダル: 通算の強豪、時代ごとの勢力図、メダルの色構成。
#
# 国別メダル数は公式集計（medal_table）だけを使う。
# 選手データ由来のメダルは 2018 年以降が不完全なため（fig03 参照）。

source("R/setup.R")
ensure_packages()
source("R/theme_olympic.R")
source("R/prepare_data.R")

message("02_medals_country_era: 実行中")

# 分裂・統合した国は系列が途切れるので、継承関係でまとめた「主体」を作る。
medals_entity <- medals_official |>
  mutate(entity = coalesce(lineage_label, country))

medals_entity_long <- medals_official_long |>
  mutate(entity = coalesce(lineage_label, country))

# --- fig04: 通算メダル数の上位国 ---------------------------------------------
top_entities <- medals_entity |>
  group_by(entity) |>
  summarise(total = sum(total), .groups = "drop") |>
  slice_max(total, n = 15) |>
  pull(entity)

by_season <- medals_entity |>
  filter(entity %in% top_entities) |>
  group_by(entity, season) |>
  summarise(total = sum(total), .groups = "drop") |>
  mutate(entity = fct_reorder(entity, total, .fun = sum))

p4 <- ggplot(by_season, aes(total, entity, fill = season)) +
  # 塗り同士の間に地の色を 2px 挟む
  geom_col(colour = PAL$surface, linewidth = 0.7, width = 0.72) +
  scale_fill_olympic(name = NULL) +
  scale_x_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "通算メダル数の上位 15 主体",
    subtitle = "旧ソ連・東西ドイツ・チェコスロバキア・ユーゴは継承関係でまとめた（粗い集約）",
    x = "メダル数（公式集計）", y = NULL,
    caption = paste0(SOURCE_CAPTION,
                     "\n団体競技も 1 種目 1 メダルとして数える IOC 方式。AIN（中立選手）はどの主体にも含めていない。")
  ) +
  theme_olympic()

save_fig(p4, "fig04_top_entities.png", height = 6)

# --- fig05: 年代ごとの勢力図 -------------------------------------------------
# 「その年代に配られたメダルのうち何 % を取ったか」で見る。
# 種目数が時代とともに増えるので、実数のままでは比較できない。
decade_total <- medals_entity |>
  group_by(decade) |>
  summarise(all_medals = sum(total), .groups = "drop")

era_top <- medals_entity |>
  group_by(entity) |>
  summarise(total = sum(total), .groups = "drop") |>
  slice_max(total, n = 6) |>
  pull(entity)

era_share <- medals_entity |>
  filter(entity %in% era_top) |>
  group_by(decade, entity) |>
  summarise(medals = sum(total), .groups = "drop") |>
  left_join(decade_total, by = "decade") |>
  mutate(share = 100 * medals / all_medals) |>
  complete(decade = unique(decade_total$decade), entity = era_top,
           fill = list(share = 0))

# 6 系列を 1 枚に重ねると右端でラベルが潰れ、色だけが頼りになる。
# 主体ごとの小さな図に分け、他主体を薄いグレーで背後に置いて比較できるようにする。
era_backdrop <- era_share |>
  select(decade, backdrop_entity = entity, share) |>
  tidyr::expand_grid(entity = era_top) |>
  filter(backdrop_entity != entity)

p5 <- ggplot(era_share, aes(decade, share)) +
  geom_line(
    data = era_backdrop, aes(group = backdrop_entity),
    colour = PAL$grid, linewidth = 0.5
  ) +
  geom_line(aes(colour = entity), linewidth = 1) +
  facet_wrap(~entity, nrow = 2) +
  scale_colour_olympic(guide = "none") +
  scale_x_continuous(breaks = seq(1900, 2020, 40)) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "アメリカの一強は 1900 年代まで、以降は 1 割前後で安定",
    subtitle = paste(
      "各年代に配られた全メダルに占める割合。実数ではなくシェアで見る（種目数が時代とともに増えるため）。",
      "灰色は他の 5 主体", sep = "\n"
    ),
    x = NULL, y = "その年代のメダルに占める割合",
    caption = paste0(
      SOURCE_CAPTION,
      "\nソ連の 1950 年代以前と、ドイツの 1940 年代（1948 年大会から除外）の 0% は不参加によるもの。"
    )
  ) +
  theme_olympic() +
  theme(panel.spacing = unit(1.1, "lines"))

save_fig(p5, "fig05_era_share.png", width = 10, height = 6)

# --- fig06: メダルの色構成 ---------------------------------------------------
# 「金を取り切る国」と「表彰台に多く乗るが金は少ない国」を分ける。
composition <- medals_entity_long |>
  filter(entity %in% top_entities[1:12]) |>
  group_by(entity, medal) |>
  summarise(n = sum(n), .groups = "drop") |>
  group_by(entity) |>
  mutate(
    share = 100 * n / sum(n),
    total = sum(n),
    gold_share = share[medal == "Gold"]
  ) |>
  ungroup() |>
  mutate(entity = fct_reorder(entity, gold_share))

gold_gap <- with(filter(composition, medal == "Gold"), round(max(share) - min(share)))

p6 <- ggplot(composition, aes(share, entity, fill = medal)) +
  # reverse = TRUE で金を左端に積む。
  # 既定の積み順だと金が右端に来て、下の金の割合ラベル（x = share/2）が
  # 銅のセグメント上に載ってしまう。
  geom_col(colour = PAL$surface, linewidth = 0.7, width = 0.72,
           position = position_stack(reverse = TRUE)) +
  geom_text(
    data = filter(composition, medal == "Gold"),
    aes(x = share / 2, label = paste0(round(share), "%")),
    colour = "white", size = 3, fontface = "bold", show.legend = FALSE
  ) +
  scale_fill_medal(name = NULL, breaks = MEDAL_LEVELS) +
  scale_x_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0))) +
  labs(
    title = paste0("金メダル比率には ", gold_gap, " ポイントの開きがある"),
    subtitle = "通算メダル上位 12 主体の色構成。数字は金の割合。3 分の 1 が均衡点",
    x = "構成比", y = NULL,
    caption = paste0(SOURCE_CAPTION,
                     "\n柔道・ボクシング等は銅を 2 個授与するため、構造的に銅の比率が上がる点に注意。")
  ) +
  theme_olympic()

save_fig(p6, "fig06_medal_composition.png", height = 6)

message("02_medals_country_era: 完了")
