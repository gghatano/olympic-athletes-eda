# analysis/04_body_metrics.R --------------------------------------------------
# 身体データ: 競技ごとの体格、性別差、時代変化。
#
# 期間は 1960-2016 に固定する（BODY_YEARS）。
# それ以前は 7-9 割、2018 年以降も 4-7 割が欠損するため、
# 全期間で見ると「記録が残った選手」の偏りを体格差と取り違える。

source("R/setup.R")
ensure_packages()
source("R/theme_olympic.R")
source("R/prepare_data.R")

message("04_body_metrics: 実行中")

# BMI には身長体重の記録ミス由来と思われる外れ値がある（最小 8.4 / 最大 63.9）。
# 生理的にありえない範囲を落とす。件数は全体の 0.1% 未満。
BMI_RANGE <- c(13, 45)

body <- athletes |>
  filter(
    year >= BODY_YEARS[1], year <= BODY_YEARS[2],
    !is.na(height), !is.na(weight)
  ) |>
  filter(is.na(bmi) | (bmi >= BMI_RANGE[1] & bmi <= BMI_RANGE[2]))

message("  身体データ対象: ", format(nrow(body), big.mark = ","), " 行 (",
        BODY_YEARS[1], "-", BODY_YEARS[2], ")")

# --- fig10: 競技ごとの体格マップ ---------------------------------------------
# 散布図は全ての点が互いに比較されるため、色でカテゴリを分けない。
# 位置とラベルで読ませる。
sport_body <- body |>
  group_by(sport) |>
  summarise(
    n = n(),
    height = median(height),
    weight = median(weight),
    .groups = "drop"
  ) |>
  filter(n >= 500)

# ラベルが密集する中央部を避け、外側の競技だけ名前を出す
label_sports <- sport_body |>
  mutate(
    d = scale(height)[, 1]^2 + scale(weight)[, 1]^2
  ) |>
  slice_max(d, n = 14)

p10 <- ggplot(sport_body, aes(height, weight)) +
  geom_point(aes(size = n), colour = PAL$cat[1], alpha = 0.55) +
  geom_text(
    data = label_sports, aes(label = sport),
    colour = PAL$ink, size = 3, vjust = -1.1
  ) +
  scale_size_area(max_size = 9, labels = label_comma(), name = "選手 × 種目行数") +
  # 右端（Basketball）と上端のラベルが切れないように余白を広げる
  scale_x_continuous(expand = expansion(mult = 0.09)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  labs(
    title = "競技は「背の高さ」と「体重」の平面上にきれいに並ぶ",
    subtitle = paste0("競技ごとの身長・体重の中央値（", BODY_YEARS[1], "-", BODY_YEARS[2],
                      "、500 行以上の競技）。外側 14 競技に競技名を表示"),
    x = "身長の中央値 (cm)", y = "体重の中央値 (kg)",
    caption = paste0(SOURCE_CAPTION,
                     "\n男女を合算しているため、女子種目の比率が高い競技は左下に寄る。")
  ) +
  theme_olympic()

save_fig(p10, "fig10_sport_body_map.png", width = 10, height = 6.5)

# --- fig11: 体格の広がりが大きい競技 -----------------------------------------
# 中央値だけでは「全員が大きい競技」と「大小が混在する競技」を区別できない。
# 階級制競技（重量挙げ・柔道・レスリング）は後者になるはず。
top_spread <- body |>
  group_by(sport) |>
  summarise(n = n(), iqr_w = IQR(weight), .groups = "drop") |>
  filter(n >= 1500) |>
  # 体重は整数なので IQR が同値になる競技があり、slice_max は同値を全て返す。
  # 恣意的に切らず、実際の件数は下のサブタイトルに反映させる。
  slice_max(iqr_w, n = 12) |>
  pull(sport)

p11 <- body |>
  filter(sport %in% top_spread) |>
  mutate(sport = fct_reorder(sport, weight, .fun = IQR)) |>
  ggplot(aes(weight, sport, fill = sex)) +
  geom_boxplot(outlier.size = 0.3, outlier.alpha = 0.15,
               linewidth = 0.35, colour = PAL$ink_2) +
  scale_fill_olympic(name = NULL) +
  # scale_*_continuous(limits=) は箱ひげの統計量を計算する前に行を落とすため、
  # 表示範囲の調整には coord_cartesian を使う（箱の値が変わらない）。
  coord_cartesian(xlim = c(30, 160)) +
  labs(
    title = "階級制の競技は体重の幅が突出して広い",
    subtitle = paste0("体重の四分位範囲が大きい上位 ", length(top_spread), " 競技（",
                      BODY_YEARS[1], "-", BODY_YEARS[2], "、1,500 行以上。同値は全て表示）"),
    x = "体重 (kg)", y = NULL,
    caption = paste0(SOURCE_CAPTION,
                     "\n重量挙げ・柔道・レスリング・ボクシングは階級制。バスケットボールはポジション差による広がり。")
  ) +
  theme_olympic()

save_fig(p11, "fig11_weight_spread.png", height = 6.5)

# --- fig12: 体格の時代変化 ---------------------------------------------------
# 夏冬を 1 本にまとめてはいけない。1994 年以降は夏と冬が別の年に開催されるため、
# 合算すると「夏の年」と「冬の年」を交互にプロットすることになり、
# 競技構成の違いがギザギザとして現れる（体格の変化ではない）。
# さらに 1960 年は夏冬の両方があり 2016 年は夏だけなので、端点の構成も揃わない。
height_trend <- body |>
  group_by(season, year, sex) |>
  summarise(
    median_height = median(height),
    q25 = quantile(height, 0.25),
    q75 = quantile(height, 0.75),
    .groups = "drop"
  )

# 見出しの数値は目分量で書かず、各シーズンの最初と最後の大会の差から出す。
# 参考までに線形近似の傾きも添える（端点は年ごとの揺れを拾うため）。
gains <- height_trend |>
  group_by(season, sex) |>
  summarise(
    from = min(year), to = max(year),
    gain = median_height[which.max(year)] - median_height[which.min(year)],
    slope_span = unname(coef(lm(median_height ~ year))[2]) * (max(year) - min(year)),
    .groups = "drop"
  )
summer <- filter(gains, season == "Summer")

fmt_gain <- function(d) {
  paste(sprintf("%s %+.0f cm", ifelse(d$sex == "Women", "女性", "男性"), d$gain),
        collapse = " / ")
}

p12 <- ggplot(height_trend, aes(year, median_height, colour = sex, fill = sex)) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~season, nrow = 1) +
  scale_colour_olympic(name = NULL) +
  scale_fill_olympic(guide = "none") +
  scale_x_continuous(breaks = seq(1960, 2016, 16)) +
  labs(
    title = sprintf("夏季の選手の身長は %d 年間で %s 伸びた",
                    summer$to[1] - summer$from[1], fmt_gain(summer)),
    subtitle = paste0(
      "身長の中央値（帯は四分位範囲）。", BODY_YEARS[1], "-", BODY_YEARS[2], "。\n",
      "夏季と冬季は競技構成が違うため分けて描く（合算すると開催年が交互に来るだけでギザギザになる）"
    ),
    x = NULL, y = "身長 (cm)",
    caption = paste0(
      SOURCE_CAPTION,
      sprintf("\n線形近似では夏季 女性 %+.1f cm・男性 %+.1f cm。",
              summer$slope_span[summer$sex == "Women"],
              summer$slope_span[summer$sex == "Men"]),
      "\n一般人口の伸長と、種目構成の変化（背の高い競技の種目増）の両方が混ざる。切り分けは未実施。"
    )
  ) +
  theme_olympic() +
  theme(panel.spacing = unit(1.1, "lines"))

save_fig(p12, "fig12_height_trend.png", width = 10)

message("04_body_metrics: 完了")
