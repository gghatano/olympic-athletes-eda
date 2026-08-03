# analysis/11_body_size_vs_build.R -------------------------------------------
# 「体格が効く」の正体は身長（線形の大きさ）なのか、それとも身長とは別の
# 体格（肉付き）なのか。10 は身長だけを見た。ここで体重・BMI に広げて切り分ける。
#
# 素直に「身長差」と「体重差」を並べると誤る。身長と体重は選手レベルで
# 相関 0.8 と強く連動する（背が高ければ重い）。「体重も効く」の大半は
# 「身長が効く」の言い換えになってしまう。
#
# そこで体重ではなく **BMI = 体重 / 身長^2** を第二の軸にする。BMI は
# 身長で割ってあるので、身長と概ね独立（相関 0.34）に「同じ身長での肉付き」を測る。
#   - 身長差 > 0, BMI差 ≈ 0 : 相似に大きいだけ（＝実質は身長）
#   - BMI差 > 0            : 同じ身長でも、がっしりした体が有利
#   - BMI差 < 0            : 同じ身長でも、絞れた体が有利
#
# 有意性は 10 と同じ、(大会 × 種目 × 性別) を固定効果、選手 id をクラスタと
# した頑健標準誤差で判定する。1960-2016、身長体重が揃う行に限る。

source("R/setup.R")
ensure_packages()
source("R/theme_olympic.R")
source("R/prepare_data.R")

message("11_body_size_vs_build: 実行中")

BMI_RANGE <- c(13, 45)
HEIGHT_RANGE <- c(120, 230)
MIN_MED <- 3L; MIN_NON <- 5L; MIN_CELLS <- 30L

body <- athletes |>
  filter(
    year >= BODY_YEARS[1], year <= BODY_YEARS[2],
    !is.na(height), !is.na(weight),
    height >= HEIGHT_RANGE[1], height <= HEIGHT_RANGE[2],
    bmi >= BMI_RANGE[1], bmi <= BMI_RANGE[2]
  )

# --- セル = 大会 × 種目 × 性別。存在する競技だけ残す --------------------------
cell_ok <- body |>
  group_by(sport, event, games, year, season, sex) |>
  summarise(n_med = sum(is_medalist), n_non = sum(!is_medalist), .groups = "drop") |>
  filter(n_med >= MIN_MED, n_non >= MIN_NON)

sport_cells <- cell_ok |>
  count(sport, name = "n_cells") |>
  filter(n_cells >= MIN_CELLS)

rows <- body |>
  semi_join(cell_ok, by = c("sport", "event", "games", "year", "season", "sex")) |>
  semi_join(sport_cells, by = "sport") |>
  mutate(cell = paste(event, games, sex), m = as.numeric(is_medalist))

message("  対象: ", format(nrow(rows), big.mark = ","), " 行 / ",
        n_distinct(rows$sport), " 競技")

# --- セル固定効果 + 選手クラスタ頑健 SE（10 と同じ推定量、指標を差し替え可能） ---
fe_cluster <- function(x, col) {
  y <- x[[col]]
  cy <- ave(y, x$cell); cm <- ave(x$m, x$cell)
  hy <- y - cy; mx <- x$m - cm
  denom <- sum(mx^2)
  if (denom <= 0) return(c(est = NA_real_, se = NA_real_))
  beta <- sum(hy * mx) / denom
  resid <- hy - beta * mx
  meat <- 0
  for (ix in split(seq_len(nrow(x)), x$id)) meat <- meat + sum(mx[ix] * resid[ix])^2
  G <- n_distinct(x$id); n <- nrow(x); k <- 1 + n_distinct(x$cell)
  adj <- (G / (G - 1)) * ((n - 1) / max(n - k, 1))
  c(est = beta, se = sqrt(adj * meat / denom^2))
}

sig <- function(e, s) (e - 1.96 * s > 0) | (e + 1.96 * s < 0)

sport_stats <- rows |>
  group_by(sport) |>
  group_modify(~ {
    h <- fe_cluster(.x, "height"); b <- fe_cluster(.x, "bmi")
    tibble(h_est = h["est"], h_se = h["se"], b_est = b["est"], b_se = b["se"])
  }) |>
  ungroup() |>
  mutate(
    h_sig = sig(h_est, h_se),
    b_sig = sig(b_est, b_se),
    build = case_when(
      b_sig & b_est > 0 ~ "同じ身長なら、がっしり型が有利",
      b_sig & b_est < 0 ~ "同じ身長なら、絞れた型が有利",
      TRUE              ~ "身長で説明できる（BMI差は非有意）"
    ),
    build = factor(build, levels = c("同じ身長なら、がっしり型が有利",
                                     "同じ身長なら、絞れた型が有利",
                                     "身長で説明できる（BMI差は非有意）"))
  )

n_h  <- sum(sport_stats$h_sig, na.rm = TRUE)
n_bp <- sum(sport_stats$b_sig & sport_stats$b_est > 0, na.rm = TRUE)
n_bn <- sum(sport_stats$b_sig & sport_stats$b_est < 0, na.rm = TRUE)
message("  身長が有意 ", n_h, " 競技 / BMI が有意 ", n_bp + n_bn,
        "（がっしり ", n_bp, " ・絞り ", n_bn, "）")

# --- 図: 身長（線形サイズ） × BMI（同身長での体格） --------------------------
# ラベルは外側・特徴的な競技だけに絞る（中央は密集するため）
label_sports <- c("Swimming", "Weightlifting", "Bobsleigh", "Alpine Skiing",
                  "Diving", "Ski Jumping", "Gymnastics", "Rowing", "Athletics",
                  "Taekwondo", "Judo", "Volleyball", "Table Tennis", "Canoeing")
plot_df <- sport_stats |> mutate(lab = if_else(sport %in% label_sports, sport, NA_character_))

p <- ggplot(plot_df, aes(h_est, b_est, colour = build)) +
  geom_hline(yintercept = 0, colour = PAL$grid, linewidth = 0.5) +
  geom_vline(xintercept = 0, colour = PAL$grid, linewidth = 0.5) +
  geom_point(size = 2.8, alpha = 0.85) +
  geom_text(aes(label = lab), colour = PAL$ink, size = 2.9, vjust = -1,
            na.rm = TRUE, show.legend = FALSE) +
  scale_colour_manual(
    values = c("同じ身長なら、がっしり型が有利" = PAL$cat[2],
               "同じ身長なら、絞れた型が有利"   = PAL$cat[1],
               "身長で説明できる（BMI差は非有意）" = PAL$muted),
    name = NULL
  ) +
  scale_x_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "cm"),
                     expand = expansion(mult = 0.12)) +
  scale_y_continuous(expand = expansion(mult = 0.14)) +
  labs(
    title = sprintf("「体格が効く」の大半は身長。%d 競技だけ身長とは別に体格が効く", n_bp + n_bn),
    subtitle = paste0(
      "横=メダリストの身長差（線形の大きさ）、縦=BMI差（身長で割った肉付き）。",
      "身長 × 種目 × 性別を固定効果、選手 id クラスタで推定。\n",
      "多くの競技は横軸沿い＝背が高くそれに比例して重いだけ。縦に離れた競技だけ、",
      "同じ身長でも体格そのものが効いている。", BODY_YEARS[1], "-", BODY_YEARS[2]
    ),
    x = "身長差（線形の大きさ）", y = "BMI差（同じ身長での肉付き）",
    caption = paste0(
      SOURCE_CAPTION,
      "\n体重ではなく BMI を縦軸に取るのは、身長と体重が相関 0.8 と強く連動し、",
      "「体重差」の大半が「身長差」の言い換えになるため（BMI は身長と概ね独立）。",
      "\n重量挙げは身長差が負・BMI差が正＝「背は低くてよいが、身長のわりに重い体」が効く純粋な体格ケース。"
    )
  ) +
  theme_olympic()

save_fig(p, "body_size_vs_build.png", width = 10, height = 6.8)

message("11_body_size_vs_build: 完了")
