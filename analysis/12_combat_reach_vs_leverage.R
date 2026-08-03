# analysis/12_combat_reach_vs_leverage.R -------------------------------------
# 階級制の格闘技だけを取り出して、11 の身長 × BMI 平面を拡大する。
#
# 階級で総体重が揃うと、残る自由度は「その体重をどう配るか＝身長」になる。
# 打撃系（蹴る・突く）はリーチが効くので同じ体重なら長く・細く配るのが有利、
# 組技系（組む・投げる）は重心の低さと密度が効くので低く・密に配るのが有利——
# という仮説を、メダリストの大会内身長差・BMI差の向きで確かめる。
#
# 推定量は 11 と同じ (大会 × 種目 × 性別) 固定効果 + 選手 id クラスタ頑健 SE。
# 参考として、階級制だが格闘技ではない重量挙げ（純粋な「密度」側）も併置する。

source("R/setup.R")
ensure_packages()
source("R/theme_olympic.R")
source("R/prepare_data.R")

message("12_combat_reach_vs_leverage: 実行中")

BMI_RANGE <- c(13, 45); HEIGHT_RANGE <- c(120, 230)
MIN_MED <- 3L; MIN_NON <- 5L

groups <- tibble::tribble(
  ~sport,          ~grp,
  "Boxing",        "打撃系（突く・蹴る）",
  "Taekwondo",     "打撃系（突く・蹴る）",
  "Judo",          "組技系（組む・投げる）",
  "Wrestling",     "組技系（組む・投げる）",
  "Weightlifting", "参考：挙上（階級制・非格闘）"
)

body <- athletes |>
  filter(
    year >= BODY_YEARS[1], year <= BODY_YEARS[2],
    !is.na(height), !is.na(weight),
    height >= HEIGHT_RANGE[1], height <= HEIGHT_RANGE[2],
    bmi >= BMI_RANGE[1], bmi <= BMI_RANGE[2],
    sport %in% groups$sport
  )

cell_ok <- body |>
  group_by(sport, event, games, year, season, sex) |>
  summarise(n_med = sum(is_medalist), n_non = sum(!is_medalist), .groups = "drop") |>
  filter(n_med >= MIN_MED, n_non >= MIN_NON)

rows <- body |>
  semi_join(cell_ok, by = c("sport", "event", "games", "year", "season", "sex")) |>
  mutate(cell = paste(event, games, sex), m = as.numeric(is_medalist))

# 11 と同じ推定量（指標を差し替え可能に）
fe_cluster <- function(x, col) {
  y <- x[[col]]
  hy <- y - ave(y, x$cell); mx <- x$m - ave(x$m, x$cell)
  denom <- sum(mx^2)
  if (denom <= 0) return(c(est = NA_real_, se = NA_real_))
  beta <- sum(hy * mx) / denom; resid <- hy - beta * mx
  meat <- 0
  for (ix in split(seq_len(nrow(x)), x$id)) meat <- meat + sum(mx[ix] * resid[ix])^2
  G <- n_distinct(x$id); n <- nrow(x); k <- 1 + n_distinct(x$cell)
  adj <- (G / (G - 1)) * ((n - 1) / max(n - k, 1))
  c(est = beta, se = sqrt(adj * meat / denom^2))
}

est <- rows |>
  group_by(sport) |>
  group_modify(~ {
    h <- fe_cluster(.x, "height"); b <- fe_cluster(.x, "bmi")
    tibble(n_cells = n_distinct(.x$cell),
           h = h["est"], h_lo = h["est"] - 1.96 * h["se"], h_hi = h["est"] + 1.96 * h["se"],
           b = b["est"], b_lo = b["est"] - 1.96 * b["se"], b_hi = b["est"] + 1.96 * b["se"])
  }) |>
  ungroup() |>
  left_join(groups, by = "sport") |>
  mutate(grp = factor(grp, levels = c("打撃系（突く・蹴る）", "組技系（組む・投げる）",
                                      "参考：挙上（階級制・非格闘）")))

message("  対象: ", paste(est$sport, collapse = ", "))

p <- ggplot(est, aes(h, b, colour = grp)) +
  geom_hline(yintercept = 0, colour = PAL$grid, linewidth = 0.5) +
  geom_vline(xintercept = 0, colour = PAL$grid, linewidth = 0.5) +
  geom_errorbarh(aes(xmin = h_lo, xmax = h_hi), height = 0, linewidth = 0.7, alpha = 0.5) +
  geom_errorbar(aes(ymin = b_lo, ymax = b_hi), width = 0, linewidth = 0.7, alpha = 0.5) +
  geom_point(size = 3.2) +
  geom_text(aes(label = sport), colour = PAL$ink, size = 3, vjust = -1.1,
            show.legend = FALSE) +
  scale_colour_manual(
    values = setNames(c(PAL$cat[1], PAL$cat[2], PAL$muted), levels(est$grp)),
    name = NULL
  ) +
  scale_x_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "cm"),
                     expand = expansion(mult = 0.18)) +
  scale_y_continuous(expand = expansion(mult = 0.18)) +
  labs(
    title = "同じ階級制でも、打撃はリーチ（身長）・組技は密度（BMI）に振れる",
    subtitle = paste0(
      "横=メダリストの身長差、縦=BMI差（帯は 95% 信頼区間）。階級で総体重が揃うぶん、\n",
      "「その体重を長く配るか（右）／密に配るか（上）」に競技の本質が出る。",
      BODY_YEARS[1], "-", BODY_YEARS[2]
    ),
    x = "身長差（リーチ側 →）", y = "BMI差（密度・質量側 ↑）",
    caption = paste0(
      SOURCE_CAPTION,
      "\n(大会 × 種目 × 性別) を固定効果、選手 id をクラスタとした頑健標準誤差。",
      "\nレスリングだけは中立。フリー／グレコと多階級で体型の幅が広く、単一方向に寄らないためと見られる。"
    )
  ) +
  theme_olympic()

save_fig(p, "combat_reach_vs_leverage.png", width = 9.5, height = 6.5)

message("12_combat_reach_vs_leverage: 完了")
