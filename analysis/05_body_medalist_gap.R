# analysis/05_body_medalist_gap.R ---------------------------------------------
# 体格はメダルに効くのか。競技ごとに、メダリストと非メダリストの体格差を測る。
#
# 素朴に「競技ごとにメダリストの平均身長を比べる」と 2 つの理由で誤る。
#
#   1. 種目構成: 陸上のメダリストが背が高く見えるのは、たまたま背の高い種目
#      （走高跳・砲丸投）のメダリストが多く混ざったから、かもしれない。
#   2. 時代: 選手は時代とともに大型化する。メダリストの年代分布が違えば差が出る。
#
# そこで身長・体重を **種目 × 性別 × 年代** のセル内で z 化してから比較する。
# 「同じ種目・同じ性別・同じ年代で競った相手の中で、相対的にどうだったか」を見る。
#
# さらに団体競技では、チームがメダルを取ると構成員全員がメダリストになる。
# 標本は選手ではなく実質チームなので、素の標準誤差は小さく出すぎる。
# (大会 × 種目 × NOC) をクラスタとみなしたクラスタ頑健標準誤差を使う。
# 実測でホッケー 1.8 倍・野球 2.0 倍に膨らむ。

source("R/setup.R")
ensure_packages()
source("R/theme_olympic.R")
source("R/prepare_data.R")

message("05_body_medalist_gap: 実行中")

MIN_CELL <- 20L      # z 化するセルの最小人数
MIN_ROWS <- 800L     # 競技を対象にする最小行数
MIN_MEDALS <- 100L   # 同上、メダリスト数

scored <- athletes |>
  filter(
    year >= BODY_YEARS[1], year <= BODY_YEARS[2],
    !is.na(height), !is.na(weight), !is.na(noc),
    bmi >= 13, bmi <= 45
  ) |>
  group_by(event, sex, decade) |>
  filter(n() >= MIN_CELL, sd(height) > 0, sd(weight) > 0) |>
  mutate(
    z_height = (height - mean(height)) / sd(height),
    z_weight = (weight - mean(weight)) / sd(weight)
  ) |>
  ungroup() |>
  mutate(cluster = paste(games, event, noc))

message("  対象: ", format(nrow(scored), big.mark = ","), " 行 / ",
        n_distinct(paste(scored$event, scored$sex, scored$decade)), " セル")

# --- クラスタ頑健な平均差 ----------------------------------------------------
# lm(z ~ is_medalist) の係数を、クラスタ単位のサンドイッチ推定（CR1）で評価する。
cluster_robust_diff <- function(y, medalist, cluster) {
  X <- cbind(1, as.numeric(medalist))
  XtX_inv <- tryCatch(solve(crossprod(X)), error = function(e) NULL)
  if (is.null(XtX_inv)) return(c(est = NA_real_, se = NA_real_, n_clust = NA_real_))
  beta <- XtX_inv %*% crossprod(X, y)
  resid <- as.vector(y - X %*% beta)
  groups <- split(seq_along(y), cluster)
  meat <- matrix(0, 2, 2)
  for (idx in groups) {
    s <- crossprod(X[idx, , drop = FALSE], resid[idx])
    meat <- meat + tcrossprod(s)
  }
  G <- length(groups)
  n <- length(y)
  adj <- (G / (G - 1)) * ((n - 1) / (n - 2))
  V <- adj * XtX_inv %*% meat %*% XtX_inv
  c(est = beta[2], se = sqrt(V[2, 2]), n_clust = G)
}

gap_for <- function(d, var) {
  r <- cluster_robust_diff(d[[var]], d$is_medalist, d$cluster)
  sd_raw <- sd(d[[sub("^z_", "", var)]])
  tibble(
    est_sd = unname(r["est"]),
    se_sd = unname(r["se"]),
    # z 単位のままだと読めないので、その競技の実測ばらつきを掛けて実寸に戻す
    est = unname(r["est"]) * sd_raw,
    se = unname(r["se"]) * sd_raw
  )
}

gaps <- scored |>
  group_by(sport) |>
  filter(n() >= MIN_ROWS, sum(is_medalist) >= MIN_MEDALS) |>
  group_modify(~ bind_rows(
    mutate(gap_for(.x, "z_height"), measure = "height"),
    mutate(gap_for(.x, "z_weight"), measure = "weight")
  ) |>
    mutate(n = nrow(.x), n_med = sum(.x$is_medalist),
           n_clust = n_distinct(.x$cluster))) |>
  ungroup() |>
  mutate(
    lo = est - 1.96 * se,
    hi = est + 1.96 * se,
    significant = lo > 0 | hi < 0
  )

height_gap <- filter(gaps, measure == "height") |> arrange(desc(est))

n_sig_pos <- sum(height_gap$significant & height_gap$est > 0)
n_sig_neg <- sum(height_gap$significant & height_gap$est < 0)
message("  競技数 ", nrow(height_gap),
        " / 高身長が有利 ", n_sig_pos, " / 低身長が有利 ", n_sig_neg)

# --- 図1: 競技ごとの身長ギャップ ---------------------------------------------
plot_gap <- height_gap |>
  mutate(
    sport = fct_reorder(sport, est),
    verdict = case_when(
      significant & est > 0 ~ "背が高いほうが有利",
      significant & est < 0 ~ "背が低いほうが有利",
      TRUE ~ "差がはっきりしない"
    ),
    verdict = factor(verdict, levels = c("背が高いほうが有利", "背が低いほうが有利",
                                         "差がはっきりしない"))
  )

p1 <- ggplot(plot_gap, aes(est, sport, colour = verdict)) +
  geom_vline(xintercept = 0, colour = PAL$axis, linewidth = 0.5) +
  geom_segment(aes(x = lo, xend = hi, yend = sport), linewidth = 1.3, alpha = 0.4) +
  geom_point(size = 2.4) +
  scale_colour_manual(
    values = c("背が高いほうが有利" = PAL$cat[1],
               "背が低いほうが有利" = PAL$cat[2],
               "差がはっきりしない" = PAL$muted),
    name = NULL
  ) +
  scale_x_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "cm")) +
  labs(
    title = sprintf("同じ種目で競った相手より、メダリストは %d 競技で背が高い", n_sig_pos),
    subtitle = paste0(
      "メダリストと非メダリストの身長差（種目 × 性別 × 年代の中で標準化してから実寸に戻したもの）。\n",
      "帯は 95% 信頼区間。", BODY_YEARS[1], "-", BODY_YEARS[2], "、", nrow(height_gap), " 競技"
    ),
    x = "身長差（メダリスト − 非メダリスト）", y = NULL,
    caption = paste0(
      SOURCE_CAPTION,
      "\n団体競技はチーム全員が同時にメダリストになるため、(大会 × 種目 × NOC) をクラスタとした",
      "頑健標準誤差を使っている。",
      "\nこれは「その種目に出られた選手の中での傾向」であり、因果ではない。体格に恵まれない選手はそもそも出場していない。"
    )
  ) +
  theme_olympic()

save_fig(p1, "body_medalist_gap.png", height = 7.5)

# --- 図2: 身長と体重、どちらが効いているか -----------------------------------
wide <- gaps |>
  select(sport, measure, est, significant) |>
  pivot_wider(names_from = measure, values_from = c(est, significant))

label_worthy <- wide |>
  filter(significant_height | significant_weight) |>
  mutate(d = est_height^2 + est_weight^2) |>
  slice_max(d, n = 16)

p2 <- ggplot(wide, aes(est_height, est_weight)) +
  geom_hline(yintercept = 0, colour = PAL$grid, linewidth = 0.5) +
  geom_vline(xintercept = 0, colour = PAL$grid, linewidth = 0.5) +
  geom_point(aes(colour = significant_height | significant_weight),
             size = 2.6, alpha = 0.85) +
  geom_text(data = label_worthy, aes(label = sport),
            colour = PAL$ink, size = 2.9, vjust = -1) +
  scale_colour_manual(
    values = c(`TRUE` = PAL$cat[1], `FALSE` = PAL$muted),
    labels = c(`TRUE` = "どちらかが有意", `FALSE` = "差がはっきりしない"),
    name = NULL, breaks = c("TRUE", "FALSE")
  ) +
  scale_x_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "cm"),
                     expand = expansion(mult = 0.12)) +
  scale_y_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "kg"),
                     expand = expansion(mult = 0.12)) +
  labs(
    title = "ほとんどの競技で「大きいほうが有利」だが、体操と重量挙げは逆を向く",
    subtitle = "横軸=メダリストの身長差、縦軸=体重差。右上ほど大柄な選手が勝ちやすい競技",
    x = "身長差", y = "体重差",
    caption = paste0(
      SOURCE_CAPTION,
      "\n重量挙げが「背は低いが重い」側にあるのは階級制のため。",
      "同じ階級なら体重は揃うので、差が出るのは体重より背の低さ。"
    )
  ) +
  theme_olympic()

save_fig(p2, "body_gap_height_weight.png", width = 10, height = 6.5)

message("05_body_medalist_gap: 完了")
