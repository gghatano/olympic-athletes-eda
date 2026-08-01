# tests/check_claims.R --------------------------------------------------------
# README と docs/ に書いた「固定の数値」を実データに突き合わせる。
#
#   Rscript tests/check_claims.R
#
# なぜ要るか:
#   図とレポートの数値は CI が毎回再生成するので勝手に追従する。
#   一方 README・docs の本文に直接書いた数値は追従しない。
#   olympicAthletes は更新中のパッケージ（2026-08 時点で 0.5.x）なので、
#   上流がデータを足すと本文だけが静かに古くなる。
#   ここで落としてしまえば、気づかず誤った数値を公開し続けることがなくなる。
#
# 数値がずれた場合は、まず上流で何が変わったかを確認してから本文を直すこと。
# 「テストの期待値を実測に合わせて書き換える」だけで済ませない。

source("R/setup.R")
ensure_packages()
source("R/prepare_data.R")
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

failures <- character(0)
n_checks <- 0L

expect <- function(label, actual, expected, tol = 0) {
  n_checks <<- n_checks + 1L
  ok <- if (is.numeric(actual) && is.numeric(expected)) {
    abs(actual - expected) <= tol
  } else {
    identical(actual, expected)
  }
  if (ok) {
    message(sprintf("  ok   %-58s %s", label, format(actual)))
  } else {
    msg <- sprintf("FAIL %-58s 実測 %s / 記載 %s",
                   label, format(actual), format(expected))
    message("  ", msg)
    failures <<- c(failures, msg)
  }
}

message("README / docs に書いた数値を検証します\n")

# --- データの規模（README「データ出典」の表） --------------------------------
raw <- olympicAthletes::olympic_athletes
expect("生データの行数", nrow(raw), 315094)
expect("完全重複を除いた行数", nrow(athletes), 313709)
expect("選手数 (id)", n_distinct(athletes$id), 157377)
expect("大会数", n_distinct(athletes$games), 56)
expect("競技数", n_distinct(athletes$sport), 69)
expect("種目数", n_distinct(athletes$event), 1270)
expect("パッケージ版", as.character(packageVersion("olympicAthletes")), "0.5.10")

# --- 欠損の構造（README「使う前に知っておくべきこと」） ----------------------
pct_na <- function(g, col) {
  round(100 * mean(is.na(athletes[[col]][athletes$games == g])), 1)
}
expect("2016 夏 height 欠損率", pct_na("2016 Summer", "height"), 1.3, tol = 0.05)
expect("2024 夏 height 欠損率", pct_na("2024 Summer", "height"), 65.7, tol = 0.05)
expect("2024 夏 noc 欠損率", pct_na("2024 Summer", "noc"), 10.4, tol = 0.05)

# --- 団体競技の水増し（README の落とし穴 2） ---------------------------------
expect("2024 夏 USA のメダル選手行数",
       nrow(filter(athletes, games == "2024 Summer", noc == "USA", !is.na(medal))), 195)
expect("2024 夏 USA の種目単位メダル数",
       nrow(filter(event_medals, games == "2024 Summer", noc == "USA")), 99)

# --- 重複行（README の落とし穴 3） -------------------------------------------
expect("(id, games, event) の重複行数",
       sum(duplicated(raw[, c("id", "games", "event")])), 1480)
expect("全列一致の完全重複行数", sum(duplicated(raw)), 1385)

# --- メダル網羅性（docs/preprocessing.md 1-1） -------------------------------
coverage <- function(g) {
  d <- nrow(filter(event_medals, games == g))
  o <- sum(medals_official$total[medals_official$games == g])
  c(derived = d, official = o)
}
c2024 <- coverage("2024 Summer")
expect("2024 夏 種目メダル数(選手データ由来)", unname(c2024["derived"]), 802)
expect("2024 夏 公式メダル総数", unname(c2024["official"]), 1044)
expect("2024 夏 メダル付き種目数",
       n_distinct(filter(athletes, games == "2024 Summer", !is.na(medal))$event), 249)
expect("2024 夏 実施種目数(editions)",
       editions_clean$medal_events[editions_clean$games == "2024 Summer"], 329)

# 選手データ由来の集計は公式とどれだけずれるか（docs/preprocessing.md 4）
complete_games <- athletes |> filter(year <= 2016) |> distinct(games)
diffs <- event_medals |>
  count(games, name = "derived") |>
  inner_join(complete_games, by = "games") |>
  left_join(medals_official |> group_by(games) |>
              summarise(official = sum(total), .groups = "drop"), by = "games") |>
  mutate(d = derived - official)
expect("1896-2016 の突合対象大会数", nrow(diffs), 51)
expect("公式との平均絶対差", round(mean(abs(diffs$d)), 1), 1.2, tol = 0.05)
expect("公式との最大の過大", max(diffs$d), 7)
expect("公式との最大の過小", min(diffs$d), -4)

# --- 性別（README・レポート） ------------------------------------------------
share_w <- function(g) {
  round(100 * mean(athletes$sex[athletes$games == g] == "Women"), 1)
}
expect("1900 夏 女性比率", share_w("1900 Summer"), 1.7, tol = 0.05)
expect("2024 夏 女性比率", share_w("2024 Summer"), 48.3, tol = 0.05)

recent <- athletes |>
  filter(season == "Summer", year >= 2016) |>
  group_by(sport) |>
  summarise(n = n(), sw = 100 * mean(sex == "Women"), .groups = "drop") |>
  filter(n >= 300)
expect("直近3大会で対象となる競技数", nrow(recent), 30)
expect("女性比率が最も低い競技", recent$sport[which.min(recent$sw)], "Boxing")
expect("その比率", round(min(recent$sw), 1), 32.0, tol = 0.05)
expect("女性のみ競技を除き5割超の競技数",
       sum(recent$sw > 50 & recent$sport != "Synchronized Swimming"), 3)

# --- 身体データ（README・fig12） ---------------------------------------------
body <- athletes |>
  filter(year >= 1960, year <= 2016, !is.na(height), !is.na(weight),
         bmi >= 13, bmi <= 45)
expect("身体データの対象行数", nrow(body), 196619)
# fig12 と同じく夏季に限る。夏冬を混ぜると開催年が交互になり比較にならない。
gain <- body |>
  filter(season == "Summer", year %in% c(1960, 2016)) |>
  group_by(sex, year) |>
  summarise(m = median(height), .groups = "drop") |>
  pivot_wider(names_from = year, values_from = m) |>
  mutate(g = `2016` - `1960`)
expect("夏季 身長の伸び（女性・端点差 cm）", gain$g[gain$sex == "Women"], 7)
expect("夏季 身長の伸び（男性・端点差 cm）", gain$g[gain$sex == "Men"], 7)
slopes <- body |>
  filter(season == "Summer") |>
  group_by(sex, year) |>
  summarise(m = median(height), .groups = "drop") |>
  group_by(sex) |>
  summarise(s = unname(coef(lm(m ~ year))[2]) * 56, .groups = "drop")
expect("夏季 身長の伸び（女性・線形近似 cm）",
       round(slopes$s[slopes$sex == "Women"], 1), 7.0, tol = 0.05)
expect("夏季 身長の伸び（男性・線形近似 cm）",
       round(slopes$s[slopes$sex == "Men"], 1), 7.1, tol = 0.05)

# --- 開催国優位（README・fig14） ---------------------------------------------
edition_totals <- medals_official |>
  group_by(games, year, season) |>
  summarise(all_medals = sum(total), .groups = "drop")
shares <- medals_official |>
  left_join(edition_totals, by = c("games", "year", "season")) |>
  mutate(share = 100 * total / all_medals) |>
  select(noc, year, season, share)
he <- host_noc |>
  filter(!is.na(host_noc)) |>
  rowwise() |>
  mutate(
    home = {
      v <- shares$share[shares$noc == host_noc & shares$year == year &
                          shares$season == season]
      if (length(v) == 0) NA_real_ else v[1]
    },
    baseline = {
      v <- shares$share[shares$noc == host_noc & shares$season == season &
                          shares$year != year & abs(shares$year - year) <= 9]
      if (length(v) == 0) NA_real_ else mean(v)
    }
  ) |>
  ungroup() |>
  filter(!is.na(home), !is.na(baseline)) |>
  mutate(lift = home - baseline)

expect("開催国優位の比較対象大会数", nrow(he), 56)
expect("上振れした割合(%)", round(100 * mean(he$lift > 0)), 91)
expect("上振れ幅の中央値(pt)", round(median(he$lift), 1), 2.9, tol = 0.05)
expect("1960 年以降の中央値(pt)", round(median(he$lift[he$year >= 1960]), 1), 1.7, tol = 0.05)

# host_noc は 1 大会 1 行であること（1956 夏の二重計上を防いだことの確認）
expect("host_noc に重複する games がない",
       nrow(count(host_noc, games) |> filter(n > 1)), 0)

# --- メダル構成（fig06） -----------------------------------------------------
comp <- medals_official_long |>
  mutate(entity = coalesce(lineage_label, country)) |>
  group_by(entity) |>
  mutate(total_all = sum(n)) |>
  ungroup()
top12 <- comp |> distinct(entity, total_all) |> slice_max(total_all, n = 12) |> pull(entity)
gold_share <- comp |>
  filter(entity %in% top12) |>
  group_by(entity) |>
  summarise(gs = 100 * sum(n[medal == "Gold"]) / sum(n), .groups = "drop")
expect("金比率の開き(ポイント)",
       round(max(gold_share$gs) - min(gold_share$gs)), 13)

# --- 分析候補の件数（README） ------------------------------------------------
ideas <- readLines("docs/analysis-ideas.md", warn = FALSE, encoding = "UTF-8")
expect("analysis-ideas.md の項目数",
       sum(grepl("^### [A-E]-[0-9]+\\.", ideas)), 20)

# --- 体格とメダル（README・レポート 1 章） -----------------------------------
# analysis/05_body_medalist_gap.R と同じ手続きを再現する。
scored <- athletes |>
  filter(year >= 1960, year <= 2016, !is.na(height), !is.na(weight), !is.na(noc),
         bmi >= 13, bmi <= 45) |>
  group_by(event, sex, decade) |>
  filter(n() >= 20, sd(height) > 0, sd(weight) > 0) |>
  mutate(z_height = (height - mean(height)) / sd(height)) |>
  ungroup() |>
  mutate(cluster = paste(games, event, noc))

cr_diff <- function(y, medalist, cluster) {
  X <- cbind(1, as.numeric(medalist))
  XtX_inv <- solve(crossprod(X))
  beta <- XtX_inv %*% crossprod(X, y)
  resid <- as.vector(y - X %*% beta)
  meat <- matrix(0, 2, 2)
  for (idx in split(seq_along(y), cluster)) {
    s <- crossprod(X[idx, , drop = FALSE], resid[idx])
    meat <- meat + tcrossprod(s)
  }
  G <- length(unique(cluster)); n <- length(y)
  V <- (G / (G - 1)) * ((n - 1) / (n - 2)) * XtX_inv %*% meat %*% XtX_inv
  c(est = beta[2], se = sqrt(V[2, 2]))
}

gaps <- scored |>
  group_by(sport) |>
  filter(n() >= 800, sum(is_medalist) >= 100) |>
  group_modify(~ {
    r <- cr_diff(.x$z_height, .x$is_medalist, .x$cluster)
    tibble(cm = unname(r["est"]) * sd(.x$height), se_cm = unname(r["se"]) * sd(.x$height))
  }) |>
  ungroup() |>
  mutate(sig = abs(cm / se_cm) > 1.96)

expect("体格ギャップの対象競技数", nrow(gaps), 40)
expect("差が有意な競技数", sum(gaps$sig), 26)
expect("高身長が有利な競技数", sum(gaps$sig & gaps$cm > 0), 24)
expect("低身長が有利な競技数", sum(gaps$sig & gaps$cm < 0), 2)
expect("身長差が最大の競技", gaps$sport[which.max(gaps$cm)], "Swimming")
expect("競泳の身長差(cm)", round(gaps$cm[gaps$sport == "Swimming"], 1), 4.4, tol = 0.05)
expect("低身長が有利な競技（負で最大）",
       gaps$sport[which.min(gaps$cm)], "Gymnastics")

# --- 夏冬の偏り（README・レポート 3 章） -------------------------------------
by_country <- medals_official |>
  mutate(entity = coalesce(lineage_label, country)) |>
  group_by(entity, season) |>
  summarise(medals = sum(total), .groups = "drop") |>
  pivot_wider(names_from = season, values_from = medals, values_fill = 0) |>
  mutate(total = Summer + Winter, winter_share = 100 * Winter / total) |>
  filter(total >= 120)
expect("冬季比率で見る対象主体数", nrow(by_country), 32)
expect("最も冬季寄りの主体", by_country$entity[which.max(by_country$winter_share)], "Norway")
expect("ノルウェーの冬季比率(%)",
       round(by_country$winter_share[by_country$entity == "Norway"]), 72)
expect("全メダルに占める冬季の割合(%)",
       round(100 * sum(medals_official$total[medals_official$season == "Winter"]) /
               sum(medals_official$total), 1), 17.5, tol = 0.05)

# --- 開催国特権 直近 30 年（README・レポート 4 章） ---------------------------
recent_hosts <- he |> filter(year >= 1996) |> arrange(desc(lift))
expect("直近30年の開催大会数", nrow(recent_hosts), 16)
expect("特権を最も使い切った大会",
       paste(recent_hosts$host_country[1], recent_hosts$year[1]), "United States 2002")
expect("上位5大会のうち冬季の数",
       sum(head(recent_hosts, 5)$season == "Winter"), 4)

# --- 選手のキャリア（レポート 5 章） -----------------------------------------
careers <- athletes |>
  group_by(id) |>
  summarise(n_games = n_distinct(games), .groups = "drop")
expect("1 大会のみの選手の割合(%)", round(100 * mean(careers$n_games == 1)), 72)
expect("5 大会以上に出た選手数", sum(careers$n_games >= 5), 840)

# --- 競技の栄枯盛衰（README・レポート 6 章） ---------------------------------
last_edition <- athletes |>
  group_by(season) |>
  summarise(last_year = max(year), .groups = "drop")
sport_years <- athletes |>
  distinct(sport, season, year) |>
  add_count(sport, season, name = "n_years") |>
  group_by(sport) |>
  filter(season == season[which.max(n_years)]) |>
  ungroup()
gone <- sport_years |>
  group_by(sport, season) |>
  summarise(final_year = max(year), n_editions = n_distinct(year), .groups = "drop") |>
  left_join(last_edition, by = "season") |>
  filter(final_year < last_year)
expect("実施されなくなった競技数", nrow(gone), 18)
expect("消えた競技のうち最多実施", gone$sport[which.max(gone$n_editions)], "Art Competitions")

# --- 図の枚数（README・ワークフロー） ----------------------------------------
expect("figures/ の PNG 枚数",
       length(list.files("figures", pattern = "\\.png$")), 21)

# --- 結果 --------------------------------------------------------------------
message("")
if (length(failures) > 0) {
  message(length(failures), " / ", n_checks, " 件の検証に失敗しました:\n")
  for (f in failures) message("  ", f)
  message("\n本文の数値が実データと合っていません。",
          "上流パッケージの更新内容を確認してから本文を直してください。")
  quit(status = 1)
}
message("全 ", n_checks, " 件の検証に成功しました。")
