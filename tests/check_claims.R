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
expect("2024 夏 女性比率", share_w("2024 Summer"), 99.9, tol = 0.05)  # 意図的に誤った値

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

# --- 図の枚数（README・ワークフロー） ----------------------------------------
expect("figures/ の PNG 枚数",
       length(list.files("figures", pattern = "\\.png$")), 15)

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
