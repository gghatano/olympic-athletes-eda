# R/prepare_data.R ------------------------------------------------------------
# olympicAthletes の生データを、分析で使う形に整える。
# 方針の根拠と検証結果は docs/preprocessing.md を参照。
#
# 提供するオブジェクト:
#   athletes         選手 × 大会 × 種目（クリーニング + 派生変数）
#   event_medals     種目 × NOC 単位のメダル（団体競技の水増しを解消）
#   medals_official  medal_table（公式集計。国別メダル数はこちらを正とする）
#   editions_clean   大会メタデータ

suppressPackageStartupMessages({
  library(olympicAthletes)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(forcats)
})

# --- データ品質の境界線 ------------------------------------------------------
# 選手行のメダル情報は 2018 冬季以降が不完全（種目の 2〜3 割が欠落）。
# 選手データからメダルを数える分析はこの年までに限定する。
MEDAL_ATHLETE_COMPLETE_MAX_YEAR <- 2016L

# 身長・体重が概ね揃っている期間。1950 年代以前は 7〜9 割欠損、
# 2018 年以降も 4〜7 割欠損に戻るため、両端を落とす。
BODY_YEARS <- c(1960L, 2016L)

MEDAL_LEVELS <- c("Gold", "Silver", "Bronze")

# --- 国の継承関係 ------------------------------------------------------------
# 長期の趨勢を見るとき、分裂・統合した国を素の NOC のまま扱うと
# 系列が途切れる。粗い集約なので、使う図では必ず注記すること。
#
# AIN（Individual Neutral Athletes, 2024-2026）はロシアとベラルーシの
# 選手が混在するため、意図的にどの系列にも割り当てていない。
# ラベルは country 列（英語）と軸上で混在するので英語に揃える。
noc_lineage <- tibble::tribble(
  ~noc,  ~lineage,      ~lineage_label,
  "URS", "RUS_LINEAGE", "USSR / Russia",
  "EUN", "RUS_LINEAGE", "USSR / Russia",
  "RUS", "RUS_LINEAGE", "USSR / Russia",
  "OAR", "RUS_LINEAGE", "USSR / Russia",
  "ROC", "RUS_LINEAGE", "USSR / Russia",
  "GER", "GER_LINEAGE", "Germany (all)",
  "FRG", "GER_LINEAGE", "Germany (all)",
  "GDR", "GER_LINEAGE", "Germany (all)",
  "EUA", "GER_LINEAGE", "Germany (all)",
  "SAA", "GER_LINEAGE", "Germany (all)",
  "TCH", "CZE_LINEAGE", "Czechoslovakia +",
  "CZE", "CZE_LINEAGE", "Czechoslovakia +",
  "SVK", "CZE_LINEAGE", "Czechoslovakia +",
  "YUG", "YUG_LINEAGE", "Yugoslavia +",
  "SCG", "YUG_LINEAGE", "Yugoslavia +",
  "SRB", "YUG_LINEAGE", "Yugoslavia +"
)

# --- 選手データ --------------------------------------------------------------
load_athletes <- function() {
  olympicAthletes::olympic_athletes |>
    as_tibble() |>
    # 全列が一致する完全重複（約 1,385 行）だけを落とす。
    # (id, games, event) の重複は芸術競技の複数出品や
    # セーリングの複数艇など実在のケースを含むので残す。
    distinct() |>
    mutate(
      medal = factor(medal, levels = MEDAL_LEVELS),
      is_medalist = !is.na(medal),
      season = factor(season, levels = c("Summer", "Winter")),
      sex = factor(sex, levels = c("F", "M"), labels = c("Women", "Men")),
      decade = 10L * (year %/% 10L),
      # BMI。身長体重が揃う行だけ計算される。
      bmi = weight / (height / 100)^2,
      # 種目名に性別区分が埋まっている（全種目名の約 9 割）。
      # "Women" を先に判定する（"Men" は "Women" に含まれないが順序を明示）。
      event_gender = case_when(
        str_detect(event, "Women") ~ "Women",
        str_detect(event, "Mixed") ~ "Mixed",
        str_detect(event, "Men") ~ "Men",
        TRUE ~ NA_character_
      ),
      event_gender = factor(event_gender, levels = c("Women", "Men", "Mixed"))
    ) |>
    left_join(noc_lineage, by = "noc")
}

# --- 種目単位のメダル --------------------------------------------------------
# 選手行のままメダルを数えると団体競技が人数分ふくらむ
# （例: 2024 夏の USA は選手行 195 に対し実際の種目メダルは 99）。
# (games, event, noc, medal) で一意化して種目単位に落とす。
#
# 1 種目のメダルが 4 件になることがあるが、これは柔道・ボクシング等で
# 銅メダルが 2 つ授与される正規の仕様。
build_event_medals <- function(athletes) {
  athletes |>
    filter(!is.na(medal), !is.na(noc)) |>
    distinct(games, year, season, decade, noc, lineage, lineage_label,
             sport, event, medal)
}

# --- 公式メダル表 ------------------------------------------------------------
build_medals_official <- function() {
  olympicAthletes::medal_table |>
    as_tibble() |>
    mutate(
      season = factor(season, levels = c("Summer", "Winter")),
      decade = 10L * (year %/% 10L)
    ) |>
    left_join(noc_lineage, by = "noc")
}

build_medals_official_long <- function(medals_official) {
  medals_official |>
    select(games, year, season, decade, noc, country, lineage, lineage_label,
           gold, silver, bronze) |>
    pivot_longer(c(gold, silver, bronze), names_to = "medal", values_to = "n") |>
    mutate(medal = factor(str_to_title(medal), levels = MEDAL_LEVELS))
}

build_editions <- function() {
  olympicAthletes::editions |>
    as_tibble() |>
    mutate(
      season = factor(season, levels = c("Summer", "Winter")),
      decade = 10L * (year %/% 10L)
    )
}

# --- 実体化 ------------------------------------------------------------------
athletes <- load_athletes()
event_medals <- build_event_medals(athletes)
medals_official <- build_medals_official()
medals_official_long <- build_medals_official_long(medals_official)
editions_clean <- build_editions()

# 開催国の NOC。開催国優位の分析で使う。
# editions$country（国名）を medal_table の country -> noc 対応で引く。
host_noc <- editions_clean |>
  select(games, year, season, host_country = country) |>
  left_join(
    medals_official |> distinct(country, noc) |> rename(host_country = country,
                                                        host_noc = noc),
    by = "host_country"
  )
