# olympic-athletes-eda

近代オリンピック全大会（1896 アテネ 〜 2026 ミラノ・コルティナ）の選手個票データを
R で探索する分析リポジトリ。

**目的は「オリンピック選手データの理解を深めること」**。
きれいな結論を出すことより、**このデータで何が言えて何が言えないか**を
はっきりさせることを優先している。実際、最初の EDA で
「直近 5 大会（2018 冬 〜 2026 冬）の選手個票は欠損が大きく、
そのままでは使えない」ことが分かった。

🌐 **公開サイト: <https://gghatano.github.io/olympic-athletes-eda/>**（`master` への push ごとに GitHub Actions が再生成）

- 📊 **[分析レポート](reports/eda-report.md)** — 図と数値をまとめたもの。まずここを読む
- 📖 [データ辞書](docs/data-dictionary.md) / [前処理の方針](docs/preprocessing.md) / [今後の分析候補](docs/analysis-ideas.md)

---

## データ出典

| | |
|---|---|
| パッケージ | [`olympicAthletes`](https://moderndive.github.io/olympicAthletes/) 0.5.10 |
| 配布元 | CRAN（`install.packages("olympicAthletes")`） |
| 原典 | [Olympedia.org](https://www.olympedia.org/) |
| 規模 | 315,094 行（選手 × 大会 × 種目）、157,377 人、56 大会、69 競技、1,270 種目 |
| ライセンス | パッケージの [LICENSE](https://github.com/moderndive/olympicAthletes/) に従う |

CSV はリポジトリに同梱していない。パッケージから都度ロードする方針
（データの二重管理を避けるため）。

> パッケージのサイトには「GitHub からのみ入手可」と書かれているが、
> 2026-08-01 時点で CRAN にも公開されている。CRAN 版を使う。

---

## 実行手順

必要なもの: **R 4.1 以上**（`|>` を使うため）。RStudio は不要。

```sh
git clone https://github.com/gghatano/olympic-athletes-eda.git
cd olympic-athletes-eda

# 1. 全図を生成する（依存パッケージは自動でインストールされる）
Rscript run_all.R

# 2. レポートを再生成する（任意）
Rscript reports/render.R

# 3. 公開サイトを手元で作る（任意）。_site/index.html を開けば確認できる
Rscript site/build_site.R
```

`run_all.R` は `figures/` に 15 枚の PNG を出力する。所要時間は 1 分程度。

依存パッケージ（`R/setup.R` が未導入のものだけ入れる）:
`olympicAthletes`, `dplyr`, `tidyr`, `stringr`, `forcats`, `ggplot2`, `scales`,
`ragg`, `systemfonts`, `commonmark`

図のラベルに日本語を使うため、**日本語フォントが 1 つ必要**。
Windows / macOS は標準搭載。Debian 系なら `sudo apt-get install -y fonts-noto-cjk`。
見つからない場合は豆腐文字を出さずにエラーで止まる（`R/theme_olympic.R`）。

個別のスクリプトだけ動かしたい場合も、**リポジトリのルートから**実行する:

```sh
Rscript analysis/04_body_metrics.R
```

### レポートに pandoc が要らない理由

`reports/render.R` は `rmarkdown::render()` ではなく `knitr::knit()` を使い、
`.Rmd` → GitHub 上でそのまま読める `.md` に変換する。
HTML 化する必要がないので、pandoc（R 単体には同梱されない）への依存を外した。

---

## リポジトリ構成

```
├── .github/workflows/pages.yml  図・レポート・サイトを作り直して Pages に公開
├── run_all.R                    全分析スクリプトを順に実行
├── R/
│   ├── setup.R                  依存パッケージの確認・インストール、パス解決
│   ├── prepare_data.R           クリーニング・派生変数・国の継承関係
│   └── theme_olympic.R          全図で共通のテーマと配色
├── analysis/
│   ├── 01_data_overview.R       規模の推移、欠損構造、メダルの網羅性検証
│   ├── 02_medals_country_era.R  通算メダル、年代ごとの勢力図、金銀銅の構成
│   ├── 03_sport_gender.R        女性参加の拡大、競技別の男女比、種目構成
│   ├── 04_body_metrics.R        競技ごとの体格、体重の広がり、身長の時代変化
│   └── 05_extras.R              年齢、開催国優位、メダル獲得国の広がり
├── reports/
│   ├── eda-report.Rmd           分析レポート（ソース）
│   ├── eda-report.md            分析レポート（生成物・コミット対象）
│   └── render.R                 knit 実行スクリプト
├── docs/
│   ├── data-dictionary.md       全列の型・欠損率・値域と注意点
│   ├── preprocessing.md         前処理の方針とその根拠
│   └── analysis-ideas.md        今後掘れる問い 20 件
├── site/
│   ├── build_site.R             Markdown -> 静的サイト（_site/）
│   └── style.css                サイトの配色（図と同じトークン）
└── figures/                     生成された図（コミット対象）
```

`figures/` と `reports/eda-report.md` は生成物だが、
GitHub 上で結果を読めるようにするためコミットしている。
`_site/` は CI が毎回作り直すのでコミットしない。

### 公開の仕組み

`master` に push すると `.github/workflows/pages.yml` が

1. 日本語フォント（`fonts-noto-cjk`）を入れる
2. `run_all.R` で図を、`reports/render.R` でレポートを**再生成**する
3. `site/build_site.R` でサイトを組み立てる
4. GitHub Pages に公開する

を実行する。図をリポジトリのものではなく毎回作り直しているので、
スクリプトを直したのに図が古いままという状態になれない。
R スクリプトが壊れていれば CI が落ちるため、実質的にテストも兼ねている。

サイト生成にも pandoc は使わず、R の `commonmark` で Markdown を HTML にしている
（`reports/render.R` と同じ理由。手元と CI で同じ手順が動く）。

---

## 使う前に知っておくべきこと

このデータには **年代によって性質が変わる**という強い制約がある。
全期間を一括で扱うと、実態ではなく記録の整備状況を分析してしまう。

![欠損率の年代推移](figures/fig02_missingness.png)

| 分析対象 | 使える期間 | 理由 |
|---|---|---|
| 参加者数・性別 | **全期間** | `sex` は欠損ゼロ |
| 国別メダル数 | **全期間**（ただし `medal_table` を使う） | 公式集計は完全 |
| 選手データからのメダル集計 | **1896-2016** | 2018 年以降は 2〜3 割の種目が欠落 |
| 身長・体重・BMI | **1960-2016** | 両端で 4〜9 割が欠損 |
| 年齢 | 1960 年以降を推奨 | それ以前は最大 43% が欠損 |

さらに集計時の落とし穴が 3 つある。詳細は [前処理の方針](docs/preprocessing.md) に書いた。

1. **`medal` の NA は「メダルなし」**であって欠損値ではない（86% が NA）
2. **団体競技はメダル 1 個に選手行が人数分ある**。
   2024 夏の USA は選手行 195 に対し、種目単位では 99
3. **`(id, games, event)` の重複 1,480 行は多くが実在**（芸術競技の複数出品、
   セーリングで同一選手が複数艇に乗るケース）。安易に重複排除すると実データを壊す

---

## 初期 EDA でわかったこと

詳細と全 15 図は **[分析レポート](reports/eda-report.md)** にある。ここでは 3 つだけ。

### 女子種目を「足して」きた

![種目構成の変化](figures/fig09_events_by_gender.png)

女性の参加比率は 1900 年の 1.7% から 2024 年の 48.3% まで伸びたが、
その中身は男子種目を削った結果ではない。男子種目数はほぼ横ばいのまま、
女子種目を積み増して総種目数を伸ばしている。

### 競技は体格の平面上にきれいに並ぶ

![競技ごとの体格](figures/fig10_sport_body_map.png)

右上にバスケットボール・バレーボール・ボート、左下に体操・飛込。
重量挙げだけが「背は低いが重い」領域に単独で位置する。

### 開催国は 57 大会中 89% でメダルシェアを伸ばした

![開催国優位](figures/fig14_host_advantage.png)

上振れ幅の中央値は全期間で +2.9 ポイント、1960 年以降に限ると +1.7 ポイント。
初期大会は参加国が少なく開催国の比率が構造的に高いため、幅は年代とともに縮む。
これは相関であって因果ではない。

---

## 次に掘る問い

[docs/analysis-ideas.md](docs/analysis-ideas.md) に 20 件を難易度つきで整理した。
優先度が高いのは 3 つ。

1. **2018 年以降の欠損は上流の取り込み漏れか**（`E-1`）。
   解消するなら 2016 年で切る制約が外れ、分析の射程が大きく広がる
2. **夏季に強い国・冬季に強い国**（`A-2`）。既存の前処理でそのまま書ける
3. **同じ種目の中で体格は成績に効くか**（`C-1`）。
   このデータで最も「わかっていないこと」に近い問い

---

## 図の作り方について

`R/theme_olympic.R` に配色とテーマを集約している。判断の要点:

- カテゴリ色は**固定順**で使い、系列が増えても循環させない
- 散布図は全点が互いに比較されるため、色でカテゴリを分けない（位置とラベルで読ませる）
- メダルは順序尺度なので、金銀銅の金属色ではなく**単一色相の順序ランプ**を使う
  （銀が無彩色になり色覚検証の彩度下限を通らなかったため）
- 系列が 5 つを超えたら 1 枚に重ねず small multiples に分ける

配色は色覚多様性（P/D/T 型）の分離度を検証したうえで採用している。
根拠は [docs/preprocessing.md の「配色」節](docs/preprocessing.md#6-配色)。
