# 開発

再現手順・リポジトリ構成・公開の仕組み。分析結果に興味がある読者には不要な内容。

- 分析結果 → [レポート](../reports/eda-report.md)
- データの制約 → [データの癖](data-quality.md)

---

## 実行手順

必要なもの: **R 4.1 以上**（`|>` と `\(x)` を使うため）。RStudio は不要。

```sh
git clone https://github.com/gghatano/olympic-athletes-eda.git
cd olympic-athletes-eda

# 全図を生成する（依存パッケージは自動でインストールされる）
Rscript run_all.R

# 本文に書いた数値が実データと合っているか検証する
Rscript tests/check_claims.R

# レポートを再生成する
Rscript reports/render.R

# 公開サイトを手元で作る。_site/index.html を開けば確認できる
Rscript site/build_site.R
```

`run_all.R` は `figures/` に PNG を出力する。所要時間は 1 分程度。

依存パッケージ（`R/setup.R` が未導入のものだけ入れる）:
`olympicAthletes`, `dplyr`, `tidyr`, `stringr`, `forcats`, `ggplot2`, `scales`,
`ragg`, `systemfonts`, `commonmark`

個別のスクリプトだけ動かす場合も、**リポジトリのルートから**実行する:

```sh
Rscript analysis/05_body_medalist_gap.R
```

### 日本語フォントが 1 つ必要

図のラベルに日本語を使う。Windows / macOS は標準搭載。
Debian 系なら `sudo apt-get install -y fonts-noto-cjk`。

`R/theme_olympic.R` は使う書体を明示的に解決し、**見つからなければエラーで止まる**。
グリフフォールバック任せにすると、環境によっては全ラベルが豆腐文字の PNG が
黙って出来上がるため。`geom_text` はテーマの書体を継承しないので、
`update_geom_defaults()` で geom の既定値ごと差し替えている。

---

## リポジトリ構成

```
├── .github/workflows/pages.yml  図・レポート・サイトを作り直して Pages に公開
├── run_all.R                    全分析スクリプトを順に実行
├── R/
│   ├── setup.R                  依存パッケージの確認・インストール、パス解決
│   ├── prepare_data.R           クリーニング・派生変数・国の継承関係
│   └── theme_olympic.R          全図で共通のテーマと配色、フォント解決
├── analysis/
│   ├── 01_data_quality.R        規模の推移、欠損構造、メダルの網羅性検証
│   ├── 02_medals_country.R      通算メダル、年代ごとの勢力図、金銀銅の構成
│   ├── 03_sport_gender.R        女性参加の拡大、競技別の男女比、種目構成
│   ├── 04_body_metrics.R        競技ごとの体格、体重の広がり、身長の時代変化
│   ├── 05_body_medalist_gap.R   メダリストと非メダリストの体格差
│   ├── 06_host_privilege.R      開催国優位、直近 30 年の順位
│   ├── 07_season_specialists.R  夏季に強い国・冬季に強い国
│   ├── 08_athlete_careers.R     年齢、複数大会への出場
│   └── 09_sports_history.R      競技の栄枯盛衰
├── tests/
│   └── check_claims.R           本文に書いた数値を実データと突き合わせる
├── reports/
│   ├── eda-report.Rmd           レポート（ソース）
│   ├── eda-report.md            レポート（生成物・コミット対象）
│   └── render.R                 knit 実行スクリプト
├── docs/                        データ辞書・データの癖・前処理・分析候補・この文書
├── site/
│   ├── build_site.R             Markdown -> 静的サイト（_site/）
│   └── style.css                サイトの配色（図と同じトークン）
└── figures/                     生成された図（コミット対象）
```

`figures/` と `reports/eda-report.md` は生成物だが、
GitHub 上で結果を読めるようにするためコミットしている。
`_site/` は CI が毎回作り直すのでコミットしない。

図のファイル名に連番は使わない（`fig12_...` ではなく `body_height_trend.png`）。
図が増減するたびに全ドキュメントの参照を振り直す必要が出るため。

---

## 公開の仕組み

`master` に push すると `.github/workflows/pages.yml` が

1. 日本語フォント（`fonts-noto-cjk`）とシステム依存を入れる
2. `run_all.R` で図を、`reports/render.R` でレポートを**再生成**する
3. `tests/check_claims.R` で本文の数値を検証する
4. `site/build_site.R` でサイトを組み立てる
5. 生成物を検査して GitHub Pages に公開する

を実行する。**図をリポジトリのものではなく毎回作り直している**ので、
スクリプトを直したのに図が古いままという状態になれない。
R スクリプトが壊れていれば CI が落ちるため、実質的にテストも兼ねている。

プルリクエストではビルドの成否確認までを行い、公開はしない。

### pandoc も Quarto も使わない

`reports/render.R` は `rmarkdown::render()` ではなく `knitr::knit()` を使い、
`.Rmd` → GitHub 上でそのまま読める `.md` に変換する。
サイト生成も pandoc ではなく R の `commonmark` パッケージで行っている。

理由は、**手元と CI で同じ手順が動く状態を保つため**。
pandoc は R 単体には同梱されない（RStudio にしか付いてこない）ので、
依存に入れると「手元では動くが CI では動かない」あるいはその逆が起きる。
HTML 化に凝る必要がない分量なので、R だけで完結させている。

`site/build_site.R` は Markdown を HTML に変換したうえで、

- GitHub と同じ規則で見出しに `id` を振る（アンカーリンクが両方で一致する）
- `.md` へのリンクを対応する `.html` に張り替える
- サイトに載らないファイル（R スクリプトなど）へのリンクは GitHub の blob URL に向ける

をしている。CI は生成後に**未解決の `.md` リンクが残っていないこと**を検査する。

---

## 本文の数値を検証する仕組み

図とレポートは CI が毎回再生成するので、データが変われば自動で追従する。
一方 **README や docs の本文に直接書いた数値は追従しない**。
`olympicAthletes` は更新中のパッケージなので、上流がデータを足すと
本文だけが静かに古くなる。

`tests/check_claims.R` は本文に出てくる固定の数値を実データと突き合わせ、
食い違えば CI を落とす。検証項目には図の枚数やドキュメントの項目数も含む。

数値がずれたときは、**期待値を実測に書き換えて終わりにしない**。
まず上流で何が変わったかを確認する。

この仕組み自体が機能することは、故意に誤った値を入れたプルリクエストで
CI が実際に落ちることを確認して検証した。

---

## 図の作り方

`R/theme_olympic.R` に配色とテーマを集約している。判断の要点:

- **カテゴリ色は固定順**で使い、系列が増えても循環させない
- **散布図は色でカテゴリを分けない**（全点が互いに比較されるため、
  隣接ペアだけの色覚検証では足りない）。位置とラベルで読ませる
- **メダルは順序尺度**なので、金銀銅の金属色ではなく単一色相の順序ランプを使う
  （銀が無彩色になり色覚検証の彩度下限を通らなかったため）
- 系列が 5 つを超えたら 1 枚に重ねず **small multiples** に分ける
- **箱ひげ図の表示範囲は `coord_cartesian()` で調整する**。
  `scale_*_continuous(limits=)` は統計量の計算前に行を落とすので箱の値が変わる
- **離散軸に `annotate()` を使わない**。数値を渡すと連続量と誤判定されて落ち、
  水準名を渡すと因子の並び順が失われる。注記はサブタイトルに置く

配色は色覚多様性（P/D/T 型）の分離度を検証したうえで採用している。
