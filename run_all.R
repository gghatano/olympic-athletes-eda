# run_all.R -------------------------------------------------------------------
# 全分析スクリプトを順に実行し、figures/ に図を書き出す。
#
#   Rscript run_all.R
#
# リポジトリのルートで実行すること。

source("R/setup.R")
ensure_packages()

scripts <- list.files("analysis", pattern = "^[0-9]{2}_.*\\.R$", full.names = TRUE)
scripts <- sort(scripts)

for (s in scripts) {
  message("\n==== ", basename(s), " ====")
  # 各スクリプトを独立した環境で走らせ、変数の持ち越し事故を防ぐ
  source(s, local = new.env(), echo = FALSE)
}

message("\n完了。figures/ に ", length(list.files(FIG_DIR, pattern = "\\.png$")),
        " 枚の図を出力しました。")
