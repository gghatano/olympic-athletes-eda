# reports/render.R ------------------------------------------------------------
# eda-report.Rmd を GitHub 上でそのまま読める Markdown に変換する。
#
#   Rscript reports/render.R
#
# rmarkdown::render() ではなく knitr::knit() を使う。
# render() は pandoc を要求するが、このレポートは HTML 化する必要がなく、
# knit() だけなら R と knitr だけで完結するため（環境要件を減らす）。
#
# 図は analysis/ の各スクリプトが figures/ に出力済みのものを参照する。
# レポートを更新する前に run_all.R を実行しておくこと。

if (!requireNamespace("knitr", quietly = TRUE)) {
  install.packages("knitr", repos = "https://cloud.r-project.org")
}

old <- setwd("reports")
on.exit(setwd(old), add = TRUE)

knitr::opts_chunk$set(fig.path = "figures-report/")
knitr::knit("eda-report.Rmd", output = "eda-report.md")

message("reports/eda-report.md を生成しました。")
