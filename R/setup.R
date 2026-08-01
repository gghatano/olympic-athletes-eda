# R/setup.R -------------------------------------------------------------------
# 依存パッケージの確認とインストール。
# 各分析スクリプトの冒頭で source() される。

REQUIRED_PKGS <- c(
  "olympicAthletes", # データ本体（CRAN）
  "dplyr", "tidyr", "stringr", "forcats", # データ加工
  "ggplot2", "scales", # 可視化
  "ragg" # PNG 出力（フォント描画が安定する）
)

ensure_packages <- function(pkgs = REQUIRED_PKGS,
                            repos = "https://cloud.r-project.org") {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    message("インストールします: ", paste(missing, collapse = ", "))
    install.packages(missing, repos = repos)
  }
  invisible(pkgs)
}

# リポジトリルートを推定する。
# Rscript でどのディレクトリから起動されても図の出力先がぶれないようにする。
project_root <- function() {
  candidates <- c(getwd(), file.path(getwd(), ".."))
  for (p in candidates) {
    if (file.exists(file.path(p, "R", "setup.R"))) {
      return(normalizePath(p, winslash = "/"))
    }
  }
  normalizePath(getwd(), winslash = "/")
}

PROJECT_ROOT <- project_root()
FIG_DIR <- file.path(PROJECT_ROOT, "figures")
if (!dir.exists(FIG_DIR)) dir.create(FIG_DIR, recursive = TRUE)
