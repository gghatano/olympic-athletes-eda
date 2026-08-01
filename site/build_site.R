# site/build_site.R -----------------------------------------------------------
# リポジトリの Markdown から GitHub Pages 用の静的サイトを生成する。
#
#   Rscript site/build_site.R
#
# 出力先は _site/。
#
# pandoc ではなく commonmark（R パッケージ）を使う。
# reports/render.R が pandoc 依存を避けたのと同じ理由で、
# R だけでビルドが完結する状態を保つ（ローカルでも CI でも同じ手順で動く）。

suppressPackageStartupMessages(library(commonmark))

ROOT <- normalizePath(
  if (file.exists("site/build_site.R")) "." else "..",
  winslash = "/"
)
OUT <- file.path(ROOT, "_site")
REPO_URL <- "https://github.com/gghatano/olympic-athletes-eda"
BLOB <- paste0(REPO_URL, "/blob/master/")

# --- サイト構成 --------------------------------------------------------------
PAGES <- data.frame(
  src = c("README.md", "reports/eda-report.md", "docs/data-dictionary.md",
          "docs/preprocessing.md", "docs/analysis-ideas.md"),
  out = c("index.html", "reports/eda-report.html", "docs/data-dictionary.html",
          "docs/preprocessing.html", "docs/analysis-ideas.html"),
  nav = c("概要", "分析レポート", "データ辞書", "前処理", "分析候補"),
  title = c("olympic-athletes-eda", "分析レポート", "データ辞書",
            "前処理の方針", "興味深い分析の候補"),
  stringsAsFactors = FALSE
)

SITE_TITLE <- "olympic-athletes-eda"
SITE_DESC <- "moderndive olympicAthletes データの探索的分析"

# --- パス操作 ----------------------------------------------------------------

# "reports/../docs/x.md" のような相対パスを "docs/x.md" に畳む
normalize_rel <- function(path) {
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  parts <- parts[parts != "." & parts != ""]
  stack <- character(0)
  for (p in parts) {
    if (p == "..") {
      if (length(stack)) stack <- stack[-length(stack)]
    } else {
      stack <- c(stack, p)
    }
  }
  paste(stack, collapse = "/")
}

# from（出力ファイル）から to（出力ファイル）への相対リンク
relative_link <- function(from, to) {
  f <- head(strsplit(from, "/", fixed = TRUE)[[1]], -1)
  t <- strsplit(to, "/", fixed = TRUE)[[1]]
  while (length(f) && length(t) > 1 && f[1] == t[1]) {
    f <- f[-1]
    t <- t[-1]
  }
  paste(c(rep("..", length(f)), t), collapse = "/")
}

# --- 見出しの id 付与 --------------------------------------------------------
# commonmark は見出しに id を付けない。README から
# docs/preprocessing.md#6-配色 のようなリンクを張っているため、
# GitHub と同じ規則で振ってアンカーを一致させる。
slugify <- function(x) {
  x <- gsub("<[^>]*>", "", x)
  x <- tolower(x)
  # PCRE の [:alnum:] は既定で ASCII しか拾わないため、日本語の見出しが
  # まるごと消えて "#section-2" のような id になってしまう。
  # Unicode プロパティ \p{L} / \p{N} を使って文字種を判定する。
  x <- gsub("[^\\p{L}\\p{N}[:space:]_-]", "", x, perl = TRUE)
  x <- trimws(x)
  gsub("[[:space:]]+", "-", x)
}

add_heading_ids <- function(html) {
  seen <- character(0)
  # 属性なしの見出しだけにマッチする。置換すると id が付いて再マッチしなくなるので、
  # 先頭から 1 つずつ処理していけば必ず停止する。
  pattern <- "<h([1-6])>(.*?)</h\\1>"
  repeat {
    m <- regexpr(pattern, html, perl = TRUE)
    if (m == -1) break
    matched <- regmatches(html, m)
    lvl <- sub(pattern, "\\1", matched, perl = TRUE)
    inner <- sub(pattern, "\\2", matched, perl = TRUE)
    slug <- slugify(inner)
    if (slug == "") slug <- paste0("section-", length(seen) + 1)
    n_dup <- sum(seen == slug)
    seen <- c(seen, slug)
    if (n_dup > 0) slug <- paste0(slug, "-", n_dup)
    regmatches(html, m) <- sprintf(
      '<h%s id="%s">%s<a class="anchor" href="#%s" aria-label="link">#</a></h%s>',
      lvl, slug, inner, slug, lvl
    )
  }
  html
}

# --- リンクの書き換え --------------------------------------------------------
# サイトに載らないリポジトリ内のファイル（R スクリプト等）へのリンクは
# GitHub の blob URL へ向ける。載るものは対応する .html へ向ける。
rewrite_links <- function(html, src, out) {
  src_dir <- dirname(src)
  if (src_dir == ".") src_dir <- ""

  fix_one <- function(href) {
    if (grepl("^(https?:|mailto:|#)", href)) return(href)

    anchor <- ""
    if (grepl("#", href, fixed = TRUE)) {
      parts <- strsplit(href, "#", fixed = TRUE)[[1]]
      anchor <- paste0("#", paste(parts[-1], collapse = "#"))
      href <- parts[1]
      if (href == "") return(anchor)
    }

    target <- normalize_rel(
      paste0(if (nzchar(src_dir)) paste0(src_dir, "/") else "", href)
    )

    idx <- match(target, PAGES$src)
    if (!is.na(idx)) {
      return(paste0(relative_link(out, PAGES$out[idx]), anchor))
    }
    # 画像などサイトにコピーする資産は相対のまま
    if (grepl("^figures/", target)) {
      return(paste0(relative_link(out, target), anchor))
    }
    # それ以外のリポジトリ内ファイルは GitHub 上のソースへ
    paste0(BLOB, target, anchor)
  }

  m <- gregexpr('(href|src)="([^"]*)"', html, perl = TRUE)
  attrs <- regmatches(html, m)[[1]]
  if (length(attrs)) {
    fixed_attrs <- vapply(attrs, function(a) {
      key <- sub('^(href|src)=".*$', "\\1", a, perl = TRUE)
      val <- sub('^(href|src)="([^"]*)"$', "\\2", a, perl = TRUE)
      sprintf('%s="%s"', key, fix_one(val))
    }, character(1), USE.NAMES = FALSE)
    tmp <- regmatches(html, m)
    tmp[[1]] <- fixed_attrs
    regmatches(html, m) <- tmp
  }
  html
}

# --- ページ組み立て ----------------------------------------------------------
build_nav <- function(out) {
  items <- vapply(seq_len(nrow(PAGES)), function(i) {
    active <- if (PAGES$out[i] == out) ' class="active"' else ""
    sprintf('<a href="%s"%s>%s</a>',
            relative_link(out, PAGES$out[i]), active, PAGES$nav[i])
  }, character(1))
  paste(items, collapse = "\n      ")
}

wrap_page <- function(body, title, out) {
  head_title <- if (out == "index.html") {
    paste0(SITE_TITLE, " — ", SITE_DESC)
  } else {
    paste0(title, " — ", SITE_TITLE)
  }
  sprintf('<!doctype html>
<html lang="ja">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>%s</title>
<meta name="description" content="%s">
<link rel="stylesheet" href="%s">
</head>
<body>
<header class="site-header">
  <div class="wrap header-inner">
    <a class="brand" href="%s">%s</a>
    <nav>
      %s
    </nav>
  </div>
</header>
<main class="wrap">
%s
</main>
<footer class="site-footer">
  <div class="wrap">
    <p>データ: <a href="https://moderndive.github.io/olympicAthletes/">moderndive/olympicAthletes</a>（原典 <a href="https://www.olympedia.org/">Olympedia</a>）</p>
    <p>ソース: <a href="%s">%s</a><br>このページは <code>Rscript site/build_site.R</code> が生成しています。</p>
  </div>
</footer>
</body>
</html>
',
    head_title, SITE_DESC,
    relative_link(out, "assets/style.css"),
    relative_link(out, "index.html"), SITE_TITLE,
    build_nav(out),
    body,
    REPO_URL, REPO_URL
  )
}

write_utf8 <- function(text, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeBin(charToRaw(enc2utf8(text)), con)
}

# --- ビルド ------------------------------------------------------------------
if (dir.exists(OUT)) unlink(OUT, recursive = TRUE)
dir.create(OUT, recursive = TRUE)

for (i in seq_len(nrow(PAGES))) {
  src <- PAGES$src[i]
  out <- PAGES$out[i]
  md <- paste(readLines(file.path(ROOT, src), warn = FALSE, encoding = "UTF-8"),
              collapse = "\n")

  html <- commonmark::markdown_html(md, extensions = TRUE, smart = FALSE)
  html <- add_heading_ids(html)
  html <- rewrite_links(html, src, out)

  write_utf8(wrap_page(html, PAGES$title[i], out), file.path(OUT, out))
  message("  生成: ", out)
}

# --- 資産のコピー ------------------------------------------------------------
dir.create(file.path(OUT, "assets"), recursive = TRUE, showWarnings = FALSE)
invisible(file.copy(file.path(ROOT, "site", "style.css"),
                    file.path(OUT, "assets", "style.css"), overwrite = TRUE))

figs <- list.files(file.path(ROOT, "figures"), pattern = "\\.png$", full.names = TRUE)
dir.create(file.path(OUT, "figures"), recursive = TRUE, showWarnings = FALSE)
invisible(file.copy(figs, file.path(OUT, "figures"), overwrite = TRUE))
message("  図をコピー: ", length(figs), " 枚")

# GitHub Pages 側の Jekyll 処理を止める
invisible(file.create(file.path(OUT, ".nojekyll")))

message("_site/ にサイトを生成しました。")
