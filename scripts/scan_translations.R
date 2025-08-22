# --- Translation key scanner (base R) ---

# Extract "quoted strings" from a chunk of code
.extract_quoted <- function(x) {
  m <- gregexpr("(['\"]).*?\\1", x, perl = TRUE)
  qs <- regmatches(x, m)
  unlist(lapply(qs, function(v) gsub("^['\"]|['\"]$", "", v)), use.names = FALSE)
}

# Parse function calls like t("key"), .t_global("key"), t_or_default("key", ...)
.parse_t_calls <- function(lines) {
  pat <- "\\b(t|\\.t_global|t_or_default)\\s*\\(\\s*(['\"])\\s*([^'\"]+)\\2"
  m <- regexec(pat, lines, perl = TRUE)
  hits <- regmatches(lines, m)
  keys <- vapply(hits, function(h) if (length(h) >= 4) h[4] else NA_character_, character(1))
  keys[!is.na(keys)]
}

# Parse label_keys=c("A","B",...) anywhere in the file
.parse_label_keys <- function(lines) {
  pat <- "label_keys\\s*=\\s*c\\(([^)]*)\\)"
  m <- regexec(pat, lines, perl = TRUE)
  hits <- regmatches(lines, m)
  raw <- vapply(hits, function(h) if (length(h) >= 2) h[2] else NA_character_, character(1))
  raw <- raw[!is.na(raw)]
  # From the captured (...) extract all quoted tokens
  unlist(lapply(raw, .extract_quoted), use.names = FALSE)
}

# Main entry: give it files or directories
scan_translation_keys <- function(paths) {
  files <- unlist(lapply(paths, function(p) {
    if (dir.exists(p)) list.files(p, pattern = "\\.[Rr]$", full.names = TRUE, recursive = TRUE)
    else if (file.exists(p)) p else character(0)
  }), use.names = FALSE)

  keys <- character(0)
  for (f in unique(files)) {
    lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
    keys <- c(keys, .parse_t_calls(lines), .parse_label_keys(lines))
  }
  sort(unique(keys))
}

# Convenience checker you can call anywhere:
check_all_translation_keys <- function(
  translations_rds = "/srv/shiny-server/policymakers/Policymakers_translations.rds",
  paths = c("/srv/shiny-server/policymakers/app.R",
            "/srv/shiny-server/utils.R",
            "/srv/shiny-server/global.R")
) {
  tr <- readRDS(translations_rds)
  if (!"unique_ID" %in% names(tr)) stop("translations need a 'unique_ID' column")
  used <- scan_translation_keys(paths)
  missing <- setdiff(used, tr$unique_ID)
  if (length(missing)) {
    warning("Missing translation keys:\n", paste(missing, collapse = "\n"))
  } else {
    message("✅ All translation keys found (", length(used), ").")
  }
  invisible(list(used = used, missing = missing))
}


#!/usr/bin/env Rscript
options(warn = 1)

# --- set inputs (or parse args) ---
args   <- commandArgs(trailingOnly = TRUE)
tr_path <- if (length(args) >= 1) args[1] else "/srv/shiny-server/policymakers/Policymakers_translations.rds"
paths  <- if (length(args) >= 2) strsplit(args[2], ",")[[1]] else c(
  "/srv/shiny-server/policymakers/app.R",
  "/srv/shiny-server/utils.R",
  "/srv/shiny-server/global.R"
)

# --- functions: scan_translation_keys(), check_all_translation_keys() ---
# (define them above, or source("/srv/shiny-server/utils.R") if you put them there)

# --- runner (your block) ---
res <- check_all_translation_keys(tr_path, paths)

cat(sprintf("✅ Used keys: %d\n", length(res$used)))
cat(sprintf("⚠️ Missing keys: %d\n", length(res$missing)))

if (length(res$missing)) {
  cat("Missing keys:\n", paste(res$missing, collapse = "\n"), "\n", sep = "")
  quit(status = 1)  # fail loudly for CI/deploy
} else {
  cat("All translation keys found!\n")
  quit(status = 0)
}
