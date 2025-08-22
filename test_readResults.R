#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(DBI); library(RPostgres); library(RSQLite)
})

# --- Config from environment ---
host <- Sys.getenv("PGHOST"); port <- Sys.getenv("PGPORT")
user <- Sys.getenv("PGUSER"); password <- Sys.getenv("PGPASSWORD")
dbname <- Sys.getenv("PGDATABASE")
mode <- "online"  # set "offline" to test SQLite locally
table <- "policymakers_responses"

# --- Helpers ---
`%||%` <- function(a,b) if (is.null(a) || length(a)==0 || a=="") b else a

connect_to_db <- function(mode = "online") {
  tryCatch({
    if (identical(mode, "online")) {
      dbConnect(RPostgres::Postgres(),
        host=host, port=port, user=user, password=password,
        dbname=dbname, options="-c search_path=public")
    } else {
      dbConnect(RSQLite::SQLite(), "TestDatabase.sqlite")
    }
  }, error=function(e){ message("❌ DB connection failed: ", e$message); NULL })
}

readResults <- function(token_value, table, conn) {
  sql <- sprintf("SELECT * FROM %s WHERE %s = $1",
                 dbQuoteIdentifier(conn, table),
                 dbQuoteIdentifier(conn, "session_token"))
  out <- tryCatch(dbGetQuery(conn, sql, params=list(token_value)),
                  error=function(e){ message("❌ DB read error: ", e$message); data.frame() })
  if (nrow(out)) out[] <- lapply(out, function(x) if (is.character(x)) iconv(x, "", "UTF-8") else x)
  out
}

# --- Main ---
args <- commandArgs(trailingOnly=TRUE)
token <- if (length(args) >= 1 && nzchar(args[[1]])) args[[1]] else sprintf("tok_test_%s", format(Sys.time(), "%Y%m%d%H%M%S"))

conn <- connect_to_db(mode)
stopifnot(!is.null(conn))
on.exit(dbDisconnect(conn), add=TRUE)

# Ensure table exists (minimal schema; your app will add columns as needed)
dbExecute(conn, sprintf(
  "CREATE TABLE IF NOT EXISTS %s (
     session_token TEXT PRIMARY KEY,
     timestamp     TIMESTAMPTZ DEFAULT now(),
     language      TEXT,
     written_response TEXT
   )", dbQuoteIdentifier(conn, table)))

# Upsert a test row with special chars
row <- list(session_token=token,
            language="French",
            written_response="carets ^ quotes ' double \" emoji 🌱")
cols <- names(row)
col_q <- dbQuoteIdentifier(conn, cols)
placeholders <- paste0("$", seq_along(cols), collapse=", ")
set_clause <- paste(sprintf("%s = EXCLUDED.%s", col_q, col_q), collapse=", ")
sql_upsert <- sprintf(
  "INSERT INTO %s (%s) VALUES (%s)
   ON CONFLICT (%s) DO UPDATE SET %s",
  dbQuoteIdentifier(conn, table),
  paste(col_q, collapse=", "), placeholders,
  dbQuoteIdentifier(conn, "session_token"), set_clause)

dbExecute(conn, sql_upsert, params=unname(row))

# Read it back
res <- readResults(token, table, conn)

cat("\n=== TEST SUMMARY ===\n")
cat("Token: ", token, "\n", sep="")
cat("Rows returned: ", nrow(res), "\n", sep="")
if (nrow(res)) {
  print(str(res, max.level=1))
  cat("\nHead:\n"); print(utils::head(res, 1))
} else {
  cat("No row found.\n")
}
