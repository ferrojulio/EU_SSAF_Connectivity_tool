library(DBI)
library(RPostgres)

host <- Sys.getenv("PGHOST")
port <- Sys.getenv("PGPORT")
user <- Sys.getenv("PGUSER")
password <- Sys.getenv("PGPASSWORD")
dbname <- Sys.getenv("PGDATABASE")

conn <- tryCatch({
  DBI::dbConnect(RPostgres::Postgres(),
                 host = host, port = port,
                 user = user, password = password,
                 dbname = dbname)
}, error = function(e) {
  message("❌ DB connection failed: ", e$message)
  return(NULL)
})

if (!is.null(conn)) {
  on.exit(DBI::dbDisconnect(conn))
  
  query <- "CREATE TABLE IF NOT EXISTS landmanagers_responses (
              session_token TEXT PRIMARY KEY,
              consent BOOLEAN,
              timestamp_start TEXT,
              page1_timestamp TEXT
            );"
            
  tryCatch({
    DBI::dbExecute(conn, query)
    message("✅ Table 'landmanagers_responses' created successfully (if it didn't exist).")
  }, error = function(e) {
    message("❌ Table creation failed: ", e$message)
  })
}
