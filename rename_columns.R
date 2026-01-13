library(DBI)
library(RPostgres)

host <- "209.38.208.172"
port <- "5432"
user <- "juliokdigging"
password <- Sys.getenv("PGPASSWORD")
dbname <- "bassetdb"

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
  
  table_name <- "farmer_responses"
  
  existing_cols <- DBI::dbListFields(conn, table_name)
  
  rename_column_sql <- function(old_name, new_name) {
    sprintf("ALTER TABLE %s RENAME COLUMN %s TO %s", 
            DBI::dbQuoteIdentifier(conn, table_name), 
            DBI::dbQuoteIdentifier(conn, old_name), 
            DBI::dbQuoteIdentifier(conn, new_name))
  }
  
  tryCatch({
    if ("number_soil_types" %in% existing_cols) {
      dbExecute(conn, rename_column_sql("number_soil_types", "numberSoilTypes"))
    }
    if ("defense_validation_comment" %in% existing_cols) {
      dbExecute(conn, rename_column_sql("defense_validation_comment", "defense_validation2"))
    }
    if ("language" %in% existing_cols) {
      dbExecute(conn, rename_column_sql("language", "lang"))
    }
    
    message("✅ Columns renamed successfully.")
  }, error = function(e) {
    message("❌ Failed to rename columns: ", e$message)
  })
}