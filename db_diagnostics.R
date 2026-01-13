
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
  
  if (dbExistsTable(conn, table_name)) {
    message(paste("✅ Table", table_name, "exists."))
    
    query <- paste0("SELECT column_name, data_type FROM information_schema.columns WHERE table_name = '", table_name, "'")
    
    tryCatch({
      columns_info <- DBI::dbGetQuery(conn, query)
      print(columns_info)
    }, error = function(e) {
      message("❌ Failed to retrieve column information: ", e$message)
    })
  } else {
    message(paste("❌ Table", table_name, "does not exist."))
  }
}
