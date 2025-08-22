# /srv/shiny-server/health/app.R
library(shiny)
library(DBI)

ok <- function(msg="OK") paste0("OK: ", msg)
fail <- function(msg) paste0("FAIL: ", msg)

ui <- fluidPage(verbatimTextOutput("result"))
server <- function(input, output, session){
  status <- tryCatch({
    # 1) translations (use the same path pattern you already use)
    tr_ok <- file.exists("/srv/shiny-server/policymakers/Policymakers_translations.rds")
    if (!tr_ok) stop("translations RDS missing")

    # 2) DB light check (reuse env vars already used by app)
    mode <- Sys.getenv("HEALTH_MODE", unset = "online") # optional override
    if (identical(mode, "online")) {
      con <- DBI::dbConnect(RPostgres::Postgres(),
        host = Sys.getenv("PGHOST"), port = Sys.getenv("PGPORT"),
        user = Sys.getenv("PGUSER"), password = Sys.getenv("PGPASSWORD"),
        dbname = Sys.getenv("PGDATABASE"))
      on.exit(try(DBI::dbDisconnect(con), silent = TRUE))
      DBI::dbGetQuery(con, "SELECT 1")
    } else {
      con <- DBI::dbConnect(RSQLite::SQLite(), "TestDatabase.sqlite")
      on.exit(try(DBI::dbDisconnect(con), silent = TRUE))
      DBI::dbGetQuery(con, "SELECT 1")
    }
    ok()
  }, error=function(e) {print(e); fail(e$message)})

  output$result <- renderText(status)
}
shinyApp(ui, server)
