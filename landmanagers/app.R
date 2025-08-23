# app_landmanagers.R — Multilingual ####

library(shiny)
library(leaflet)
library(dplyr)
library(DBI)
library(RSQLite)
library(htmltools)
library(RPostgres)

source("/srv/shiny-server/utils.R", local = TRUE)  
source("/srv/shiny-server/global.R", local = FALSE)
source("/srv/shiny-server/shared_app_components.R", local = TRUE)

source("/srv/shiny-server/landmanagers/LandmanagersScoring.R", local = TRUE)

t <- function(key, lang = "French") {
  t_original(key, lang, translations_df = translations)
}

set_t_function(t)

try({
  test_val <- .t_global("nav_start", "French") # Using a common key for testing
  message("✅ .t_global test passed: ", test_val)
}, silent = TRUE)

# ===== DATABASE SETUP =====
dbtable <- "landmanagers_responses"

host <- Sys.getenv("PGHOST")
port <- Sys.getenv("PGPORT")
user <- Sys.getenv("PGUSER")
password <- Sys.getenv("PGPASSWORD")
dbname <- Sys.getenv("PGDATABASE")

ensure_columns_exist <- function(conn, table, data) {
  existing_cols <- DBI::dbListFields(conn, table)
  new_cols <- setdiff(names(data), existing_cols)
  for (col in new_cols) {
    # Infer type: numeric → DOUBLE PRECISION, else TEXT
    type <- if (is.numeric(data[[col]])) "DOUBLE PRECISION" else "TEXT"
    alter_sql <- sprintf(
      "ALTER TABLE %s ADD COLUMN %s %s",
      DBI::dbQuoteIdentifier(conn, table),
      DBI::dbQuoteIdentifier(conn, col),
      type
    )
    DBI::dbExecute(conn, alter_sql)
    message("➕ Added column ", col, " (", type, ")")
  }
}

# SaveData0: raw multilingual inputs
saveRawInputs <- function(data, table = dbtable) {
  conn <- tryCatch({
    if (mode == "online") {
      DBI::dbConnect(RPostgres::Postgres(),
                     host = host, port = port,
                     user = user, password = password,
                     dbname = dbname,
                     options = "-c search_path=public")
    } else {
      DBI::dbConnect(RSQLite::SQLite(), "TestDatabase.sqlite")
    }
  }, error = function(e) {
    message("❌ DB connection failed: ", e$message)
    return(NULL)
  })
  
  if (is.null(conn)) return(NULL)
  on.exit(DBI::dbDisconnect(conn))
  
  ensure_columns_exist(conn, table, data)
  
  # Ensure UTF-8 encoding for character data before sending to DB
  data[] <- lapply(data, function(x) {
    if (is.character(x)) {
      iconv(x, from = "", to = "UTF-8") # Ensure UTF-8
    } else {
      x
    }
  })
  
  fields <- names(data)
  placeholders <- paste0("$", seq_along(fields), collapse = ", ")
  
  values_list <- unname(as.list(data[1, , drop = FALSE]))
  
  quoted_fields <- as.character(DBI::dbQuoteIdentifier(conn, fields))
  
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES (%s) ON CONFLICT (session_token) DO UPDATE SET %s",
    DBI::dbQuoteIdentifier(conn, table),
    paste(quoted_fields, collapse = ", "),
    placeholders,
    paste(sprintf("%s = EXCLUDED.%s", quoted_fields, quoted_fields), collapse = ", ")
  )
  
  tryCatch({
    DBI::dbExecute(conn, query, params = values_list)
  }, error = function(e) {
    log_content <- paste(
      Sys.time(),
      "\nQuery failed:", query,
      "\nError:", e$message,
      "\nData:", paste(names(values_list), "=", sapply(values_list, function(v) if(is.character(v)) paste0("'",v,"'") else v), collapse=", "),
      "\n\n"
    )
    cat(log_content,
        file = "debug_log.txt", append = TRUE)
    showNotification(paste("DB write error:", e$message), type = "error")
  })
}

# SaveData1: simplified numerical scoring for downstream analysis
saveScoringData <- function(data, table = dbtable) {
  if (nrow(data) == 0 || !"session_token" %in% names(data)) return(NULL)
  
  conn <- tryCatch({
    if (mode == "online") {
      DBI::dbConnect(RPostgres::Postgres(),
                     host = host, port = port,
                     user = user, password = password,
                     dbname = dbname,
                     options = "-c search_path=public")
    } else {
      DBI::dbConnect(RSQLite::SQLite(), "TestDatabase.sqlite")
    }
  }, error = function(e) {
    message("❌ DB connection failed: ", e$message)
    return(NULL)
  })
  
  if (is.null(conn)) return(NULL)
  on.exit(DBI::dbDisconnect(conn))
  
  ensure_columns_exist(conn, table, data)
  
  data_sanitized <- as.data.frame(lapply(data, function(x) {
    if (is.character(x)) {
      iconv(x, from = "", to = "UTF-8")
    } else {
      x
    }
  }), stringsAsFactors = FALSE)
  names(data_sanitized) <- names(data)
  
  token_value_for_where <- data_sanitized$session_token[1]
  
  fields_to_update <- setdiff(names(data_sanitized), "session_token")
  if (length(fields_to_update) == 0) return(NULL)
  
  all_fields <- names(data_sanitized)
  placeholders <- paste0("$", seq_along(all_fields), collapse = ", ")
  
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES (%s) ON CONFLICT (session_token) DO UPDATE SET %s",
    table,
    paste(all_fields, collapse = ", "),
    placeholders,
    paste(sprintf("%s = EXCLUDED.%s", setdiff(all_fields, "session_token"), setdiff(all_fields, "session_token")), collapse = ", ")
  )
  
  values_list <- unname(as.list(data_sanitized[1, all_fields, drop = FALSE]))
  
  tryCatch({
    DBI::dbExecute(conn, query, params = values_list)
  }, error = function(e) {
    log_content <- paste(
      Sys.time(),
      "\nQuery failed:", query,
      "\nError:", e$message,
      "\nData for SET:", paste(fields_to_update, "=", sapply(data_sanitized[1, fields_to_update, drop=FALSE], function(v) if(is.character(v)) paste0("'",v,"'") else v), collapse=", "),
      "\nToken for WHERE:", token_value_for_where,
      "\n\n"
    )
    cat(log_content,
        file = "debug_log.txt", append = TRUE)
    showNotification(paste("DB write error:", e$message), type = "error")
  })
}

# ===== UI =====
ui <- fluidPage(
  common_head_ui("landmanager"),
  
  common_top_bar_ui(),
  
  # MAIN UI
  div(
    class = "container-fluid",
    fluidRow(
      column(
        width = 10,
        offset = 1,
        br(),
        uiOutput("dynamicTitle"),
        uiOutput("progressUI"),
        uiOutput("mainUI")
      )
    )),
  common_footer_ui()
)

# ===== SERVER =====
server <- function(input, output, session) {
  # Setup common app session and progress bar
  common_rvs <- setup_app_session_and_progress(input, output, session, dbtable_name = dbtable, appPrefix = "landmanager", total_pages_reactive = reactive(2)) # total_pages_reactive set to 2 for landmanagers
  session_token <- common_rvs$session_token
  currentPage <- common_rvs$currentPage

  observeEvent(input$btnStart, {
    lang <- isolate(input$lang %||% "French")
    if (!isTRUE(input$consent)) {
      showNotification(t("missing_consent", lang), type = "error", duration = 5)
      return()
    }
    
    # Save consent data
    saveRawInputs(data.frame(
      session_token = session_token(),
      consent = input$consent,
      timestamp_start = format(Sys.time(), tz = "UTC", usetz = FALSE),
      stringsAsFactors = FALSE
    ))
    
    currentPage(currentPage() + 1)
  })

  observeEvent(input$nav_next_btn, {
    # This is for page 1 to 2
    if(currentPage() == 1) {
        # Assuming there are some inputs on page 1 to save
        # For now, let's just save a timestamp
        saveRawInputs(data.frame(
            session_token = session_token(),
            page1_timestamp = format(Sys.time(), tz = "UTC", usetz = FALSE),
            stringsAsFactors = FALSE
        ))
    }
    currentPage(currentPage() + 1)
  })

  observeEvent(input$nav_back_btn, {
    currentPage(currentPage() - 1)
  })

  # Exclusive checkbox logic for landmanagers app
  observeEvent(input$supply_newconcepts, {
    current_selection <- input$supply_newconcepts
    exclusive_value <- "supply_newconcepts_A5"

    if (exclusive_value %in% current_selection && length(current_selection) > 1) {
      updateCheckboxGroupInput(session, "supply_newconcepts", selected = exclusive_value)
    }
  }, ignoreNULL = FALSE)

  observeEvent(input$food_newconcepts, {
    current_selection <- input$food_newconcepts
    exclusive_value <- "food_newconcepts_A5"

    if (exclusive_value %in% current_selection && length(current_selection) > 1) {
      updateCheckboxGroupInput(session, "food_newconcepts", selected = exclusive_value)
    }
  }, ignoreNULL = FALSE)

  observeEvent(input$econ_newconcepts, {
    current_selection <- input$econ_newconcepts
    exclusive_value <- "econ_newconcepts_A5"

    if (exclusive_value %in% current_selection && length(current_selection) > 1) {
      updateCheckboxGroupInput(session, "econ_newconcepts", selected = exclusive_value)
    }
  }, ignoreNULL = FALSE)

  observeEvent(input$intSec_newconcepts, {
    current_selection <- input$intSec_newconcepts
    exclusive_value <- "intSec_newconcepts_A5"

    if (exclusive_value %in% current_selection && length(current_selection) > 1) {
      updateCheckboxGroupInput(session, "intSec_newconcepts", selected = exclusive_value)
    }
  }, ignoreNULL = FALSE)

  observeEvent(input$defense_newconcepts, {
    current_selection <- input$defense_newconcepts
    exclusive_value <- "defense_newconcepts_A5"

    if (exclusive_value %in% current_selection && length(current_selection) > 1) {
      updateCheckboxGroupInput(session, "defense_newconcepts", selected = exclusive_value)
    }
  }, ignoreNULL = FALSE)

  observeEvent(input$E_K, {
    current_selection <- input$E_K
    exclusive_value <- "E_K_A5"

    if (exclusive_value %in% current_selection && length(current_selection) > 1) {
      updateCheckboxGroupInput(session, "E_K", selected = exclusive_value)
    }
  }, ignoreNULL = FALSE)

  observeEvent(input$A_K, {
    current_selection <- input$A_K
    exclusive_value <- "A_K_A5"

    if (exclusive_value %in% current_selection && length(current_selection) > 1) {
      updateCheckboxGroupInput(session, "A_K", selected = exclusive_value)
    }
  }, ignoreNULL = FALSE)

  observeEvent(input$SD_K, {
    current_selection <- input$SD_K
    exclusive_value <- "SD_K_A5"

    if (exclusive_value %in% current_selection && length(current_selection) > 1) {
      updateCheckboxGroupInput(session, "SD_K", selected = exclusive_value)
    }
  }, ignoreNULL = FALSE)

  observeEvent(input$S_K, {
    current_selection <- input$S_K
    exclusive_value <- "S_K_A5"

    if (exclusive_value %in% current_selection && length(current_selection) > 1) {
      updateCheckboxGroupInput(session, "S_K", selected = exclusive_value)
    }
  }, ignoreNULL = FALSE)

  observeEvent(input$HL_K, {
    current_selection <- input$HL_K
    exclusive_value <- "HL_K_A5"

    if (exclusive_value %in% current_selection && length(current_selection) > 1) {
      updateCheckboxGroupInput(session, "HL_K", selected = exclusive_value)
    }
  }, ignoreNULL = FALSE)

  observeEvent(input$NM_K, {
    current_selection <- input$NM_K
    exclusive_value <- "NM_K_A5"

    if (exclusive_value %in% current_selection && length(current_selection) > 1) {
      updateCheckboxGroupInput(session, "NM_K", selected = exclusive_value)
    }
  }, ignoreNULL = FALSE)

  observeEvent(input$SW_K, {
    current_selection <- input$SW_K
    exclusive_value <- "SW_K_A5"

    if (exclusive_value %in% current_selection && length(current_selection) > 1) {
      updateCheckboxGroupInput(session, "SW_K", selected = exclusive_value)
    }
  }, ignoreNULL = FALSE)

  observeEvent(input$DC_K, {
    current_selection <- input$DC_K
    exclusive_value <- "DC_K_A5"

    if (exclusive_value %in% current_selection && length(current_selection) > 1) {
      updateCheckboxGroupInput(session, "DC_K", selected = exclusive_value)
    }
  }, ignoreNULL = FALSE)

  output$mainUI <- renderUI({
    p <- currentPage()
    lang <- isolate(input$lang %||% "French")

    if (p == 0) {
      return(
        fluidPage(
          titlePanel(t("page0_intro_header", lang)),
          HTML(sprintf("<p>%s</p>", t("page0_intro_text", lang))),
          checkboxInput("consent", t("consent_text", lang), value = FALSE),
          actionButton("btnStart", t("nav_start", lang), class = "btn-primary")
        )
      )
    } else if (p == 1) {
      return(
        fluidPage(
          h3(t("landmanagers_page1_header", lang)),
          p(t("landmanagers_page1_text", lang)),
          actionButton("nav_back_btn", t("nav_back", lang)),
          actionButton("nav_next_btn", t("nav_next", lang))
        )
      )
    } else {
      return(
        fluidPage(
          h3(t("landmanagers_end_header", lang)),
          p(t("landmanagers_end_text", lang)),
          actionButton("nav_back_btn", t("nav_back", lang))
        )
      )
    }
  })
}

shinyApp(ui = ui, server = server)
