# app_Farmers.R — Multilingual ####
# 


library(shiny)
library(leaflet)
library(dplyr)
library(DBI)
library(RSQLite)
library(htmltools)
library(RPostgres)


library(readxl)

translations <<- read_excel("/srv/shiny-server/farmers/Farmers_translations.xlsx")

translations <- translations[!is.na(translations$unique_ID), ]

source("/srv/shiny-server/utils.R",local = TRUE)  


source("/srv/shiny-server/farmers/farmersScoring.R", local = TRUE)
source("/srv/shiny-server/shared_app_components.R", local = TRUE)

t <- function(key, lang = "French") {
  t_original(key, lang)
}

set_t_function(t)

try({
  test_val <- .t_global("A_K_Q", "French")
  message("✅ .t_global test passed: ", test_val)
}, silent = TRUE)





# ===== DATABASE SETUP =====
# 


dbtable <- "farmer_responses"



host <- Sys.getenv("PGHOST")
port <- Sys.getenv("PGPORT")
user <- Sys.getenv("PGUSER")
password <- Sys.getenv("PGPASSWORD")
dbname <- Sys.getenv("PGDATABASE")





csv_collapse <- function(x) if (is.null(x)) "" else paste(x, collapse = ";")

# Create table if missing
mode <- "online"

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
      # No need for gsub("'", "''", x) here, DBI handles it with parameterized queries
    } else {
      x
    }
  })
  
  fields <- names(data)
  placeholders <- paste0("$", seq_along(fields), collapse = ", ")
  
  
  # Convert data frame row to a list for params argument
  values_list <- unname(as.list(data[1, , drop = FALSE]))
  
  message("🔧 Fields to insert: ", paste(fields, collapse = ", "))
  message("🔧 Values: ", paste(unlist(values_list), collapse = ", "))
  
  # Construct UPSERT query (PostgreSQL)
  
  quoted_fields <- as.character(DBI::dbQuoteIdentifier(conn, fields))
  
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES (%s) ON CONFLICT (session_token) DO UPDATE SET %s",
    DBI::dbQuoteIdentifier(conn, table),
    paste(quoted_fields, collapse = ", "),
    placeholders,
    paste(sprintf("%s = EXCLUDED.%s", quoted_fields, quoted_fields), collapse = ", ")
  )
  
  log_content <- paste0(
    Sys.time(),
    "\n🔍 Constructed UPSERT query:\n", query,
    "\n🔢 Fields: ", paste(fields, collapse = ", "),
    "\n📦 Values: ", paste(unlist(values_list), collapse = " | "),
    "\n\n"
  )
  
  cat(log_content, file = "/srv/shiny-server/farmers/debug_sql_log.txt", append = TRUE)
  
  
  tryCatch({
    DBI::dbExecute(conn, query, params = values_list) # Pass values as a list
  }, error = function(e) {
    # Log the actual data that failed, be mindful of sensitive data in logs
    log_content <- paste(
      Sys.time(),
      "\nQuery failed:", query,
      "\nError:", e$message,
      "\nData:", paste(names(values_list), "=", sapply(values_list, function(v) if(is.character(v)) paste0("'",v,"'") else v), collapse=", "),
      "\n\n"
    )
    cat(log_content,
        file = "/srv/shiny-server/farmers/event_log.txt", append = TRUE)
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
  
  # Ensure UTF-8 encoding for character data
  data_sanitized <- as.data.frame(lapply(data, function(x) {
    if (is.character(x)) {
      iconv(x, from = "", to = "UTF-8")
    } else {
      x
    }
  }), stringsAsFactors = FALSE)
  # Preserve original names if as.data.frame messes them up
  names(data_sanitized) <- names(data)
  
  
  token_value_for_where <- data_sanitized$session_token[1]
  
  fields_to_update <- setdiff(names(data_sanitized), "session_token")
  if (length(fields_to_update) == 0) return(NULL)
  
  set_clause <- paste0(fields_to_update, " = $", seq_along(fields_to_update), collapse = ", ")
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES (%s)
   ON CONFLICT (session_token) DO UPDATE SET %s",
    table,
    paste(fields_to_update, collapse = ", "),
    paste0("$", seq_along(fields_to_update), collapse = ", "),
    paste(sprintf("%s = EXCLUDED.%s", fields_to_update, fields_to_update), collapse = ", ")
  )
  
  
  # Include session_token in both fields and values
  all_fields <- names(data_sanitized)
  placeholders <- paste0("$", seq_along(all_fields), collapse = ", ")
  
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES (%s)
   ON CONFLICT (session_token) DO UPDATE SET %s",
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
        file = "/srv/shiny-server/farmers/event_log.txt", append = TRUE)
    showNotification(paste("DB write error:", e$message), type = "error")
  })
}



readResults <- function(token_value = session_token(), table = dbtable) {
  if (is.null(token_value) || token_value == "") return(NULL)
  
  conn <- tryCatch({
    if (mode == "online") {
      DBI::dbConnect(RPostgres::Postgres(),
                     host = host, port = port,
                     user = user, password = password,
                     dbname = dbname)
    } else {
      DBI::dbConnect(RSQLite::SQLite(), "TestDatabase.sqlite")
    }
  }, error = function(e) {
    message("❌ DB connection failed: ", e$message)
    return(NULL)
  })
  
  if (is.null(conn)) return(NULL)
  on.exit(DBI::dbDisconnect(conn))
  
  query <- sprintf("SELECT * FROM %s WHERE session_token = '%s'", table, token_value)
  
  result <- tryCatch({
    DBI::dbGetQuery(conn, query)
  }, error = function(e) {
    message("❌ DB read error: ", e$message)
    return(NULL)
  })
  
  result[] <- lapply(result, function(x) {
    if (is.character(x)) iconv(x, from = "", to = "UTF-8") else x
  })
  
  return(result)
}





safe_isolate_string <- function(x) {
  val <- isolate(x)
  if (is.null(val) || is.na(val)) return("")
  as.character(val)
}






# ===== UI =====
ui <- fluidPage(
  tags$head(
    tags$link(rel = "icon", href = "favicon.ico", type = "image/x-icon"),
    tags$link(rel = "stylesheet", href = "app.css"),
    tags$script(src = "app.js"),
    tags$script(HTML("window.appPrefix = 'farmers';")),
    tags$script(src = "exclusive-checkbox.js")
  ),
  
  # LOGO
  div(
    class = "top-bar",
    
    ## LEFT  – language selector
    div(
      style = "min-width:160px;",    # keeps width steady as UI re-renders
      selectInput("lang",
                  label = "Langue / Language / Idioma",
                  choices = c("French", "Spanish", "English"),
                  selected = "French",
                  width   = "160px")
    ),
    
    ## RIGHT – logo pair
    div(
      class = "logo-bar",
      tags$img(src = "Logos.png",  alt = "USYD and ESDR3C Lab Logos")
    )
  ),
  
  # MAIN UI
  div(
    class = "container-fluid",  # full-width background
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
  div(
    class = "footer",
    div(class = "page-wrapper", HTML('
    <p style="margin-bottom: 5px;">
      <a href="https://creativecommons.org/licenses/by/4.0/" target="_blank" rel="noopener noreferrer">
        <img src="https://licensebuttons.net/l/by/4.0/88x31.png" alt="CC BY 4.0" style="height: 31px; vertical-align: middle;">
      </a>
    </p>
    <p style="margin-top: 5px;">
      Application designed by Celine Basset and coded by Julio Pachon.
    </p>
  '))
  )
  
  
)



# ===== SERVER =====
server <- function(input, output, session) {
  try(cat(paste(Sys.time(), "BOOT farmers"), file = "/srv/shiny-server/farmers/debug_sql_log.txt", append = TRUE), silent = TRUE)


  commune_centroids_reactive <- reactive({
    req(input$country)
    country_file <- file.path("/srv/shiny-server/country_data", paste0(input$country, ".rds"))
    if (file.exists(country_file)) {
      readRDS(country_file)
    } else {
      NULL
    }
  })

  observe({
    if (!is.null(commune_centroids_reactive())) {
      message("📦 Commune centroids has ", nrow(commune_centroids_reactive()), " rows")
    }
  })
  #observeRegionSelection(input, output, session, lang = input$lang %||% "French")
  
  once <- reactiveVal(TRUE) 
  # Setup session token and page
  # Use shared session management
  shared_session_data <- setup_app_session_and_progress(
    input, output, session, 
    dbtable_name = "farmer_responses", 
    appPrefix = "farmers",
    total_pages_reactive = reactive({ 18 }) # Assuming 18 total pages for farmers app
  )
  session_token <- shared_session_data$session_token
  currentPage <- shared_session_data$currentPage
  
  
  
  
  observeEvent(input$restoredPage, {
    lang <- input$lang %||% "French"
    restoredPage <- suppressWarnings(as.numeric(input$restoredPage))
    if (!is.na(restoredPage)) currentPage(restoredPage)
  })
  #### observeEvent(currentPage()####
  observeEvent(currentPage(), {
    lang <- input$lang %||% "French"
    session$sendCustomMessage("updateCurrentPage", currentPage())
    
    session$sendCustomMessage("scrollTop", list())
    # Send default if input$lang not yet set
    user_lang <- if (!is.null(input$lang)) input$lang else "French"
    session$sendCustomMessage("disconnectedAlert", t("disconnected_alert", user_lang))
    session$sendCustomMessage("pushPageState", currentPage())
  })
  observeEvent(input$browserBackPage, {
    if (!is.null(input$browserBackPage) && is.finite(input$browserBackPage)) {
      currentPage(as.numeric(input$browserBackPage))
    }
  })
  
  
  userCoords <- reactiveValues(lat = NULL, lon = NULL)
  
  
  
  
  
  
  ##Progress bar ####
  output$progressUI <- renderUI({
    p <- currentPage()
    total_pages <- 18
    
    if (p < 1 || p > total_pages) return(NULL)
    
    percent <- round((p - 1) / (total_pages - 1) * 100)
    
    div(class = "progress-wrapper",
        div(class = "progress",
            div(class = "progress-bar",
                role = "progressbar",
                style = paste0("width:", percent, "%;"))
        )
    )
  })
  
  
  
  
  # ---- Mapping Page state ----
  
  observe({
    lang <- input$lang %||% "French"
    
    # Reset location when user changes page or country/region
    if (currentPage() != 7) {
      userCoords$lat <- NULL
      userCoords$lon <- NULL
    }
  })
  
  # 
  output$region1_ui_new <- renderUI({
    req(input$country)
    commune_centroids <- commune_centroids_reactive()
    req(commune_centroids)
    regions1 <- sort(unique(commune_centroids$region1[commune_centroids$country == input$country]))
    
    # If input$region1 isn't in regions1, select first available (or NULL if none)
    default_region <- if (!is.null(input$region1) && input$region1 %in% regions1) input$region1 else if (length(regions1) > 0) regions1[1] else NULL
    
    selectInput(
      "region1",
      t("select_region1", input$lang),
      choices = regions1,
      selected = default_region
    )
  })
  # 
  output$region2_ui_new <- renderUI({
    req(input$country, input$region1)
    commune_centroids <- commune_centroids_reactive()
    req(commune_centroids)
    regions2 <- sort(unique(commune_centroids$region2[
      commune_centroids$country == input$country &
        commune_centroids$region1 == input$region1
    ]))
    sel <- if (!is.null(input$region2) && input$region2 %in% regions2) input$region2 else regions2[1]
    selectInput("region2", t("select_region2", input$lang), choices = regions2, selected = sel)
  })
  
  # 
  output$soilMap_new <- renderLeaflet({
    # Default view: whole country/region if selected, else global
    lat <- 20
    lon <- 0
    zoom <- 2
    # Try centering to region2 if set, then region1, then country
    req(input$country)
    commune_centroids <- commune_centroids_reactive()
    req(commune_centroids)
    filtered <- commune_centroids %>% filter(country == input$country)
    if (!is.null(input$region1) && input$region1 != "") {
      filtered <- filtered %>% filter(region1 == input$region1)
      zoom <- 6
    }
    if (!is.null(input$region2) && input$region2 != "") {
      filtered <- filtered %>% filter(region2 == input$region2)
      zoom <- 9
    }
    if (nrow(filtered) > 0) {
      lat <- mean(filtered$lat, na.rm = TRUE)
      lon <- mean(filtered$lon, na.rm = TRUE)
    }
    leaflet() %>%
      addProviderTiles("Esri.WorldStreetMap", group = "Street Map") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite Image") %>%
      addProviderTiles("CartoDB.PositronOnlyLabels", group = "Satellite Image") %>%
      
      setView(lng = lon, lat = lat, zoom = zoom)
  })
  
  
  
  observeEvent(input$soilMap_new_click, {
    lang <- input$lang %||% "French"
    
    click <- input$soilMap_new_click
    if (!is.null(click$lat) && !is.null(click$lng)) {
      userCoords$lat <- click$lat
      userCoords$lon <- click$lng
      leafletProxy("soilMap_new") %>%
        clearMarkers() %>%
        addMarkers(lng = userCoords$lon, lat = userCoords$lat)
    }
  })
  
  observeEvent(input$region2, {
    lang <- input$lang %||% "French"
    
    req(input$country, input$region1, input$region2)
    commune_centroids <- commune_centroids_reactive()
    req(commune_centroids)
    centroid <- commune_centroids %>%
      filter(country == input$country,
             region1 == input$region1,
             region2 == input$region2)
    if (nrow(centroid) == 1) {
      userCoords$lat <- centroid$lat
      userCoords$lon <- centroid$lon
      leafletProxy("soilMap_new") %>%
        clearMarkers() %>%
        addMarkers(lng = userCoords$lon, lat = userCoords$lat)
    }
  }, ignoreInit = TRUE)
  
  #
  output$selected_coords_new <- renderText({
    if (!is.null(userCoords$lat) && !is.null(userCoords$lon)) {
      paste0(t("selected_coords", input$lang), ": ",
             round(userCoords$lat, 4), ", ", round(userCoords$lon, 4))
    } else {
      t("prompt_select_on_map", input$lang)
    }
  })
  
  
  observeEvent(input$no_gps_button, {
    userCoords$lat <- -999
    userCoords$lon <- -999
    
    leafletProxy("soilMap_new") %>%
      clearMarkers()
    
    output$selected_coords_new <- renderText({
      t("location_none_clicked", input$lang)
    })
  })
  
  # 
  output$Soil_Type_Description_count_new <- renderText({
    desc <- input$soil_type_description %||% ""
    paste0(nchar(desc), " / 500")
  })
  # 
  # 
  ##mini Map####
  
  
  
  
  
  
  
  # Render multilingual UI
  output$mainUI <- renderUI({  
    p <- currentPage()
    
    #Page 0: Language and Consent #### 
    if (p == 0) {
      lang <- input$lang %||% "French"
      
      return(
        fluidPage(
          
          
          titlePanel(t("page0_intro_header", lang)),  
          h3(t("page0_intro_header2", lang)),
          h4(t("page0_intro_header3", lang)),
          HTML(sprintf("<p>%s</p>", t("page0_intro_text", lang))),
          h4(t("page0_intro_header4", lang)),
          tags$ul(
            tags$li(t("page0_info1", lang)),
            tags$li(t("page0_info2", lang)),
            tags$li(t("page0_info3", lang)),
            tags$li(t("page0_info4", lang)),
            tags$li(t("page0_info5", lang)),
            tags$li(t("page0_info6", lang)),
            tags$li(t("page0_info7", lang))
          ),
          HTML(sprintf("<p>%s</p>", t("page0_info8", lang))),
          checkboxInput("consent", t("consent_text", lang), value = FALSE),
          actionButton("btnStart", t("nav_start", lang), class = "btn-primary")
        )
      )
      
      
      
      
      ## PAGE 1: PRESENTATION #############################
    } else if(p == 1){
      
      lang <- input$lang %||% "French"
      
      fluidPage(
        h3(t("PRESENTATION_header", lang)),
        p(t("PRESENTATION_intro_text", lang)),
        
        radioButtons("role", t("role_Q", lang), 
                     choices = setNames(
                       c("farmer", "other"),
                       c(t("role_A1", lang), t("role_A2", lang))
                     ),
                     selected = isolate(input$role %||% character(0))
        ),
        
        conditionalPanel(
          condition = "input.role == 'other'",
          textInput("other_role", t("other_role_A3", lang), 
                    value = safe_isolate_string(input$other_role %||% ""))
        ),
        
        uiOutput("farmEnterprises_1"),
        
        conditionalPanel(
          condition = "input.farmEnterprises && input.farmEnterprises.includes('Autre (précisez)')",
          textInput("other_farmEnterprises", t("other_farmEnterprises", lang),
                    value = safe_isolate_string(input$other_farmEnterprises %||% ""))
        ),
        
        uiOutput("perceivedThreats_1"),
        
        conditionalPanel(
          condition = "input.perceivedThreats && input.perceivedThreats.includes('Autre (précisez)')", 
          textInput("other_perceivedThreats", t("other_perceivedThreats", lang),
                    value = safe_isolate_string(input$other_perceivedThreats %||% ""))
        ),
        
        tags$script(HTML(sprintf("initExclusiveGroup('%s', %s);", htmltools::htmlEscape("perceivedThreats"), jsonlite::toJSON("None of the above", auto_unbox = TRUE)))),
        actionButton("PRESENTATION_Back", t("nav_back", lang)),
        actionButton("PRESENTATION_Next", t("nav_next", lang))
      )
      
      ### PAGE 2 CHAINES D'APPRO ############################
    }   else if (p == 2) {
      tagList(   
        h3(t("Supply_header", input$lang)),
        
        tags$script(HTML(sprintf("initExclusiveGroup('%s', %s);", htmltools::htmlEscape("supply_newconcepts"), jsonlite::toJSON("supply_newconcepts_5", auto_unbox = TRUE)))),
        uiOutput("supply_newconcepts_1"),
        
        radioButtons("supply_approach", 
                     t("supply_approach_Q", input$lang),
                     choices = setNames(
                       paste0("supply_approach_", 1:4),
                       c(
                         t("supply_approach_A1", input$lang),
                         t("supply_approach_A2", input$lang),
                         t("supply_approach_A3", input$lang),
                         t("supply_approach_A4", input$lang)
                       )
                     ),
                     selected = input$supply_approach %||% character(0)
        ),
        
        radioButtons("supply_opinion", 
                     t("supply_opinion_Q", input$lang),
                     choices = setNames(
                       paste0("supply_opinion_", 1:4),
                       c(
                         t("supply_opinion_A1", input$lang),
                         t("supply_opinion_A2", input$lang),
                         t("supply_opinion_A3", input$lang),
                         t("supply_opinion_A4", input$lang)
                       )
                     ),
                     selected = input$supply_opinion %||% character(0)
        ),
        
        actionButton("Supply_Back", t("nav_back", input$lang)),
        actionButton("Supply_Next", t("nav_next", input$lang))
      )
    
      
      ### PAGE 3: SÉCURITÉ ALIMENTAIRE ############################
    }     else if (p == 3) {
      fluidPage(
        h3(t("food_header", input$lang)),
        
        # Disable others if "I'm familiar with all" is selected
        tags$script(HTML(sprintf("initExclusiveGroup('%s', %s);", htmltools::htmlEscape("food_newconcepts"), jsonlite::toJSON("food_newconcepts_5", auto_unbox = TRUE)))),
        
        
        
        
        
        radioButtons("food_approach",
                     t("food_approach_Q", input$lang),
                     choices = setNames(
                       paste0("food_approach_", 1:4),
                       c(
                         t("food_approach_A1", input$lang),
                         t("food_approach_A2", input$lang),
                         t("food_approach_A3", input$lang),
                         t("food_approach_A4", input$lang)
                       )
                     ),
                     selected = isolate(input$food_approach %||% character(0))
        ),
        
        radioButtons("food_opinion",
                     t("food_opinion_Q", input$lang),
                     choices = setNames(
                       paste0("food_opinion_", 1:4),
                       c(
                         t("food_opinion_A1", input$lang),
                         t("food_opinion_A2", input$lang),
                         t("food_opinion_A3", input$lang),
                         t("food_opinion_A4", input$lang)
                       )
                     ),
                     selected = isolate(input$food_opinion %||% character(0))
        ),
        
        
        actionButton("food_Back", t("nav_back", input$lang)),
        actionButton("food_Next", t("nav_next", input$lang))
      )
      ### PAGE 4: econ ############################
    }    else if(p == 4){
      fluidPage(
        h3(t("econ_header", input$lang)),
        
        tags$script(HTML(sprintf("initExclusiveGroup('%s', %s);", htmltools::htmlEscape("econ_newconcepts"), jsonlite::toJSON("econ_newconcepts_5", auto_unbox = TRUE)))),
        
        uiOutput("econ_newconcepts_1"),
        
        radioButtons("econ_approach", 
                     t("econ_approach_Q", input$lang),
                     choices = setNames(
                       paste0("econ_approach_", 1:4),
                       c(
                         t("econ_approach_A1", input$lang),
                         t("econ_approach_A2", input$lang),
                         t("econ_approach_A3", input$lang),
                         t("econ_approach_A4", input$lang)
                       )
                     ),
                     selected = isolate(input$econ_approach %||% character(0))
        ),
        
        radioButtons("econ_opinion", 
                     t("econ_opinion_Q", input$lang),
                     choices = setNames(
                       paste0("econ_opinion_", 1:4),
                       c(
                         t("econ_opinion_A1", input$lang),
                         t("econ_opinion_A2", input$lang),
                         t("econ_opinion_A3", input$lang),
                         t("econ_opinion_A4", input$lang)
                       )
                     ),
                     selected = isolate(input$econ_opinion %||% character(0))
        ),
        
        
        actionButton("econ_Back", t("nav_back", input$lang)),
        actionButton("econ_Next", t("nav_next", input$lang))
      )
      
      
      ### PAGE 5: SÉCURITÉ INTÉRIEURE ET ORDRE PUBLIC############################
    }     else if (p == 5) {
      fluidPage(
        h3(t("intSec_header", input$lang)),
        
        tags$script(HTML(sprintf("initExclusiveGroup('%s', %s);", htmltools::htmlEscape("intSec_newconcepts"), jsonlite::toJSON(c("intSec_newconcept_5"), auto_unbox = TRUE)))),

        checkboxGroupInput(
          "intSec_newconcepts",
          label = t("intSec_newconcepts_Q", input$lang),
          choices = character(0),
          selected = character(0)
        ),
        
        
        
        radioButtons("intSec_approach",
                     t("intSec_approach_Q", input$lang),
                     choices = setNames(
                       paste0("intSec_approach_", 1:4),
                       c(
                         t("intSec_approach_A1", input$lang),
                         t("intSec_approach_A2", input$lang),
                         t("intSec_approach_A3", input$lang),
                         t("intSec_approach_A4", input$lang)
                       )
                     ),
                     selected = isolate(input$intSec_approach %||% character(0))
        ),
        
        radioButtons("intSec_opinion",
                     t("intSec_opinion_Q", input$lang),
                     choices = setNames(
                       paste0("intSec_opinion_", 1:4),
                       c(
                         t("intSec_opinion_A1", input$lang),
                         t("intSec_opinion_A2", input$lang),
                         t("intSec_opinion_A3", input$lang),
                         t("intSec_opinion_A4", input$lang)
                       )
                     ),
                     selected = isolate(input$intSec_opinion %||% character(0))
        ),
        
        radioButtons("intSec_validation",
                     t("intSec_val_Q", input$lang),
                     choices = setNames(c("YES", "NO"),
                                        c(t("intSec_validation_A1", input$lang),
                                          t("intSec_validation_A2", input$lang))),
                     selected = isolate(input$intSec_validation %||% character(0))
        ),
        
        textAreaInput("intSec_validation_comment", 
                      t("intSec_validation_text", input$lang),
                      placeholder = t("comment_placeholder", input$lang),
                      width = "100%", height = "100px",
                      value = isolate(input$intSec_validation_comment %||% "")),
        
        actionButton("intSec_Back", t("nav_back", input$lang)),
        actionButton("intSec_Next", t("nav_next", input$lang))
      )
      
      ### PAGE 6: DÉFENSE NATIONALE ############################
    } 
    else if (p == 6) {
      fluidPage(
        h3(t("defense_header", input$lang)),
        
        tags$script(HTML(sprintf("initExclusiveGroup('%s', %s);", htmltools::htmlEscape("defense_newconcepts"), jsonlite::toJSON("defense_newconcepts_5", auto_unbox = TRUE)))),
        
        uiOutput("defense_newconcepts_1"),
        
        radioButtons("defense_approach",
                     t("defense_approach_Q", input$lang),
                     choices = setNames(
                       paste0("defense_approach_", 1:4),
                       c(
                         t("defense_approach_A1", input$lang),
                         t("defense_approach_A2", input$lang),
                         t("defense_approach_A3", input$lang),
                         t("defense_approach_A4", input$lang)
                       )
                     ),
                     selected = isolate(input$defense_approach %||% character(0))
        ),
        
        radioButtons("defense_opinion",
                     t("defense_opinion_Q", input$lang),
                     choices = setNames(
                       paste0("defense_opinion_", 1:4),
                       c(
                         t("defense_opinion_A1", input$lang),
                         t("defense_opinion_A2", input$lang),
                         t("defense_opinion_A3", input$lang),
                         t("defense_opinion_A4", input$lang)
                       )
                     ),
                     selected = isolate(input$defense_opinion %||% character(0))
        ),
        
        radioButtons("defense_validation",
                     t("defense_validation_Q", input$lang),
                     choices = setNames(c("YES", "NO"),
                                        c(t("yes", input$lang),
                                          t("no", input$lang))),
                     selected = isolate(input$defense_validation %||% character(0))
        ),
        
        textAreaInput("defense_validation2", 
                      label = t("defense_validation_text", input$lang),
                      placeholder = t("comment_placeholder", input$lang),
                      width = "100%", height = "100px",
                      value = isolate(input$commentaires %||% "")
        ),
        
        actionButton("defense_Back", t("nav_back", input$lang)),
        actionButton("defense_Next", t("nav_next", input$lang))
      )
      
      ##PAGE 7: map ####
      
    } else if (p == 7) {
      fluidPage(
        h3(t("map_header", input$lang)),
        
        br(),
        p(t("Soil_intro_text", input$lang)),
        
        br(),
        h4(t("Soil_focus_text", input$lang)),
        br(),
        radioButtons(
          "numberSoilTypes",
          t("numberSoilTypes_Q", input$lang),
          choices = c(
            "1" = 1,
            "2-3" = "2-3",
            "4+" = "4+"
          ),
          selected = character(0)  # or a default like 1
        ),
        
        br(),
        p(t("Map_instruction", input$lang)),
        br(),
        
        # Country selection
        selectInput("country", t("select_country", input$lang),
                    choices = sort(gsub(".rds", "", list.files("/srv/shiny-server/country_data"))), 
                    selected = isolate(input$country)),
        # Region 1 selection
        uiOutput("region1_ui_new"),
        # Region 2 selection
        uiOutput("region2_ui_new"),
        br(),
        br(),
        br(),
        leafletOutput("soilMap_new", height = 450),
        br(),
        actionButton("no_gps_button", t("no_gps_button_label", input$lang), class = "btn-outline-secondary"),
        
        textOutput("selected_coords_new"),
        br(),
        
        

        
        # # Soil type description
        textAreaInput(
          "soil_type_description",
          t("Soil_Type_Description_Q", input$lang),
          value = isolate(input$soil_type_description %||% ""),
          placeholder = t("comment_placeholder", input$lang),
          width = "100%", height = "100px"
        ),
        
        
        
        textOutput("Soil_Type_Description_count_new"),
        br(),
        actionButton("map_Back", t("nav_back", input$lang)),
        actionButton("map_Next", t("nav_next", input$lang))
      )
      ### PAGE 8: EROSION ############################
    }else if (p == 8) {
      res <- NULL
      desc <- ""
      # Only call inside renderUI, which is reactive
      try({
        res <- readResults(session_token())
        if (!is.null(res) && nrow(res) > 0 && "soil_type_description" %in% names(res)) {
          desc <- as.character(res$soil_type_description[1])
          if (is.na(desc) || is.null(desc)) desc <- ""
        }
      }, silent = TRUE)
      
      tagList(
        h3(t("E_header", input$lang)),
        
        
        if (!identical(desc, "") && nzchar(desc)) {
          div(
            style = "margin-bottom: 12px; color: #333;",
            sprintf("%s: %s", t("your_soil_description", input$lang), desc)
          )
        },
        
        tags$script(HTML(sprintf("initExclusiveGroup('%s', %s);", htmltools::htmlEscape("E_K"), jsonlite::toJSON("E_K_5", auto_unbox = TRUE)))),
        uiOutput("E_K_1"),
        radioButtons(
          "E_approach",
          t("E_approach_Q", input$lang),
          choices = setNames(
            paste0("E_approach_", 1:4),
            c(
              t("E_approach_A1", input$lang),
              t("E_approach_A2", input$lang),
              t("E_approach_A3", input$lang),
              t("E_approach_A4", input$lang)
            )
          ),
          selected = isolate(input$E_approach %||% character(0))
        ),
        radioButtons(
          "E_opinion",
          t("E_opinion_Q", input$lang),
          choices = setNames(
            paste0("E_opinion_", 1:4),
            c(
              t("E_opinion_A1", input$lang),
              t("E_opinion_A2", input$lang),
              t("E_opinion_A3", input$lang),
              t("E_opinion_A4", input$lang)
            )
          ),
          selected = isolate(input$E_opinion %||% character(0))
        ),
        actionButton("erosion_Back", t("nav_back", input$lang)),
        actionButton("erosion_Next", t("nav_next", input$lang))
      )
    }
    ### PAGE 9: ACIDITÉ DU SOL ############################
    
    
    else if (p == 9) {
      res <- NULL
      desc <- ""
      
      try({
        res <- readResults(session_token())
        if (!is.null(res) && nrow(res) > 0 && "soil_type_description" %in% names(res)) {
          desc <- as.character(res$soil_type_description[1])
          if (is.na(desc) || is.null(desc)) desc <- ""
        }
      }, silent = TRUE)
      
      tagList(
        h3(t("A_header", input$lang)),
        
        if (!identical(desc, "") && nzchar(desc)) {
          div(
            style = "margin-bottom: 12px; color: #333;",
            sprintf("%s: %s", t("your_soil_description", input$lang), desc)
          )
        },
        
        
        tags$script(HTML(sprintf("initExclusiveGroup('%s', %s);", htmltools::htmlEscape("A_K"), jsonlite::toJSON("A_K_5", auto_unbox = TRUE)))),
        uiOutput("A_K_1"),
        
        radioButtons("A_approach",
                     t("A_approach_Q", input$lang),
                     choices = setNames(
                       paste0("A_approach_", 1:4),
                       sapply(paste0("A_approach_A", 1:4), t, lang = input$lang)
                     ),
                     selected = isolate(input$A_approach %||% character(0))
        ),
        
        radioButtons("A_opinion",
                     t("A_opinion_Q", input$lang),
                     choices = setNames(
                       paste0("A_opinion_", 1:4),
                       sapply(paste0("A_opinion_A", 1:4), t, lang = input$lang)
                     ),
                     selected = isolate(input$A_opinion %||% character(0))
        ),
        
        
        
        actionButton("acid_Back", t("nav_back", input$lang)),
        actionButton("acid_Next", t("nav_next", input$lang))
      )
      
      ### PAGE 10: STRUCTURE DU SOL############################
    } 
    else if (p == 10) {
      res <- NULL
      desc <- ""
      
      try({
        res <- readResults(session_token())
        if (!is.null(res) && nrow(res) > 0 && "soil_type_description" %in% names(res)) {
          desc <- as.character(res$soil_type_description[1])
          if (is.na(desc) || is.null(desc)) desc <- ""
        }
      }, silent = TRUE)
      
      tagList(
        h3(t("SD_K_header", input$lang)),
        
        if (!identical(desc, "") && nzchar(desc)) {
          div(
            style = "margin-bottom: 12px; color: #333;",
            sprintf("%s: %s", t("your_soil_description", input$lang), desc)
          )
        },
        
        
        tags$script(HTML(sprintf("initExclusiveGroup('%s', %s);", htmltools::htmlEscape("SD_K"), jsonlite::toJSON("SD_K_A5", auto_unbox = TRUE)))),
        
        uiOutput("SD_K_1"),
        
        radioButtons("SD_approach",
                     t("SD_approach", input$lang),
                     choices = setNames(
                       paste0("SD_approach_A", 1:4),
                       c(
                         t("SD_approach_A1", input$lang),
                         t("SD_approach_A2", input$lang),
                         t("SD_approach_A3", input$lang),
                         t("SD_approach_A4", input$lang)
                       )
                     ),
                     selected = isolate(input$SD_approach %||% character(0))
        ),
        
        radioButtons("SD_opinion",
                     t("SD_opinion", input$lang),
                     choices = setNames(
                       paste0("SD_opinion_A", 1:4),
                       c(
                         t("SD_opinion_A1", input$lang),
                         t("SD_opinion_A2", input$lang),
                         t("SD_opinion_A3", input$lang),
                         t("SD_opinion_A4", input$lang)
                       )
                     ),
                     selected = isolate(input$SD_opinion %||% character(0))
        ),
        
        actionButton("sd_Back", t("nav_back", input$lang)),
        actionButton("sd_Next", t("nav_next", input$lang))
      )
      
      
      ### PAGE 11: SALINISATION ############################
    } else if (p == 11) {
      res <- NULL
      desc <- ""
      
      try({
        res <- readResults(session_token())
        if (!is.null(res) && nrow(res) > 0 && "soil_type_description" %in% names(res)) {
          desc <- as.character(res$soil_type_description[1])
          if (is.na(desc) || is.null(desc)) desc <- ""
        }
      }, silent = TRUE)
      tagList(
        h3(t("S_K_header", input$lang)),
        
        if (!identical(desc, "") && nzchar(desc)) {
          div(
            style = "margin-bottom: 12px; color: #333;",
            sprintf("%s: %s", t("your_soil_description", input$lang), desc)
          )
        },
        
        tags$script(HTML(sprintf("initExclusiveGroup('%s', %s);", htmltools::htmlEscape("S_K"), jsonlite::toJSON("S_K_5", auto_unbox = TRUE)))),
        uiOutput("S_K_1"),
        
        radioButtons("S_approach",
                     t("S_approach_Q", input$lang),
                     choices = setNames(
                       paste0("S_approach_", 1:4),
                       c(
                         t("S_approach_A1", input$lang),
                         t("S_approach_A2", input$lang),
                         t("S_approach_A3", input$lang),
                         t("S_approach_A4", input$lang)
                       )
                     ),
                     selected = isolate(input$S_approach %||% character(0))
        ),
        
        radioButtons("S_opinion",
                     t("S_opinion_Q", input$lang),
                     choices = setNames(
                       paste0("S_opinion_", 1:4),
                       c(
                         t("S_opinion_A1", input$lang),
                         t("S_opinion_A2", input$lang),
                         t("S_opinion_A3", input$lang),
                         t("S_opinion_A4", input$lang)
                       )
                     ),
                     selected = isolate(input$S_opinion %||% character(0))
        ),
        
        actionButton("sal_Back", t("nav_back", input$lang)),
        actionButton("sal_Next", t("nav_next", input$lang))
      )
      
      ### PAGE 12: BIODIVERSITÉ DU SOL ############################
    } else if (p == 12) {
      
      res <- NULL
      desc <- ""
      
      try({
        res <- readResults(session_token())
        if (!is.null(res) && nrow(res) > 0 && "soil_type_description" %in% names(res)) {
          desc <- as.character(res$soil_type_description[1])
          if (is.na(desc) || is.null(desc)) desc <- ""
        }
      }, silent = TRUE)
      
      tagList(
        h3(t("HL_K_header", input$lang)),
        
        if (!identical(desc, "") && nzchar(desc)) {
          div(
            style = "margin-bottom: 12px; color: #333;",
            sprintf("%s: %s", t("your_soil_description", input$lang), desc)
          )
        },
        
        
        tags$script(HTML(sprintf("initExclusiveGroup('%s', %s);", htmltools::htmlEscape("HL_K"), jsonlite::toJSON("HL_K_5", auto_unbox = TRUE)))),
        uiOutput("HL_K_1"),
        
        radioButtons("HL_approach",
                     t("HL_approach_Q", input$lang),
                     choices = setNames(
                       paste0("HL_approach_", 1:4),
                       c(
                         t("HL_approach_A1", input$lang),
                         t("HL_approach_A2", input$lang),
                         t("HL_approach_A3", input$lang),
                         t("HL_approach_A4", input$lang)
                       )
                     ),
                     selected = isolate(input$HL_approach %||% character(0))
        ),
        
        radioButtons("HL_opinion",
                     t("HL_opinion_Q", input$lang),
                     choices = setNames(
                       paste0("HL_opinion_", 1:4),
                       c(
                         t("HL_opinion_A1", input$lang),
                         t("HL_opinion_A2", input$lang),
                         t("HL_opinion_A3", input$lang),
                         t("HL_opinion_A4", input$lang)
                       )
                     ),
                     selected = isolate(input$HL_opinion %||% character(0))
        ),
        
        radioButtons("HL_val1",
                     t("HL_val1_Q", input$lang),
                     choices = setNames(
                       c("HL_val1_A1", "HL_val1_A2"),
                       c(t("yes", input$lang), t("no", input$lang))
                     ),
                     selected = isolate(input$HL_val1 %||% character(0))
        ),
        
        actionButton("bio_Back", t("nav_back", input$lang)),
        actionButton("bio_Next", t("nav_next", input$lang))
      )
      
      ### PAGE 13: FERTILISATION ############################
    } else if (p == 13) {
      res <- NULL
      desc <- ""
      
      try({
        res <- readResults(session_token())
        if (!is.null(res) && nrow(res) > 0 && "soil_type_description" %in% names(res)) {
          desc <- as.character(res$soil_type_description[1])
          if (is.na(desc) || is.null(desc)) desc <- ""
        }
      }, silent = TRUE)
      
      tagList(
        h3(t("NM_header", input$lang)),
        
        if (!identical(desc, "") && nzchar(desc)) {
          div(
            style = "margin-bottom: 12px; color: #333;",
            sprintf("%s: %s", t("your_soil_description", input$lang), desc)
          )
        },
        
        tags$script(HTML(sprintf("initExclusiveGroup('%s', %s);", htmltools::htmlEscape("NM_K"), jsonlite::toJSON("NM_K_5", auto_unbox = TRUE)))),
        uiOutput("NM_K_1"),
        
        radioButtons("NM_approach",
                     t("NM_approach_Q", input$lang),
                     choices = setNames(
                       paste0("NM_approach_", 1:4),
                       c(
                         t("NM_approach_A1", input$lang),
                         t("NM_approach_A2", input$lang),
                         t("NM_approach_A3", input$lang),
                         t("NM_approach_A4", input$lang)
                       )
                     ),
                     selected = isolate(input$NM_approach %||% character(0))
        ),
        
        radioButtons("NM_opinion",
                     t("NM_opinion_Q", input$lang),
                     choices = setNames(
                       paste0("NM_opinion_", 1:4),
                       c(
                         t("NM_opinion_A1", input$lang),
                         t("NM_opinion_A2", input$lang),
                         t("NM_opinion_A3", input$lang),
                         t("NM_opinion_A4", input$lang)
                       )
                     ),
                     selected = isolate(input$NM_opinion %||% character(0))
        ),
        
        actionButton("nm_Back", t("nav_back", input$lang)),
        actionButton("nm_Next", t("nav_next", input$lang))
      )
      
      
      ### PAGE 14: WATER MANAGEMENT ############################################
    } else if (p == 14) {
      
      res <- NULL
      desc <- ""
      
      try({
        res <- readResults(session_token())
        if (!is.null(res) && nrow(res) > 0 && "soil_type_description" %in% names(res)) {
          desc <- as.character(res$soil_type_description[1])
          if (is.na(desc) || is.null(desc)) desc <- ""
        }
      }, silent = TRUE)
      
      tagList(
        h3(t("SW_header", input$lang)),
        
        if (!identical(desc, "") && nzchar(desc)) {
          div(
            style = "margin-bottom: 12px; color: #333;",
            sprintf("%s: %s", t("your_soil_description", input$lang), desc)
          )
        },
        
        
        tags$script(HTML(sprintf("initExclusiveGroup('%s', %s);", htmltools::htmlEscape("SW_K"), jsonlite::toJSON("SW_K_5", auto_unbox = TRUE)))),
        uiOutput("SW_K_1"),
        
        radioButtons("SW_approach",
                     t("SW_approach_Q", input$lang),
                     choices = setNames(
                       paste0("SW_approach_", 1:4),
                       c(
                         t("SW_approach_A1", input$lang),
                         t("SW_approach_A2", input$lang),
                         t("SW_approach_A3", input$lang),
                         t("SW_approach_A4", input$lang)
                       )
                     ),
                     selected = isolate(input$SW_approach %||% character(0))
        ),
        
        radioButtons("SW_opinion",
                     t("SW_opinion_Q", input$lang),
                     choices = setNames(
                       paste0("SW_opinion_", 1:4),
                       c(
                         t("SW_opinion_A1", input$lang),
                         t("SW_opinion_A2", input$lang),
                         t("SW_opinion_A3", input$lang),
                         t("SW_opinion_A4", input$lang)
                       )
                     ),
                     selected = isolate(input$SW_opinion %||% character(0))
        ),
        
        actionButton("sw_Back", t("nav_back", input$lang)),
        actionButton("sw_Next", t("nav_next", input$lang))
      )
      
      ### PAGE 15: CARBON ######################################################
    } else if (p == 15) {
      
      res <- NULL
      desc <- ""
      
      try({
        res <- readResults(session_token())
        if (!is.null(res) && nrow(res) > 0 && "soil_type_description" %in% names(res)) {
          desc <- as.character(res$soil_type_description[1])
          if (is.na(desc) || is.null(desc)) desc <- ""
        }
      }, silent = TRUE)
      
      tagList(
        h3(t("DC_header", input$lang)),
        
        
        if (!identical(desc, "") && nzchar(desc)) {
          div(
            style = "margin-bottom: 12px; color: #333;",
            sprintf("%s: %s", t("your_soil_description", input$lang), desc)
          )
        },
        
        tags$script(HTML(sprintf("initExclusiveGroup('%s', %s);", htmltools::htmlEscape("DC_K"), jsonlite::toJSON("DC_K_5", auto_unbox = TRUE)))),
        uiOutput("DC_K_1"),
        
        radioButtons("DC_approach",
                     t("DC_approach_Q", input$lang),
                     choices = setNames(
                       paste0("DC_approach_", 1:4),
                       c(
                         t("DC_approach_A1", input$lang),
                         t("DC_approach_A2", input$lang),
                         t("DC_approach_A3", input$lang),
                         t("DC_approach_A4", input$lang)
                       )
                     ),
                     selected = isolate(input$DC_approach %||% character(0))
        ),
        
        radioButtons("DC_opinion",
                     t("DC_opinion_Q", input$lang),
                     choices = setNames(
                       paste0("DC_opinion_", 1:4),
                       c(
                         t("DC_opinion_A1", input$lang),
                         t("DC_opinion_A2", input$lang),
                         t("DC_opinion_A3", input$lang),
                         t("DC_opinion_A4", input$lang)
                       )
                     ),
                     selected = isolate(input$DC_opinion %||% character(0))
        ),
        
        actionButton("dc_Back", t("nav_back", input$lang)),
        actionButton("dc_Next", t("nav_next", input$lang))
      )
      
      
      ### PAGE 16: THREATS #####################################################
    } else if (p == 16) {
      tagList(
        h3(t("Threats_header", input$lang)),
        
        # Erosion
        radioButtons("Threat_Val_E", t("Threat_Val_E_Q", input$lang),
                     choices = setNames(
                       paste0("Threat_Val_E_A", 1:6),
                       c(
                         t("Threat_Val_E_A1", input$lang),
                         t("Threat_Val_E_A2", input$lang),
                         t("Threat_Val_E_A3", input$lang),
                         t("Threat_Val_E_A4", input$lang),
                         t("Threat_Val_E_A5", input$lang),
                         t("Threat_Val_E_A6", input$lang)
                       )
                     ),
                     selected = isolate(input$Threat_Val_E %||% character(0))
        ),
        textInput("Threat_Val_E_comment", t("Threat_Val_E_comment", input$lang), 
                  value = safe_isolate_string(input$Threat_Val_E_comment %||% "")),
        
        # Acidification
        radioButtons("Threat_Val_A", t("Threat_Val_A_Q", input$lang),
                     choices = setNames(
                       paste0("Threat_Val_A", 1:6),
                       c(
                         t("Threat_Val_A1", input$lang),
                         t("Threat_Val_A2", input$lang),
                         t("Threat_Val_A3", input$lang),
                         t("Threat_Val_A4", input$lang),
                         t("Threat_Val_A5", input$lang),
                         t("Threat_Val_A6", input$lang)
                       )
                     ),
                     selected = isolate(input$Threat_Val_A %||% character(0))
        ),
        textInput("Threat_Val_A_comment", t("Threat_Val_A_comment", input$lang), 
                  value = safe_isolate_string(input$Threat_Val_A_comment %||% "")),
        
        # Structure decline
        radioButtons("Threat_Val_SD", t("Threat_Val_SD_Q", input$lang),
                     choices = setNames(
                       paste0("Threat_Val_SD_A", 1:6),
                       c(
                         t("Threat_Val_SD_A1", input$lang),
                         t("Threat_Val_SD_A2", input$lang),
                         t("Threat_Val_SD_A3", input$lang),
                         t("Threat_Val_SD_A4", input$lang),
                         t("Threat_Val_SD_A5", input$lang),
                         t("Threat_Val_SD_A6", input$lang)
                       )
                     ),
                     selected = isolate(input$Threat_Val_SD %||% character(0))
        ),
        textInput("Threat_Val_SD_comment", t("Threat_Val_SD_comment", input$lang), 
                  value = safe_isolate_string(input$Threat_Val_SD_comment %||% "")),
        
        # Salinisation
        radioButtons("Threat_Val_S", t("Threat_Val_S_Q", input$lang),
                     choices = setNames(
                       paste0("Threat_Val_S_A", 1:6),
                       c(
                         t("Threat_Val_S_A1", input$lang),
                         t("Threat_Val_S_A2", input$lang),
                         t("Threat_Val_S_A3", input$lang),
                         t("Threat_Val_S_A4", input$lang),
                         t("Threat_Val_S_A5", input$lang),
                         t("Threat_Val_S_A6", input$lang)
                       )
                     ),
                     selected = isolate(input$Threat_Val_S %||% character(0))
        ),
        textInput("Threat_Val_S_comment", t("Threat_Val_S_comment", input$lang), 
                  value = safe_isolate_string(input$Threat_Val_S_comment %||% "")),
        
        # Biodiversity loss (HL)
        radioButtons("Threat_Val_HL", t("Threat_Val_HL_Q", input$lang),
                     choices = setNames(
                       paste0("Threat_Val_HL_A", 1:6),
                       c(
                         t("Threat_Val_HL_A1", input$lang),
                         t("Threat_Val_HL_A2", input$lang),
                         t("Threat_Val_HL_A3", input$lang),
                         t("Threat_Val_HL_A4", input$lang),
                         t("Threat_Val_HL_A5", input$lang),
                         t("Threat_Val_HL_A6", input$lang)
                       )
                     ),
                     selected = isolate(input$Threat_Val_HL %||% character(0))
        ),
        textInput(
          "Threat_Val_HL_comment",
          t("Threat_Val_HL_comment", input$lang),
          value = safe_isolate_string(input$Threat_Val_HL_comment %||% "")
        ),
        
        # Fertility loss / nutrient management (NM)
        radioButtons(
          "Threat_Val_NM",
          t("Threat_Val_NM_Q", input$lang),
          choices = setNames(
            paste0("Threat_Val_NM_A", 1:6),
            c(
              t("Threat_Val_NM_A1", input$lang),
              t("Threat_Val_NM_A2", input$lang),
              t("Threat_Val_NM_A3", input$lang),
              t("Threat_Val_NM_A4", input$lang),
              t("Threat_Val_NM_A5", input$lang),
              t("Threat_Val_NM_A6", input$lang)
            )
          ),
          selected = isolate(input$Threat_Val_NM %||% character(0))
        ),
        textInput(
          "Threat_Val_NM_comment",
          t("Threat_Val_NM_comment", input$lang),
          value = safe_isolate_string(input$Threat_Val_NM_comment %||% "")),
        
        # Decarbonisation
        radioButtons("Threat_Val_DC", t("Threat_Val_DC_Q", input$lang),
                     choices = setNames(
                       paste0("Threat_Val_DC_A", 1:6),
                       c(
                         t("Threat_Val_DC_A1", input$lang),
                         t("Threat_Val_DC_A2", input$lang),
                         t("Threat_Val_DC_A3", input$lang),
                         t("Threat_Val_DC_A4", input$lang),
                         t("Threat_Val_DC_A5", input$lang),
                         t("Threat_Val_DC_A6", input$lang)
                       )
                     ),
                     selected = isolate(input$Threat_Val_DC %||% character(0))
        ),
        
        textInput("Threat_Val_DC_comment", t("Threat_Val_DC_comment", input$lang), 
                  value = safe_isolate_string(input$Threat_Val_DC_comment %||% "")),
        br(),
        br(),
        textInput("OptionalComment", t("OptionalComment_text", input$lang), 
                  value = safe_isolate_string(input$OptionalComment %||% "")),
        actionButton("threats_Back", t("nav_back", input$lang)),
        actionButton("threats_Next", t("nav_next", input$lang))
      )
    }
    else if (p == 17) {
      tagList(
        h3(t("submit_answers_header", input$lang)),  # Submit your answers
        
        
        # --- Demographics ---
        h4(t("Demographics_header", input$lang)),
        radioButtons(
          "Age",
          t("Age_Q", input$lang),
          choices = setNames(
            c("under25", "25to34", "35to44", "45to54", "55to64", "65to74", "75plus", "noanswer"),
            c(
              t("Age_A1", input$lang),
              t("Age_A2", input$lang),
              t("Age_A3", input$lang),
              t("Age_A4", input$lang),
              t("Age_A5", input$lang),
              t("Age_A6", input$lang),
              t("Age_A7", input$lang),
              t("Age_A8", input$lang)
            )
          ),
          selected = isolate(input$Age %||% character(0))
        ),
        
        radioButtons(
          "education_level",
          t("education_level_Q", input$lang),
          choices = setNames(
            paste0("education_", 1:8),
            c(
              t("education_level_A1", input$lang),
              t("education_level_A2", input$lang),
              t("education_level_A3", input$lang),
              t("education_level_A4", input$lang),
              t("education_level_A5", input$lang),
              t("education_level_A6", input$lang),
              t("education_level_A7", input$lang),
              t("education_level_A8", input$lang)
            )
          ),
          selected = isolate(input$education_level %||% character(0))
        ),
        
        uiOutput("Land_ownership_1"),
        
        tryCatch({
          numericInput(
            "Land_area", t("Land_area_Q", input$lang),
            value = isolate(if (is.null(input$Land_area)) NA else input$Land_area),
            min = 0
          )
        }, error = function(e) {
          print(paste("Error in Land_area numericInput:", e$message))
          NULL
        }),
        br(),
        
        # --- Optional: Receive results/contact for studies ---
        h4(t("receive_results_Q", input$lang)),
        radioButtons(
          "receive_results", NULL,
          choices = setNames(
            c("yes", "no"),
            c(t("receive_results_A1", input$lang), t("receive_results_A2", input$lang))
          ),
          selected = isolate(input$receive_results %||% character(0))
        ),
        conditionalPanel(
          condition = "input.receive_results == 'yes'",
          textInput("contact_email", t("receive_results_email", input$lang),
                    safe_isolate_string(input$contact_email %||% ""))
        ),
        p(tags$em(t("receive_results_privacy", input$lang))),
        br(),
        
        actionButton("final_Back", t("nav_back", input$lang)),
        actionButton("submitAll", t("submitAll", input$lang), class = "btn-success")
      )
    }
    else if (p == 18) {
      tagList(
        h3(t("thank_you_header", input$lang)),
        
        uiOutput("finalScoreMessage"),
        
        br(),
        actionButton("restartApp", t("restartApp", input$lang), icon = icon("redo")),
        
        tags$div(
          style = "font-size: 2em; text-align: center; margin-top: 20px;",
          "🎉✨🎇"  # Simple celebratory emojis that work without JS or internet
        )
      )
    }
    
  })
  
  
  
  
  
  ###Presentation addition  ####
  
  output$farmEnterprises_1 <- renderUI({
    tryCatch({
      message("🔍 farmEnterprises_1 entered")
      label <- t("farmEnterprises_Q", input$lang)
      choices <- setNames(
        c(
          "Cultures de céréales",
          "Lait",
          "Élevage extensif de moutons ou de bovins",
          "Élevage intensif de moutons ou de bovins",
          "Porc ou volaille en plein air",
          "Cultures horticoles en champ",
          "Vignes",
          "Verger – fruits, noix",
          "Autre (précisez)"
        ),
        c(
          t("farmEnterprises_A1", input$lang),
          t("farmEnterprises_A2", input$lang),
          t("farmEnterprises_A3", input$lang),
          t("farmEnterprises_A4", input$lang),
          t("farmEnterprises_A5", input$lang),
          t("farmEnterprises_A6", input$lang),
          t("farmEnterprises_A7", input$lang),
          t("farmEnterprises_A8", input$lang),
          t("farmEnterprises_A9", input$lang)
        )
      )
      message("✅ Choices for farmEnterprises built")
      checkboxGroupInput("farmEnterprises", label = label, choices = choices, selected = input$farmEnterprises)
    }, error = function(e) {
      message("❌ ERROR in farmEnterprises_1: ", e$message)
      div(style = "color:red;", "⚠ Error loading farmEnterprises")
    })
  })
  
  
  output$perceivedThreats_1 <- renderUI({
    message("🔧 entered renderUI for perceivedThreats")
    
    tryCatch({
      label <- t("perceivedThreats_Q", input$lang)
      choices <- setNames(
        c(
          "Érosion", "Acidification", "Déclin de la structure (par ex, compaction)",
          "Perte de carbone dans le sol", "Salinisation", "Perte d'habitat/dégradation biologie du sol",
          "Pollution des sols", "Artificialisation des terres", "Autre (précisez)"
        ),
        c(
          t("perceivedThreats_A1", input$lang), t("perceivedThreats_A2", input$lang),
          t("perceivedThreats_A3", input$lang), t("perceivedThreats_A4", input$lang),
          t("perceivedThreats_A5", input$lang), t("perceivedThreats_A6", input$lang),
          t("perceivedThreats_A7", input$lang), t("perceivedThreats_A8", input$lang),
          t("perceivedThreats_A9", input$lang)
        )
      )
      
      message("✅ Label: ", label)
      message("✅ Choices built")
      
      checkboxGroupInput("perceivedThreats", label = label, choices = choices, selected = input$perceivedThreats)
    }, error = function(e) {
      message("❌ ERROR in perceivedThreats_1: ", e$message)
      div(style = "color:red;", "⚠ Error loading perceivedThreats")
    })
  })
  
  ####supply add on ####
  
  output$supply_newconcepts_1 <- renderUI({
    renderTranslatedCheckbox(
      id = "supply_newconcepts",
      label_key = "supply_newconcepts_Q",
      value_choices = paste0("supply_newconcepts_", 1:5),
      label_keys = paste0("supply_newconcepts_A", 1:5),
      lang = input$lang,
      current_input = input$supply_newconcepts,
      session = session,
      t_func = t
    )
  })
  
  ####Food Add on ####
  output$food_newconcepts_1 <- renderUI({
    renderTranslatedCheckbox(
      id = "food_newconcepts",
      label_key = "food_newconcepts_Q",
      value_choices = paste0("food_newconcepts_", 1:5),
      label_keys = paste0("food_newconcepts_A", 1:5),
      lang = input$lang,
      current_input = input$food_newconcepts,
      session = session,
      t_func = t
    )
  })
  
  output$econ_newconcepts_1 <- renderUI({
    renderTranslatedCheckbox(
      id = "econ_newconcepts",
      label_key = "econ_newconcepts_Q",
      value_choices = paste0("econ_newconcepts_", 1:5),
      label_keys = paste0("econ_newconcepts_A", 1:5),
      lang = input$lang,
      current_input = input$econ_newconcepts,
      session = session,
      t_func = t
    )
  })
  
  
  
  output$defense_newconcepts_1 <- renderUI({
    renderTranslatedCheckbox(
      id = "defense_newconcepts",
      label_key = "defense_newconcepts_Q",
      value_choices = paste0("defense_newconcepts_", 1:5),
      label_keys = paste0("defense_newconcepts_A", 1:5),
      lang = input$lang,
      current_input = input$defense_newconcepts,
      session = session,
      t_func = t
    )
  })

  observe({
    lang <- input$lang %||% "French"
    
    # Get translated label
    label <- t("intSec_newconcepts_Q", lang)
    
    # Get translated choices
    value_choices <- paste0("intSec_newconcepts_", 1:5)
    label_keys <- paste0("intSec_newconcepts_A", 1:5)
    choices <- setNames(
      value_choices,
      vapply(label_keys, function(k) {
        t(k, lang)
      }, character(1))
    )
    
    # Get current selected values
    selected_values <- input$intSec_newconcepts %||% character(0)
    
    updateCheckboxGroupInput(
      session,
      "intSec_newconcepts",
      label = label,
      choices = choices,
      selected = selected_values
    )
  })
  
  ###E_K_1####
  output$E_K_1 <- renderUI({
    renderTranslatedCheckbox(
      id = "E_K",
      label_key = "E_K_Q",
      value_choices = paste0("E_K_", 1:5),
      label_keys = paste0("E_K_A", 1:5),
      lang = input$lang,
      current_input = input$E_K,
      session = session,
      t_func = t
    )
  })
  
  output$A_K_1 <- renderUI({
    renderTranslatedCheckbox(
      id = "A_K",
      label_key = "A_K_Q",
      value_choices = paste0("A_K_", 1:5),
      label_keys = paste0("A_K_A", 1:5),
      lang = input$lang,
      current_input = input$A_K,
      session = session,
      t_func = t 
    )
  })
  
  
  output$SD_K_1 <- renderUI({
    renderTranslatedCheckbox(
      id = "SD_K",
      label_key = "SD_K_Q",
      value_choices = paste0("SD_K_", 1:5),
      label_keys = paste0("SD_K_A", 1:5),
      lang = input$lang,
      current_input = input$SD_K,
      session = session,
      t_func = t
    )
  })
  
  output$S_K_1 <- renderUI({
    renderTranslatedCheckbox(
      id = "S_K",
      label_key = "S_K_Q",
      value_choices = paste0("S_K_", 1:5),
      label_keys = paste0("S_K_A", 1:5),
      lang = input$lang,
      current_input = input$S_K,
      session = session,
      t_func = t
    )
  })
  
  output$HL_K_1 <- renderUI({
    renderTranslatedCheckbox(
      id = "HL_K",
      label_key = "HL_K_Q",
      value_choices = paste0("HL_K_", 1:5),
      label_keys = paste0("HL_K_A", 1:5),
      lang = input$lang,
      current_input = input$HL_K,
      session = session,
      t_func = t
    )
  })
  
  output$NM_K_1 <- renderUI({
    renderTranslatedCheckbox(
      id = "NM_K",
      label_key = "NM_K_Q",
      value_choices = paste0("NM_K_", 1:5),
      label_keys = paste0("NM_K_A", 1:5),
      lang = input$lang,
      current_input = input$NM_K,
      session = session,
      t_func = t
    )
  })
  
  output$SW_K_1 <- renderUI({
    renderTranslatedCheckbox(
      id = "SW_K",
      label_key = "SW_K_Q",
      value_choices = paste0("SW_K_", 1:5),
      label_keys = paste0("SW_K_A", 1:5),
      lang = input$lang,
      current_input = input$SW_K,
      session = session,
      t_func = t
    )
  })
  
  output$DC_K_1 <- renderUI({
    renderTranslatedCheckbox(
      id = "DC_K",
      label_key = "DC_K_Q",
      value_choices = paste0("DC_K_", 1:5),
      label_keys = paste0("DC_K_A", 1:5),
      lang = input$lang,
      current_input = input$DC_K,
      session = session,
      t_func = t
    )
  })
  
  output$Land_ownership_1 <- renderUI({
    renderTranslatedCheckbox(
      id = "Land_ownership",
      label_key = "Land_ownership_Q",
      value_choices = paste0("Land_ownership_A", 1:5),
      label_keys = paste0("Land_ownership_A", 1:5),
      lang = input$lang,
      current_input = input$Land_ownership,
      session = session,
      t_func = t
    )
  })
  
  output$finalScoreMessage <- renderUI({
    res <- readResults(session_token(), table = "farmers_results")
    if (is.null(res) || nrow(res) == 0) {
      return(div(style = "color:#a00; font-weight:600;", t("no_results_found", input$lang)))
    }
    
    final_scores <- res[1, grep("^score_.*_final$", names(res)), drop = FALSE]
    numeric_scores <- as.numeric(unlist(final_scores))
    numeric_scores <- numeric_scores[is.finite(numeric_scores)]
    if (length(numeric_scores) == 0) {
      return(div(style = "color:#a00; font-weight:600;", t("no_valid_scores", input$lang)))
    }
    
    avg_score <- mean(numeric_scores)
    band_key <- dplyr::case_when(
      avg_score <= 0.25 ~ "scoring_band_1",
      avg_score <= 0.5  ~ "scoring_band_2",
      avg_score <= 0.75 ~ "scoring_band_3",
      TRUE              ~ "scoring_band_4"
    )
    
    div(
      style = "padding: 30px; background-color: #f0f7ff; border-radius: 12px; max-width: 700px; margin: 0 auto; border: 1px solid #d0e3f0;",
      h3(t("score_interpretation_title", input$lang), style = "color:#2c3e50;"),
      p(t(band_key, input$lang), style = "font-size: 1.15em; line-height: 1.6em; color: #333;"),
      tags$hr(),
      p(t("score_thank_you_extended", input$lang), style = "font-style: italic; margin-top: 16px;")
    )
  })
  
  
  
  
  
  ##Buttons####
  
  
  observeEvent(input$btnStart, {
    req(isTRUE(input$consent))           # must tick consent
    req(nzchar(session_token()))         # avoid race: only proceed once token exists

    data_row <- data.frame(
      session_token = session_token(),
      timestamp     = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
      language      = input$lang %||% "French",
      stringsAsFactors = FALSE
    )

    saveRawInputs(data_row)
    currentPage(1L)
  })
  
  ###Page PRESENTATION ####
  
  
  observeEvent(input$PRESENTATION_Next, {
    lang <- input$lang %||% "French"
    
    # Validate token
    token <- session_token()
    if (is.null(token) || length(token) == 0 || token == "") {
      
      message("⚠️ session_token was null — generated new token: ", token)
    }
    
    
    # Validate required inputs
    if (is.null(input$role) || !(input$role %in% c("farmer", "other"))) {
      showNotification(t("missing_role", lang), type = "error")
      return()
    }
    if (is.null(input$perceivedThreats) || length(input$perceivedThreats) == 0) {
      showNotification(t("missing_threats", lang), type = "error")
      return()
    }
    
    if ("Autre (précisez)" %in% input$farmEnterprises && is.null(input$other_farmEnterprises)) {
      showNotification(t("missing_other_farm", lang), type = "error")
      return()
    }
    
    if ("Autre (précisez)" %in% input$perceivedThreats && is.null(input$other_perceivedThreats)) {
      showNotification(t("missing_other_threat", lang), type = "error")
      return()
    }
    
    # Safe data frame creation
    data_row <- data.frame(
      session_token = token,
      role = input$role %||% "",
      other_role = input$other_role %||% "",
      other_farmEnterprises = input$other_farmEnterprises %||% "",
      other_perceivedThreats = input$other_perceivedThreats %||% "",
      farm_enterprises = csv_collapse(input$farmEnterprises),
      perceived_threats = csv_collapse(input$perceivedThreats),
      
      stringsAsFactors = FALSE
    )
    
    saveRawInputs(data_row)
    clearLocalStorageInputs(session, c("role", "farm_enterprises", "perceived_threats", "supply_opinion"))
    log_event(paste("Updated Page 2 for:", token))
    currentPage(2)
  })
  
  
  
  ###page Supply #####
  
  observeEvent(input$Supply_Back, {
    lang <- input$lang %||% "French"
    currentPage(1)
  })
  
  observeEvent(input$Supply_Next, {
    lang <- input$lang %||% "French"
    
    # Optional validation: make sure something is selected (or warn)
    if (is.null(input$supply_newconcepts) || length(input$supply_newconcepts) == 0) {
      showNotification(t("missing_newconcepts", lang), type = "error")
      return()
    }
    if (is.null(input$supply_approach) || input$supply_approach == "") {
      showNotification(t("missing_approach", lang), type = "error")
      return()
    }
    if (is.null(input$supply_opinion) || input$supply_opinion == "") {
      showNotification(t("missing_opinion", lang), type = "error")
      return()
    }
    
    data_row <- data.frame(
      session_token = session_token(),
      supply_newconcepts = csv_collapse(input$supply_newconcepts),
      supply_approach = input$supply_approach,
      supply_opinion = input$supply_opinion,
      stringsAsFactors = FALSE
    )
    
    saveRawInputs(data_row)
    
    score_data <- scoreSupplySection(input, session_token())
    
    saveScoringData(score_data, table = "farmers_results")
    
    clearLocalStorageInputs(session, c(
      "supply_newconcepts", "supply_approach", "supply_opinion"
    ))
    log_event(paste("Updated Page 3 for:", session_token()))
    currentPage(3)
  })
  
  ###page food #####
  observeEvent(input$food_Back, {
    lang <- input$lang %||% "French"
    currentPage(2)
  })
  
  observeEvent(input$food_Next, {
    lang <- input$lang %||% "French"
    
    if (is.null(input$food_newconcepts) || length(input$food_newconcepts) == 0) {
      showNotification(t("missing_newconcepts", lang), type = "error")
      return()
    }
    if (is.null(input$food_approach) || input$food_approach == "") {
      showNotification(t("missing_approach", lang), type = "error")
      return()
    }
    if (is.null(input$food_opinion) || input$food_opinion == "") {
      showNotification(t("missing_opinion", lang), type = "error")
      return()
    }
    
    
    data_row <- data.frame(
      session_token = session_token(),
      food_newconcepts = csv_collapse(input$food_newconcepts),
      food_approach = input$food_approach,
      food_opinion = input$food_opinion,
      stringsAsFactors = FALSE
    )
    
    saveRawInputs(data_row)
    score_data <- scoreFoodSection(input, session_token())
    saveScoringData(score_data, table = "farmers_results")
    
    clearLocalStorageInputs(session, c(
      "food_newconcepts", "food_approach", "food_opinion"
    ))
    
    log_event(paste("Updated Page 4 for:", session_token()))
    currentPage(4)
  })
  
  ###page econ #####
  observeEvent(input$econ_Next, {
    lang <- input$lang %||% "French"
    
    if (is.null(input$econ_newconcepts) || length(input$econ_newconcepts) == 0) {
      showNotification(t("missing_newconcepts", lang), type = "error")
      return()
    }
    if (is.null(input$econ_approach) || input$econ_approach == "") {
      showNotification(t("missing_approach", lang), type = "error")
      return()
    }
    if (is.null(input$econ_opinion) || input$econ_opinion == "") {
      showNotification(t("missing_opinion", lang), type = "error")
      return()
    }
    
    
    
    data_row <- data.frame(
      session_token = session_token(),
      econ_newconcepts = paste(input$econ_newconcepts, collapse = ";"),
      econ_approach = input$econ_approach,
      econ_opinion = input$econ_opinion,
      stringsAsFactors = FALSE
    )
    saveRawInputs(data_row)
    score_data <- scoreEconSection(input, session_token())
    saveScoringData(score_data, table = "farmers_results")
    
    clearLocalStorageInputs(session, c(
      "econ_newconcepts", "econ_approach", "econ_opinion"
    ))
    log_event(paste("Saved Page econ for:", session_token()))
    currentPage(5)
  })
  
  observeEvent(input$econ_Back, {
    lang <- input$lang %||% "French"
    currentPage(3)
  })
  
  ###Page intSec ####
  observeEvent(input$intSec_Next, {
    lang <- input$lang %||% "French"
    
    if (is.null(input$intSec_newconcepts) || length(input$intSec_newconcepts) == 0) {
      showNotification(t("missing_newconcepts", lang), type = "error")
      return()
    }
    if (is.null(input$intSec_approach) || input$intSec_approach == "") {
      showNotification(t("missing_approach", lang), type = "error")
      return()
    }
    if (is.null(input$intSec_opinion) || input$intSec_opinion == "") {
      showNotification(t("missing_opinion", lang), type = "error")
      return()
    }
    if (is.null(input$intSec_validation) || input$intSec_validation == "") {
      showNotification(t("missing_intSec_validation", lang), type = "error")
      return()
    }
    
    
    data_row <- data.frame(
      session_token = session_token(),
      intSec_newconcepts = paste(input$intSec_newconcepts, collapse = ";"),
      intSec_approach = input$intSec_approach,
      intSec_opinion = input$intSec_opinion,
      intSec_validation = input$intSec_validation,
      intSec_validation_comment = input$intSec_validation_comment %||% "",
      stringsAsFactors = FALSE
    )
    saveRawInputs(data_row)
    score_data <- scoreIntSecSection(input, session_token())
    saveScoringData(score_data, table = "farmers_results")
    
    
    clearLocalStorageInputs(session, c(
      "intSec_newconcepts", "intSec_approach", "intSec_opinion",
      "intSec_validation", "intSec_validation_A3"
    ))
    
    log_event(paste("Saved Page 5 (intSec) for:", session_token()))
    currentPage(6)
  })
  
  observeEvent(input$intSec_Back, {
    lang <- input$lang %||% "French"
    currentPage(4)
  })
  
  
  ### defense ####
  observeEvent(input$defense_Next, {
    lang <- input$lang %||% "French"
    
    if (is.null(input$defense_newconcepts) || length(input$defense_newconcepts) == 0) {
      showNotification(t("missing_newconcepts", lang), type = "error")
      return()
    }
    if (is.null(input$defense_approach) || input$defense_approach == "") {
      showNotification(t("missing_approach", lang), type = "error")
      return()
    }
    if (is.null(input$defense_opinion) || input$defense_opinion == "") {
      showNotification(t("missing_opinion", lang), type = "error")
      return()
    }
    if (is.null(input$defense_validation) || input$defense_validation == "") {
      showNotification(t("missing_validation", lang), type = "error")
      return()
    }
    
    
    data_row <- data.frame(
      session_token = session_token(),
      defense_newconcepts = paste(input$defense_newconcepts, collapse = ";"),
      defense_approach = input$defense_approach,
      defense_opinion = input$defense_opinion,
      defense_validation = input$defense_validation,
      defense_validation_comment = input$defense_validation2 %||% "",
      stringsAsFactors = FALSE
    )
    saveRawInputs(data_row)
    score_data <- scoreDefenseSection(input, session_token())
    saveScoringData(score_data, table = "farmers_results")
    
    clearLocalStorageInputs(session, c(
      "defense_newconcepts", "defense_approach", "defense_opinion",
      "defense_validation", "commentaires"
    ))
    
    log_event(paste("Saved Page 6 (defense) for:", session_token()))
    currentPage(7)
  })
  
  observeEvent(input$defense_Back, {
    lang <- input$lang %||% "French"
    currentPage(5)
  })
  
  
  
  
  ###Map Page ####


  
  
  
  observeEvent(input$map_Back, {
    lang <- input$lang %||% "French"
    currentPage(6)
  })
  
  observeEvent(input$map_Next, {
    lang <- input$lang %||% "French"
    
    if (is.null(input$country) || input$country == "") {
      showNotification(t("missing_country", lang), type = "error")
      return()
    }
    if (is.null(input$region2) || input$region2 == "") {
      showNotification(t("missing_region", lang), type = "error")
      return()
    }
    if (is.null(input$soil_type_description) || input$soil_type_description == "") {
      showNotification(t("missing_Soil_Type_Description", lang), type = "error")
      return()
    }
    if (is.null(input$numberSoilTypes) || input$numberSoilTypes == "") {
      showNotification(t("missing_numberSoilTypes", lang), type = "error")
      return()
    }
    if (is.null(userCoords$lat) || is.null(userCoords$lon)) {
      showNotification(t("missing_region_or_map", lang), type = "error")
      return()
    }
    
    
    data_row <- data.frame(
      session_token = session_token(),
      number_soil_types = input$numberSoilTypes,
      timestamp_map = format(Sys.time(), tz = "UTC", usetz = FALSE),
      language = input$lang,
      country = input$country,
      region1 = input$region1,
      region2 = input$region2,
      soil_type_description = input$soil_type_description,
      lat = if (!is.null(userCoords$lat)) userCoords$lat else NA,
      lon = if (!is.null(userCoords$lon)) userCoords$lon else NA,
      stringsAsFactors = FALSE
    )
    
    
    saveRawInputs(data_row)
    log_event(paste("Saved Page 7 (Map) data for:", session_token()))
    currentPage(8)
  })
  
  
  ### Page Erosion ####
  observeEvent(input$erosion_Next, {
    lang <- input$lang %||% "French"
    
    if (is.null(input$E_K) || length(input$E_K) == 0) {
      showNotification(t("missing_newconcepts", lang), type = "error")
      return()
    }
    if (is.null(input$E_approach) || input$E_approach == "") {
      showNotification(t("missing_approach", lang), type = "error")
      return()
    }
    if (is.null(input$E_opinion) || input$E_opinion == "") {
      showNotification(t("missing_opinion", lang), type = "error")
      return()
    }
    
    
    data_row <- data.frame(
      session_token = session_token(),
      E_K = paste(input$E_K, collapse = ";"),
      E_approach = input$E_approach,
      E_opinion = input$E_opinion,
      stringsAsFactors = FALSE
    )
    
    saveRawInputs(data_row)
    score_data <- scoreErosionSection(input, session_token())
    saveScoringData(score_data, table = "farmers_results")
    
    
    clearLocalStorageInputs(session, c("E_K", "E_approach", "E_opinion", "E_legislation"))
    log_event(paste("Saved Page 8 (Erosion) for:", session_token()))
    currentPage(9)
  })
  
  observeEvent(input$erosion_Back, {
    lang <- input$lang %||% "French"
    currentPage(7)
  })
  
  ### Page Acidification ####
  observeEvent(input$acid_Next, {
    lang <- input$lang %||% "French"
    
    if (is.null(input$A_K) || length(input$A_K) == 0) {
      showNotification(t("missing_newconcepts", lang), type = "error")
      return()
    }
    if (is.null(input$A_approach) || input$A_approach == "") {
      showNotification(t("missing_approach", lang), type = "error")
      return()
    }
    if (is.null(input$A_opinion) || input$A_opinion == "") {
      showNotification(t("missing_opinion", lang), type = "error")
      return()
    }
    
    
    data_row <- data.frame(
      session_token = session_token(), 
      A_K = paste(input$A_K, collapse = ";"),
      A_approach = input$A_approach,
      A_opinion = input$A_opinion,
      stringsAsFactors = FALSE
    )
    
    saveRawInputs(data_row)
    
    score_data <- scoreAcidSection(input, session_token())
    saveScoringData(score_data, table = "farmers_results")
    
    
    clearLocalStorageInputs(session, c("A_K", "A_approach", "A_opinion"))
    log_event(paste("Saved Page 9 (Acidification) for:", session_token()))
    currentPage(10)
  })
  
  observeEvent(input$acid_Back, {
    lang <- input$lang %||% "French"
    currentPage(8)
  })
  
  
  ### Page Soil Structure ####
  # Navigation and validation
  observeEvent(input$sd_Back, {
    lang <- input$lang %||% "French"
    currentPage(9)
  })
  
  observeEvent(input$sd_Next, {
    lang <- input$lang %||% "French"
    if (is.null(input$SD_K) || length(input$SD_K) == 0) {
      showNotification(t("missing_newconcepts", lang), type = "error")
      return()
    }
    if (is.null(input$SD_approach) || input$SD_approach == "") {
      showNotification(t("missing_approach", lang), type = "error")
      return()
    }
    if (is.null(input$SD_opinion) || input$SD_opinion == "") {
      showNotification(t("missing_opinion", lang), type = "error")
      return()
    }
    
    saveRawInputs(data.frame(
      session_token = session_token(),
      SD_K = csv_collapse(input$SD_K),
      SD_approach = input$SD_approach,
      SD_opinion = input$SD_opinion,
      stringsAsFactors = FALSE
    ))
    
    score_data <- scoreStructureSection(input, session_token())
    saveScoringData(score_data, table = "farmers_results")
    
    
    clearLocalStorageInputs(session, c("SD_K", "SD_approach", "SD_opinion"))
    log_event(paste("Saved Page 10 (Soil Structure) for:", session_token()))
    currentPage(11)
  })
  
  
  ###Sal inisation Page ####
  #
  observeEvent(input$sal_Next, {
    lang <- input$lang %||% "French"
    
    if (is.null(input$S_K) || length(input$S_K) == 0) {
      showNotification(t("missing_newconcepts", lang), type = "error")
      return()
    }
    if (is.null(input$S_approach) || input$S_approach == "") {
      showNotification(t("missing_approach", lang), type = "error")
      return()
    }
    if (is.null(input$S_opinion) || input$S_opinion == "") {
      showNotification(t("missing_opinion", lang), type = "error")
      return()
    }
    
    saveRawInputs(data.frame(
      session_token = session_token(),
      S_K = csv_collapse(input$S_K),
      S_approach = input$S_approach,
      S_opinion = input$S_opinion,
      stringsAsFactors = FALSE
    ))
    score_data <- scoreSalinitySection(input, session_token())
    saveScoringData(score_data, table = "farmers_results")
    
    clearLocalStorageInputs(session, c("S_K", "S_approach", "S_opinion"))
    log_event(paste("Saved Page 11 (Salinisation) for:", session_token()))
    currentPage(12)
  })
  
  
  observeEvent(input$sal_Back, {
    lang <- input$lang %||% "French"
    currentPage(10)
  })
  
  
  ### Biodiversity Page ####
  observeEvent(input$bio_Next, {
    lang <- input$lang %||% "French"
    
    if (is.null(input$HL_K) || length(input$HL_K) == 0) {
      showNotification(t("missing_newconcepts", lang), type = "error")
      return()
    }
    if (is.null(input$HL_approach)) {
      showNotification(t("missing_approach", lang), type = "error")
      return()
    }
    if (is.null(input$HL_opinion)) {
      showNotification(t("missing_opinion", lang), type = "error")
      return()
    }
    if (is.null(input$HL_val1)) {
      showNotification(t("missing_HL_validation", lang), type = "error")
      return()
    }
    
    saveRawInputs(data.frame(
      session_token = session_token(),
      HL_K       = csv_collapse(input$HL_K),
      HL_approach= input$HL_approach,
      HL_opinion = input$HL_opinion,
      HL_val1    = input$HL_val1,
      stringsAsFactors = FALSE
    ))
    
    score_data <- scoreBiodiversitySection(input, session_token())
    saveScoringData(score_data, table = "farmers_results")
    clearLocalStorageInputs(session, c("HL_K", "HL_approach", "HL_opinion", "HL_val1"))
    
    currentPage(13)
  })
  
  observeEvent(input$bio_Back, {
    lang <- input$lang %||% "French"
    currentPage(11)
  })
  
  
  
  ### Fert Page ####
  observeEvent(input$nm_Next, {
    lang <- input$lang %||% "French"
    
    if (is.null(input$NM_K) || length(input$NM_K) == 0) {
      showNotification(t("missing_newconcepts", lang), type = "error")
      return()
    }
    if (is.null(input$NM_approach)) {
      showNotification(t("missing_approach", lang), type = "error")
      return()
    }
    if (is.null(input$NM_opinion)) {
      showNotification(t("missing_opinion", lang), type = "error")
      return()
    }
    
    saveRawInputs(data.frame(
      session_token = session_token(),
      NM_K       = csv_collapse(input$NM_K),
      NM_approach= input$NM_approach,
      NM_opinion = input$NM_opinion,
      
      
      stringsAsFactors = FALSE
    ))
    
    score_data <- scoreFertilisationSection(input, session_token())
    saveScoringData(score_data, table = "farmers_results")
    
    
    
    clearLocalStorageInputs(session, c("NM_K", "NM_approach", "NM_opinion"))
    
    
    currentPage(14)
  })
  
  
  observeEvent(input$nm_Back, {
    lang <- input$lang %||% "French"
    currentPage(12)
  })
  
  ### Water Management Page ####
  
  observeEvent(input$sw_Next, {
    lang <- input$lang %||% "French"
    
    if (is.null(input$SW_K) || length(input$SW_K) == 0) {
      showNotification(t("missing_newconcepts", lang), type = "error")
      return()
    }
    if (is.null(input$SW_approach)) {
      showNotification(t("missing_approach", lang), type = "error")
      return()
    }
    if (is.null(input$SW_opinion)) {
      showNotification(t("missing_opinion", lang), type = "error")
      return()
    }
    
    saveRawInputs(data.frame(
      session_token = session_token(),
      SW_K = csv_collapse(input$SW_K),
      SW_approach = input$SW_approach,
      SW_opinion = input$SW_opinion,
      stringsAsFactors = FALSE
    ))
    
    score_data <- scoreWaterSection(input, session_token())
    saveScoringData(score_data, table = "farmers_results")
    
    
    clearLocalStorageInputs(session, c("SW_K", "SW_approach", "SW_opinion"))
    log_event(paste("Saved Page 14 (Water) for:", session_token()))
    currentPage(15)
  })
  
  
  
  ### Carbon Page ####
  observeEvent(input$dc_Next, {
    lang <- input$lang %||% "French"
    if (is.null(input$DC_K) || length(input$DC_K) == 0) {
      showNotification(t("missing_newconcepts", lang), type = "error")
      return()
    }
    if (is.null(input$DC_approach) || input$DC_approach == "") {
      showNotification(t("missing_approach", lang), type = "error")
      return()
    }
    if (is.null(input$DC_opinion) || input$DC_opinion == "") {
      showNotification(t("missing_opinion", lang), type = "error")
      return()
    }
    
    saveRawInputs(data.frame(
      
      session_token = session_token(),
      DC_K = csv_collapse(input$DC_K),
      DC_approach = input$DC_approach,
      DC_opinion = input$DC_opinion,
      stringsAsFactors = FALSE
    ))
    
    score_data <- scoreCarbonSection(input, session_token())
    saveScoringData(score_data, table = "farmers_results")
    
    clearLocalStorageInputs(session, c("DC_K", "DC_approach", "DC_opinion"))
    log_event(paste("Saved Page 15 (Carbon) for:", session_token()))
    currentPage(16)
  })
  
  
  observeEvent(input$dc_Back, {
    lang <- input$lang %||% "French"
    currentPage(14)
  })
  
  
  
  ### Threats Page ####
  
  observeEvent(input$threats_Next, {
    lang <- input$lang %||% "French"
    safe <- function(x) if (is.null(x)) NA else x
    
    saveRawInputs(data.frame(
      session_token = session_token(),
      Threat_Val_E        = safe(input$Threat_Val_E),
      Threat_Val_E_comment= safe(input$Threat_Val_E_comment),
      Threat_Val_A        = safe(input$Threat_Val_A),
      Threat_Val_A_comment= safe(input$Threat_Val_A_comment),
      Threat_Val_SD       = safe(input$Threat_Val_SD),
      Threat_Val_SD_comment = safe(input$Threat_Val_SD_comment),
      Threat_Val_S        = safe(input$Threat_Val_S),
      Threat_Val_S_comment= safe(input$Threat_Val_S_comment),
      Threat_Val_HL       = safe(input$Threat_Val_HL),
      Threat_Val_HL_comment= safe(input$Threat_Val_HL_comment),
      Threat_Val_NM       = safe(input$Threat_Val_NM),
      Threat_Val_NM_comment= safe(input$Threat_Val_NM_comment),
      Threat_Val_DC       = safe(input$Threat_Val_DC),
      Threat_Val_DC_comment= safe(input$Threat_Val_DC_comment),
      OptionalComment     = safe(input$OptionalComment),
      stringsAsFactors = FALSE
    ))
    currentPage(17)
  })
  
  observeEvent(input$threats_Back, {
    lang <- input$lang %||% "French"
    currentPage(15)
  })
  
  
  
  ### Demographics Page ####
  

  observeEvent(input$final_Back, {
    lang <- input$lang %||% "French"
    currentPage(16)
  })
  
  observeEvent(input$submitAll, {
    lang <- input$lang %||% "French"
    
    if (input$receive_results == "yes" && (is.null(input$contact_email) || input$contact_email == "")) {
      showNotification(t("missing_email", lang), type = "error")
      return()
    }
    # Demographic and contact consent page
    saveRawInputs(data.frame(
      session_token = session_token(),
      Age = input$Age,
      education_level = input$education_level,
      Land_ownership = csv_collapse(input$Land_ownership),
      Land_area = input$Land_area,
      receive_results = input$receive_results,
      contact_email = if (!is.null(input$receive_results) && input$receive_results == "yes") input$contact_email else "",
      timestamp_submit = format(Sys.time(), tz = "UTC", usetz = FALSE),
      stringsAsFactors = FALSE
    ))
    log_event(paste("Final submission by", session_token()))
    currentPage(18)
  })
  
  observeEvent(input$restartApp, {
    showNotification("🔄 Restarting...", type = "message")  
    session$sendCustomMessage("restartApp", list())
    currentPage(0)
    session_token(NULL)
  })
  
  #END
  
  
}

shinyApp(ui = ui, server = server)





