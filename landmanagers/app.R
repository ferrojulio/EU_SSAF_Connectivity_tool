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

# Functions from utils.R are used here (saveRawInputs, saveScoringData, readResults)
# No need to redefine them, just ensure they are sourced from utils.R

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
    currentPage(currentPage() + 1)
  })

  observeEvent(input$nav_next_btn, {
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
