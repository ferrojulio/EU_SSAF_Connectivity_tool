# app_Farmers.R — Multilingual ####
# 


library(shiny)
library(leaflet)
library(dplyr)
library(DBI)
library(RSQLite)
library(htmltools)
library(RPostgres)


translations <- readRDS("/srv/shiny-server/farmers/Farmers_translations.rds")

translations <- translations[!is.na(translations$unique_ID), ]



source( "/srv/shiny-server/utils.R")

t <- function(key, lang = "French") {
	t_original(key, lang, translations_df = translations)
}

checkTranslationKeys(used_translation_keys, translations)

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

# SaveData0: raw multilingual inputs
saveRawInputs <- function(data, table = dbtable) {
  # Establish DB connection
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

  # Sanitize text values
  data[] <- lapply(data, function(x) {
    if (is.character(x)) {
      x <- iconv(x, from = "", to = "UTF-8")
      x <- gsub("'", "''", x)
    }
    x
  })

  # Build INSERT query
  fields <- names(data)
  values <- sapply(data[1, ], function(x) {
    if (is.character(x)) paste0("'", x, "'") else as.character(x)
  })

  query <- sprintf("INSERT INTO %s (%s) VALUES (%s)",
                   table,
                   paste(fields, collapse = ", "),
                   paste(values, collapse = ", "))

  tryCatch({
    DBI::dbExecute(conn, query)
  }, error = function(e) {
    cat(Sys.time(), "\nQuery failed:\n", query, "\nError:\n", e$message, "\n\n",
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

  # Sanitize inputs
  data[] <- lapply(data, function(x) {
    if (is.character(x)) {
      x <- iconv(x, from = "", to = "UTF-8")
      gsub("'", "''", x)
    } else x
  })

  token <- data$session_token[1]
  fields <- setdiff(names(data), "session_token")
  if (length(fields) == 0) return(NULL)

  updates <- paste0(fields, " = '", unlist(data[1, fields]), "'", collapse = ", ")
  query <- sprintf("UPDATE %s SET %s WHERE session_token = '%s'", table, updates, token)

  tryCatch({
    DBI::dbExecute(conn, query)
  }, error = function(e) {
    cat(Sys.time(), "\nQuery failed:\n", query, "\nError:\n", e$message, "\n\n",
        file = "/srv/shiny-server/farmers/event_log.txt", append = TRUE)
    showNotification(paste("DB write error:", e$message), type = "error")
  })
}



readResults <- function(session_token, table = dbtable) {
  if (is.null(session_token) || session_token == "") return(NULL)

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

  query <- sprintf("SELECT * FROM %s WHERE session_token = '%s'", table, session_token)

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



# ===== UI =====
ui <- fluidPage(
  tags$head(
    tags$link(rel = "icon", href = "favicon.ico", type = "image/x-icon"),
    tags$link(rel = "stylesheet", href = "app.css"),
    tags$script(HTML("
      function clearLocalStorage() {
        localStorage.clear();
        location.reload();
      }

      function getFromLocalStorage(key) {
        return localStorage.getItem(key) || '';
      }

      Shiny.addCustomMessageHandler('disconnectedAlert', function(msg) {
        $(document).on('shiny:disconnected', function() {
          alert(msg);
        });
      });

      Shiny.addCustomMessageHandler('saveToLocalStore', function(msg) {
        localStorage.setItem(msg.key, msg.value);
      });

      Shiny.addCustomMessageHandler('setSessionToken', function(msg) {
        localStorage.setItem('sessionToken', msg);
      });

      Shiny.addCustomMessageHandler('updateCurrentPage', function(msg) {
        localStorage.setItem('currentPage', msg);
      });

      $(document).on('shiny:connected', function() {
        var sessionToken = getFromLocalStorage('sessionToken');
        if (sessionToken) {
          Shiny.setInputValue('restoredSessionToken', sessionToken);
        }
        var savedPage = getFromLocalStorage('currentPage');
        if (savedPage !== null) {
          Shiny.setInputValue('restoredPage', savedPage);
        }
      });

      Shiny.addCustomMessageHandler('clearCurrentInputs', function(msg) {
        const keysToRemove = msg.keys || [];
        keysToRemove.forEach(key => localStorage.removeItem(key));
      });
      
      Shiny.addCustomMessageHandler('scrollTop', function(message) {
        window.scrollTo({ top: 0, behavior: 'smooth' });
      });
    "))
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

  

  # Setup session token and page
  session_token <- NULL
  

  currentPage <- reactiveVal(1)
  
  observeEvent(input$restoredSessionToken, {
    session_token <<- input$restoredSessionToken
  })
  
  observeEvent(input$restoredPage, {
    restoredPage <- suppressWarnings(as.numeric(input$restoredPage))
    if (!is.na(restoredPage)) currentPage(restoredPage)
  })
  #### observeEvent(currentPage()####
  observeEvent(currentPage(), {
    session$sendCustomMessage("updateCurrentPage", currentPage())
    session$sendCustomMessage("setSessionToken", session_token)
    session$sendCustomMessage("scrollTop", list())
    # Send default if input$lang not yet set
    user_lang <- if (!is.null(input$lang)) input$lang else "French"
    session$sendCustomMessage("disconnectedAlert", t("disconnected_alert", user_lang))
    
    if (is.null(input$restoredSessionToken) || input$restoredSessionToken == "") {
      session_token <<- generateUserID()
    } else {
      session_token <<- input$restoredSessionToken
    }
    })
  
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
            tags$li(t("page0_info1", input$lang)),
            tags$li(t("page0_info2", input$lang)),
            tags$li(t("page0_info3", input$lang)),
            tags$li(t("page0_info4", input$lang)),
            tags$li(t("page0_info5", input$lang)),
            tags$li(t("page0_info6", input$lang)),
            tags$li(t("page0_info7", input$lang))
          ),
          HTML(sprintf("<p>%s</p>", t("page0_info8", lang))),
          checkboxInput("consent", t("consent_text", lang), value = FALSE),
          actionButton("btnStart", t("nav_start", lang), class = "btn-primary")
        )
      )
      
    
  
    
    ## PAGE 1: PRESENTATION #############################
    } else if(p == 1){
      
      
      fluidPage(
        h3(t("PRESENTATION_header", input$lang)),
        
        p(t("PRESENTATION_intro_text", input$lang)),
        radioButtons("role", t("role_Q", input$lang), 
                     choices = setNames(
                       c("farmer", "other"),
                       c(t("role_A1", input$lang), t("role_A2", input$lang))
                     ),
                     selected = input$role %||% character(0)
        ),
        
        conditionalPanel(
          condition = "input.role == 'other'",
          textInput("other_role", t("other_role_A3", input$lang))
        ),
        
        checkboxGroupInput("farmEnterprises",t("farmEnterprises_Q", input$lang),
                           choices = setNames(c(
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
                           )),
                           selected = input$farmEnterprises %||% character(0)),
        conditionalPanel(
          condition = "input.farmEnterprises.includes('Autre (précisez)')",
          textInput("other_farmEnterprises",t("other_farmEnterprises", input$lang))
        ),
        
        checkboxGroupInput("perceivedThreats",t("perceivedThreats_Q",input$lang),
                            choices = setNames(c(
                             "Érosion",
                             "Acidification",
                             "Déclin de la structure (par ex, compaction)",
                             "Perte de carbone dans le sol",
                             "Salinisation",
                             "Perte d'habitat/dégradation biologie du sol",
                             "Pollution des sols",
                             "Artificialisation des terres",
                             "Autre (précisez)"
                           ),
                           c(
                             t("perceivedThreats_A1", input$lang),
                             t("perceivedThreats_A2", input$lang),
                             t("perceivedThreats_A3", input$lang),
                             t("perceivedThreats_A4", input$lang),
                             t("perceivedThreats_A5", input$lang),
                             t("perceivedThreats_A6", input$lang),
                             t("perceivedThreats_A7", input$lang),
                             t("perceivedThreats_A8", input$lang),
                             t("perceivedThreats_A9", input$lang)
                           )),
                           selected = input$perceivedThreats %||% character(0)),
                   
        conditionalPanel(
          condition = "input.perceivedThreats.includes('Autre (précisez)')",
          textInput("other_perceivedThreats",t("other_perceivedThreats", input$lang))
        ),
        
        radioButtons("numberSoilTypes",t("numberSoilTypes_Q", input$lang),
                     choices=c("1","2-3","4+"), selected=input$numberSoilTypes %||% character(0) ),

        actionButton("PRESENTATION_Back", t("nav_back", input$lang)),
        actionButton("PRESENTATION_Next", t("nav_next", input$lang))
      )
      
      ### PAGE 2 CHAINES D'APPRO ############################
    } 
   else if (p == 2) {
     tagList(   
        
      h3(t("Supply_header", input$lang)),
      
      exclusiveCheckboxScript("supply_newconcepts", "supply_newconcept_5"),
      
      checkboxGroupInput("supply_newconcepts", 
                         t("supply_newconcepts_Q", input$lang),
                         choices = setNames(
                           paste0("supply_newconcept_", 1:5),
                           c(
                             t("supply_newconcepts_A1", input$lang),
                             t("supply_newconcepts_A2", input$lang),
                             t("supply_newconcepts_A3", input$lang),
                             t("supply_newconcepts_A4", input$lang),
                             t("supply_newconcepts_A5", input$lang)
                           )
                         ),
                         selected = input$supply_newconcepts %||% character(0)
      ),
      
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
   } 
    else if (p == 3) {
     fluidPage(
       h3(t("food_header", input$lang)),
       
       # Disable others if "I'm familiar with all" is selected
       exclusiveCheckboxScript("food_newconcepts", "food_newconcept_5"),
       
       
       checkboxGroupInput("food_newconcepts",
                          t("food_newconcepts_Q", input$lang),
                          choices = setNames(
                            paste0("food_newconcept_", 1:5),
                            c(
                              t("food_newconcepts_A1", input$lang),
                              t("food_newconcepts_A2", input$lang),
                              t("food_newconcepts_A3", input$lang),
                              t("food_newconcepts_A4", input$lang),
                              t("food_newconcepts_A5", input$lang)
                            )
                          ),
                          selected = input$food_newconcepts %||% character(0)
       ),
       
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
                    selected = input$food_approach %||% character(0)
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
                    selected = input$food_opinion %||% character(0)
       ),
       
       
       actionButton("food_Back", t("nav_back", input$lang)),
       actionButton("food_Next", t("nav_next", input$lang))
     )
      ### PAGE 4: econ ############################
   }
      
    else if(p == 4){
      fluidPage(
        h3(t("econ_header", input$lang)),
        
        exclusiveCheckboxScript("econ_newconcepts", "econ_newconcepts_5"),
        
        checkboxGroupInput("econ_newconcepts", 
                           t("econ_newconcepts_Q", input$lang),
                           choices = setNames(
                             paste0("econ_newconcepts_", 1:5),
                             c(
                               t("econ_newconcepts_A1", input$lang),
                               t("econ_newconcepts_A2", input$lang),
                               t("econ_newconcepts_A3", input$lang),
                               t("econ_newconcepts_A4", input$lang),
                               t("econ_newconcepts_A5", input$lang)
                             )
                           ),
                           selected = input$econ_newconcepts %||% character(0)
        ),
        
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
                     selected = input$econ_approach %||% character(0)
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
                     selected = input$econ_opinion %||% character(0)
        ),
        
        
        actionButton("econ_Back", t("nav_back", input$lang)),
        actionButton("econ_Next", t("nav_next", input$lang))
      )
    
      
      ### PAGE 5: SÉCURITÉ INTÉRIEURE ET ORDRE PUBLIC############################
    } 
    else if (p == 5) {
      fluidPage(
        h3(t("intSec_header", input$lang)),
        
        exclusiveCheckboxScript("intSec_newconcepts", "intSec_newconcept_5"),
        
        checkboxGroupInput("intSec_newconcepts",
                           t("intSec_newconcepts_Q", input$lang),
                           choices = setNames(
                             paste0("intSec_newconcept_", 1:5),
                             c(
                               t("intSec_newconcepts_A1", input$lang),
                               t("intSec_newconcepts_A2", input$lang),
                               t("intSec_newconcepts_A3", input$lang),
                               t("intSec_newconcepts_A4", input$lang),
                               t("intSec_newconcepts_A5", input$lang)
                             )
                           ),
                           selected = input$intSec_newconcepts %||% character(0)
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
                     selected = input$intSec_approach %||% character(0)
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
                     selected = input$intSec_opinion %||% character(0)
        ),
        
        radioButtons("intSec_validation",
                     t("intSec_validation", input$lang),
                     choices = setNames(c("YES", "NO"),
                                        c(t("intSec_validation_A1", input$lang),
                                          t("intSec_validation_A2", input$lang))),
                     selected = input$intSec_validation %||% character(0)
        ),
        
        textAreaInput("intSec_validation_A3", t("intSec_validation_", input$lang),
                      placeholder = t("comment_placeholder", input$lang),
                      width = "100%", height = "100px",
                      value = input$intSec_validation_A3 %||% ""),
        
        actionButton("intSec_Back", t("nav_back", input$lang)),
        actionButton("intSec_Next", t("nav_next", input$lang))
      )
    
      ### PAGE 6: DÉFENSE NATIONALE ############################
    } 
    else if (p == 6) {
      fluidPage(
        h3(t("defense_header", input$lang)),
        
        exclusiveCheckboxScript("defense_newconcepts", "defense_newconcepts_5"),
        
        checkboxGroupInput("defense_newconcepts", 
                           t("defense_newconcepts_Q", input$lang),
                           choices = setNames(
                             paste0("defense_newconcepts_", 1:5),
                             c(
                               t("defense_newconcepts_A1", input$lang),
                               t("defense_newconcepts_A2", input$lang),
                               t("defense_newconcepts_A3", input$lang),
                               t("defense_newconcepts_A4", input$lang),
                               t("defense_newconcepts_A5", input$lang)
                             )
                           ),
                           selected = input$defense_newconcepts %||% character(0)
        ),
        
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
                     selected = input$defense_approach %||% character(0)
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
                     selected = input$defense_opinion %||% character(0)
        ),
        
        radioButtons("defense_validation",
                     t("defense_validation_Q", input$lang),
                     choices = setNames(c("YES", "NO"),
                                        c(t("intSec_validation_A1", input$lang),
                                          t("intSec_validation_A2", input$lang))),
                     selected = input$defense_validation %||% character(0)
        ),
        
        textAreaInput("defense_validation2", 
                      label = t("intSec_validation_", input$lang),
                      placeholder = t("comment_placeholder", input$lang),
                      width = "100%", height = "100px",
                      value = input$commentaires %||% ""
        ),
        
        actionButton("defense_Back", t("nav_back", input$lang)),
        actionButton("defense_Next", t("nav_next", input$lang))
      )
    
      ##PAGE 7: map ####
      
    } 
    
    
    else if (p == 7) {
    
    lang <- if (!is.null(input$lang)) input$lang else "French"
    
    
    observeEvent(input$country, {
      loc <- commune_centroids %>% filter(country == input$country)
      if (nrow(loc) > 0) {
        leafletProxy("soilMap") %>%
          setView(lng = mean(loc$lon, na.rm = TRUE), lat = mean(loc$lat, na.rm = TRUE), zoom = 6)
      }
    })
    
    observeEvent(input$region2, {
      loc <- commune_centroids %>% filter(region2 == input$region2)
      if (nrow(loc) == 1) {
        leafletProxy("soilMap") %>%
          setView(lng = loc$lon, lat = loc$lat, zoom = 8)
      }
    })
    
    
    output$soilMap <- renderLeaflet({
      leaflet(options = leafletOptions(scrollWheelZoom = TRUE, zoomControl = FALSE)) %>%
        addProviderTiles("Esri.WorldStreetMap", group = "Street Map") %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite Image") %>%
        addProviderTiles("CartoDB.PositronOnlyLabels", group = "Satellite Image") %>%
        
        setView(lng = 10, lat = 48, zoom = 4) %>%
        htmlwidgets::onRender("
      function(el, x) {
        L.control.zoom({position: 'bottomleft'}).addTo(this);
      }
    ") %>%
        addEasyButton(easyButton(
          icon = "fa-crosshairs",
          title = "Locate Me",
          onClick = JS("function(btn, map){ map.locate({setView: true}); }")
        )) %>%
        addLayersControl(
          baseGroups = c("Street Map", "Satellite Image"),
          options = layersControlOptions(collapsed = FALSE)
        )
    })
    
    
    output$clicked_location <- renderPrint({
      if (!is.null(input$soilMap_click)) {
        paste("Coordonnées cliquées :",
              round(input$soilMap_click$lat, 4), ",",
              round(input$soilMap_click$lng, 4))
      }
    })
    
    observeEvent(input$soilMap_click, {
      lat <- input$soilMap_click$lat
      lng <- input$soilMap_click$lng
      
      leafletProxy("soilMap") %>%
        clearMarkers() %>%  # remove previous marker
        addMarkers(lng = lng, lat = lat, popup = paste(round(lat, 4), round(lng, 4)))
    })
    
    output$Soil_Type_Description_count <- renderText({
      n <- nchar(input$Soil_Type_Description %||% "")
      paste0(n, "/500")
    })
    
    fluidPage(
      h3(t("map_header", input$lang)),
      
      radioButtons("numberSoilTypes",
                   t("numberSoilTypes_Q", input$lang),
                   choices=c("1","2-3","4+"), selected=(0)),
      
      p(t("Soil_focus_text", input$lang)),
      h4(t("Map_instruction", input$lang)),
      countryRegionUI(),
      leafletOutput("soilMap", height = "500px"),
      verbatimTextOutput(t("map_selected_location", lang)),
      br(),
      textInput2("Soil_Type_Description", t("Soil_Type_Description_Q", input$lang), width = "100%", maxlength = 500),
      textOutput("Soil_Type_Description_count"),
      br(),
      actionButton("map_Back", t("nav_back", input$lang)), 
      actionButton("map_Next", t("nav_next", input$lang))
    )
    
    ### PAGE 8: SOL (EROSION) ############################
  }
    
    
    else if (p == 8) {
      fluidPage(
        h3(t("erosion_header", input$lang)),
        p(t("erosion_intro", input$lang)),
        
        
        output$miniMap <- renderLeaflet({
          renderMiniMap(input)
        }),
        
        if (!is.null(input$Soil_Type_Description) && input$Soil_Type_Description != "") {
          div(style = "margin-bottom: 10px; font-style: italic; color: #333;",
              paste(t("your_soil_description", input$lang), ":", input$Soil_Type_Description))
        },
        
        exclusiveCheckboxScript("E_K", "E_K_5"),
        
        checkboxGroupInput("E_K",
                           t("E_K_Q", input$lang),
                           choices = setNames(
                             paste0("E_K_", 1:5),
                             c(
                               t("E_K_A1", input$lang),
                               t("E_K_A2", input$lang),
                               t("E_K_A3", input$lang),
                               t("E_K_A4", input$lang),
                               t("E_K_A5", input$lang)
                             )
                           ),
                           selected = input$E_K %||% character(0)
        ),
        
        radioButtons("E_approach",
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
                     selected = input$E_approach %||% character(0)
        ),
        
        radioButtons("E_opinion",
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
                     selected = input$E_opinion %||% character(0)
        ),
        
        radioButtons("E_legislation",
                     t("E_legislation_Q", input$lang),
                     choices = setNames(c("OUI", "NON"), c(t("yes", input$lang), t("no", input$lang))),
                     selected = input$E_legislation %||% character(0)
        ),
        
        actionButton("erosion_Back", t("nav_back", input$lang)),
        actionButton("erosion_Next", t("nav_next", input$lang))
      )
    
      ### PAGE 9: ACIDIFICATION ############################
    } 
    else if (p == 9) {
      fluidPage(
        h3(t("acid_header", input$lang)),
      
        if (!is.null(input$Soil_Type_Description) && input$Soil_Type_Description != "") {
          div(style = "margin-bottom: 10px; font-style: italic; color: #333;",
              paste(t("your_soil_description", input$lang), ":", input$Soil_Type_Description))
        },
        
        if (!is.null(input$soilMap_click$lat) && !is.null(input$soilMap_click$lng)) {
          tags$img(
            src = sprintf(
              "https://api.mapbox.com/styles/v1/mapbox/satellite-v9/static/%f,%f,15/600x300?access_token=YOUR_MAPBOX_TOKEN",
              input$soilMap_click$lng, input$soilMap_click$lat
            ),
            style = "width: 100%; margin-bottom: 15px;",
            alt = "Vue satellite de votre emplacement"
          )
        },
        
        exclusiveCheckboxScript("A_K", "A_K_5"),
      
        
        checkboxGroupInput("A_K",
                           t("A_K_Q", input$lang),
                           choices = setNames(
                             paste0("A_K_", 1:5),
                             c(
                               t("A_K_A1", input$lang),
                               t("A_K_A2", input$lang),
                               t("A_K_A3", input$lang),
                               t("A_K_A4", input$lang),
                               t("A_K_A5", input$lang)
                             )
                           ),
                           selected = input$A_K %||% character(0)
        ),
        
        radioButtons("A_approach",
                     t("A_approach_Q", input$lang),
                     choices = setNames(
                       paste0("A_approach_", 1:4),
                       c(
                         t("A_approach_A1", input$lang),
                         t("A_approach_A2", input$lang),
                         t("A_approach_A3", input$lang),
                         t("A_approach_A4", input$lang)
                       )
                     ),
                     selected = input$A_approach %||% character(0)
        ),
        
        radioButtons("A_opinion",
                     t("A_opinion_Q", input$lang),
                     choices = setNames(
                       paste0("A_opinion_", 1:4),
                       c(
                         t("A_opinion_A1", input$lang),
                         t("A_opinion_A2", input$lang),
                         t("A_opinion_A3", input$lang),
                         t("A_opinion_A4", input$lang)
                       )
                     ),
                     selected = input$A_opinion %||% character(0)
        ),
        
        radioButtons("A_pH",
                     t("A_pH_Q", input$lang),
                     choices = setNames(
                       c("pH < 5.5", "pH 5.6 à 7.5", "> pH 7.6", "Je ne sais pas"),
                       c(
                         t("A_pH_A1", input$lang),
                         t("A_pH_A2", input$lang),
                         t("A_pH_A3", input$lang),
                         t("A_pH_A4", input$lang)
                       )
                     ),
                     selected = input$A_pH %||% character(0)
        ),
        
        actionButton("acid_Back", t("nav_back", input$lang)),
        actionButton("acid_Next", t("nav_next", input$lang))
      )
    
      ### PAGE 10: STRUCTURE DU SOL############################
    } 
    else if (p == 10) {
      fluidPage(
        h3(t("acid_header", input$lang)),
        
        exclusiveCheckboxScript("A_K", "A_K_5"),
        
        checkboxGroupInput("A_K",
                           t("A_K_Q", input$lang),
                           choices = setNames(
                             paste0("A_K_", 1:5),
                             c(
                               t("A_K_A1", input$lang),
                               t("A_K_A2", input$lang),
                               t("A_K_A3", input$lang),
                               t("A_K_A4", input$lang),
                               t("A_K_A5", input$lang)
                             )
                           ),
                           selected = input$A_K %||% character(0)
        ),
        
        radioButtons("A_approach",
                     t("A_approach_Q", input$lang),
                     choices = setNames(
                       paste0("A_approach_", 1:4),
                       c(
                         t("A_approach_A1", input$lang),
                         t("A_approach_A2", input$lang),
                         t("A_approach_A3", input$lang),
                         t("A_approach_A4", input$lang)
                       )
                     ),
                     selected = input$A_approach %||% character(0)
        ),
        
        radioButtons("A_opinion",
                     t("A_opinion_Q", input$lang),
                     choices = setNames(
                       paste0("A_opinion_", 1:4),
                       c(
                         t("A_opinion_A1", input$lang),
                         t("A_opinion_A2", input$lang),
                         t("A_opinion_A3", input$lang),
                         t("A_opinion_A4", input$lang)
                       )
                     ),
                     selected = input$A_opinion %||% character(0)
        ),
        
        radioButtons("A_pH",
                     t("A_pH_Q", input$lang),
                     choices = setNames(
                       c("pH < 5.5", "pH 5.6 à 7.5", "> pH 7.6", "Je ne sais pas"),
                       c(
                         t("A_pH_A1", input$lang),
                         t("A_pH_A2", input$lang),
                         t("A_pH_A3", input$lang),
                         t("A_pH_A4", input$lang)
                       )
                     ),
                     selected = input$A_pH %||% character(0)
        ),
        
        actionButton("acid_Back", t("nav_back", input$lang)),
        actionButton("acid_Next", t("nav_next", input$lang))
      )
    
      
      ### PAGE 11: SALINISATION ############################
    } else if (p == 11) {
      tagList(
        h3(t("sal_header", input$lang)),
        p(t("sal_intro", input$lang)),
        
        exclusiveCheckboxScript("S_K", "S_K_5"),
        checkboxGroupInput("S_K", t("S_K_Q", input$lang),
                           choices = setNames(paste0("S_K_", 1:5),
                                              t(paste0("S_K_A", 1:5), input$lang)),
                           selected = input$S_K %||% character(0)),
        
        radioButtons("S_approach", t("S_approach_Q", input$lang),
                     choices = setNames(paste0("S_approach_", 1:4),
                                        t(paste0("S_approach_A", 1:4), input$lang)),
                     selected = input$S_approach %||% character(0)),
        
        radioButtons("S_opinion", t("S_opinion_Q", input$lang),
                     choices = setNames(paste0("S_opinion_", 1:4),
                                        t(paste0("S_opinion_A", 1:4), input$lang)),
                     selected = input$S_opinion %||% character(0)),
        
        radioButtons("S_type", t("S_type_Q", input$lang),
                     choices = setNames(paste0("S_type_", 1:6),
                                        t(paste0("S_type_A", 1:6), input$lang)),
                     selected = input$S_type %||% character(0)),
        
        actionButton("sal_Back", t("nav_back", input$lang)),
        actionButton("sal_Next", t("nav_next", input$lang))
      )
      
      ### PAGE 12: BIODIVERSITÉ DU SOL ############################
    } else if (p == 12) {
      tagList(
        h3(t("bio_header", input$lang)),
        p(t("bio_intro", input$lang)),
        
        exclusiveCheckboxScript("HL_K", "HL_K_5"),
        checkboxGroupInput("HL_K", t("HL_K_Q", input$lang),
                           choices = setNames(paste0("HL_K_", 1:5),
                                              t(paste0("HL_K_A", 1:5), input$lang)),
                           selected = input$HL_K %||% character(0)),
        
        radioButtons("HL_approach", t("HL_approach_Q", input$lang),
                     choices = setNames(paste0("HL_approach_", 1:4),
                                        t(paste0("HL_approach_A", 1:4), input$lang)),
                     selected = input$HL_approach %||% character(0)),
        
        radioButtons("HL_opinion", t("HL_opinion_Q", input$lang),
                     choices = setNames(paste0("HL_opinion_", 1:4),
                                        t(paste0("HL_opinion_A", 1:4), input$lang)),
                     selected = input$HL_opinion %||% character(0)),
        
        checkboxGroupInput("HL_val1", t("HL_val1_Q", input$lang),
                           choices = setNames(paste0("HL_val1_", 1:7),
                                              t(paste0("HL_val1_A", 1:7), input$lang)),
                           selected = input$HL_val1 %||% character(0)),
        
        radioButtons("HL_val2", t("HL_val2_Q", input$lang),
                     choices = setNames(c("YES", "NO"),
                                        c(t("yes", input$lang), t("no", input$lang))),
                     selected = input$HL_val2 %||% character(0)),
        
        actionButton("bio_Back", t("nav_back", input$lang)),
        actionButton("bio_Next", t("nav_next", input$lang))
      )
      
      ### PAGE 13: FERTILISATION ############################
    } else if (p == 13) {
      tagList(
        h3(t("NM_header", input$lang)),
        p(t("NM_intro", input$lang)),
        
        exclusiveCheckboxScript("NM_K", "NM_K_5"),
        checkboxGroupInput("NM_K", t("NM_K_Q", input$lang),
                           choices = setNames(paste0("NM_K_", 1:5),
                                              t(paste0("NM_K_A", 1:5), input$lang)),
                           selected = input$NM_K %||% character(0)),
        
        radioButtons("NM_approach", t("NM_approach_Q", input$lang),
                     choices = setNames(paste0("NM_approach_", 1:4),
                                        t(paste0("NM_approach_A", 1:4), input$lang)),
                     selected = input$NM_approach %||% character(0)),
        
        radioButtons("NM_opinion", t("NM_opinion_Q", input$lang),
                     choices = setNames(paste0("NM_opinion_", 1:4),
                                        t(paste0("NM_opinion_A", 1:4), input$lang)),
                     selected = input$NM_opinion %||% character(0)),
        
        checkboxGroupInput("NM_forms", t("NM_forms_Q", input$lang),
                           choices = setNames(paste0("NM_forms_", 1:4),
                                              t(paste0("NM_forms_A", 1:4), input$lang)),
                           selected = input$NM_forms %||% character(0)),
        
        radioButtons("NM_system", t("NM_system_Q", input$lang),
                     choices = setNames(paste0("NM_system_", 1:2),
                                        t(paste0("NM_system_A", 1:2), input$lang)),
                     selected = input$NM_system %||% character(0)),
        
        actionButton("nm_Back", t("nav_back", input$lang)),
        actionButton("nm_Next", t("nav_next", input$lang))
      )
    
    
      
      ### PAGE 14: WATER MANAGEMENT ############################################
    } else if (p == 14) {
      tagList(
        h3(t("SW_header", input$lang)),
        p(t("SW_intro", input$lang)),
        
        exclusiveCheckboxScript("SW_K", "SW_K_5"),
        checkboxGroupInput("SW_K", t("SW_K_Q", input$lang),
                           choices = setNames(paste0("SW_K_", 1:5),
                                              t(paste0("SW_K_A", 1:5), input$lang)),
                           selected = input$SW_K %||% character(0)),
        
        radioButtons("SW_approach", t("SW_approach_Q", input$lang),
                     choices = setNames(paste0("SW_approach_", 1:4),
                                        t(paste0("SW_approach_A", 1:4), input$lang)),
                     selected = input$SW_approach %||% character(0)),
        
        radioButtons("SW_opinion", t("SW_opinion_Q", input$lang),
                     choices = setNames(paste0("SW_opinion_", 1:4),
                                        t(paste0("SW_opinion_A", 1:4), input$lang)),
                     selected = input$SW_opinion %||% character(0)),
        
        radioButtons("SW_probleme", t("SW_probleme_Q", input$lang),
                     choices = setNames(paste0("SW_probleme_", 1:5),
                                        t(paste0("SW_probleme_A", 1:5), input$lang)),
                     selected = input$SW_probleme %||% character(0)),
        
        actionButton("sw_Back", t("nav_back", input$lang)),
        actionButton("sw_Next", t("nav_next", input$lang))
      )
    
      
      ### PAGE 15: CARBON ######################################################
    } else if (p == 15) {
      tagList(
        h3(t("DC_header", input$lang)),
        exclusiveCheckboxScript("DC_K", "DC_K_5"),
        checkboxGroupInput("DC_K", t("DC_K_Q", input$lang),
                           choices = setNames(paste0("DC_K_", 1:5),
                                              t(paste0("DC_K_A", 1:5), input$lang)),
                           selected = input$DC_K %||% character(0)),
        radioButtons("DC_approach", t("DC_approach_Q", input$lang),
                     choices = setNames(paste0("DC_approach_", 1:4),
                                        t(paste0("DC_approach_A", 1:4), input$lang)),
                     selected = input$DC_approach %||% character(0)),
        radioButtons("DC_opinion", t("DC_opinion_Q", input$lang),
                     choices = setNames(paste0("DC_opinion_", 1:4),
                                        t(paste0("DC_opinion_A", 1:4), input$lang)),
                     selected = input$DC_opinion %||% character(0)),
        checkboxGroupInput("DC_practices", t("DC_practices_Q", input$lang),
                           choices = setNames(paste0("DC_practices_", 1:6),
                                              t(paste0("DC_practices_A", 1:6), input$lang)),
                           selected = input$DC_practices %||% character(0)),
        radioButtons("DC_carbon_credits", t("DC_carbon_credits_Q", input$lang),
                     choices = setNames(paste0("DC_carbon_credits_", 1:7),
                                        t(paste0("DC_carbon_credits_A", 1:7), input$lang)),
                     selected = input$DC_carbon_credits %||% character(0)),
        actionButton("dc_Back", t("nav_back", input$lang)),
        actionButton("dc_Next", t("nav_next", input$lang))
      )
    
      
      ### PAGE 16: THREATS #####################################################
    } else if (p == 16) {
      tagList(
        h3(t("Threats_header", input$lang)),
        p(t("Threats_intro", input$lang)),
        radioButtons("Threat_Val_E", t("Threat_Val_E_Q", input$lang),
                     choices = t(paste0("Threat_Level_", 1:6), input$lang),
                     selected = input$Threat_Val_E %||% character(0)),
        textInput("Threat_Val_E_comment", t("Threat_Val_comment", input$lang), ""),
        radioButtons("Threat_Val_A", t("Threat_Val_A_Q", input$lang),
                     choices = t(paste0("Threat_Level_", 1:6), input$lang),
                     selected = input$Threat_Val_A %||% character(0)),
        textInput("Threat_Val_A_comment", t("Threat_Val_comment", input$lang), ""),
        actionButton("threats_Back", t("nav_back", input$lang)),
        actionButton("threats_Next", t("nav_next", input$lang))
      )
    
      ### PAGE 17: DEMOGRAPHICS ################################################
    } else if (p == 17) {
      tagList(
        h3(t("Demographics_header", input$lang)),
        radioButtons("Age", t("Age_Q", input$lang),
                     choices = t(paste0("Age_", 1:8), input$lang),
                     selected = input$Age %||% character(0)),
        radioButtons("education_level", t("education_level_Q", input$lang),
                     choices = t(paste0("education_", 1:8), input$lang),
                     selected = input$education_level %||% character(0)),
        checkboxGroupInput("Land_ownership", t("Land_ownership_Q", input$lang),
                           choices = t(paste0("Land_", 1:6), input$lang),
                           selected = input$Land_ownership %||% character(0)),
        numericInput("Land_area", t("Land_area_Q", input$lang), value = NULL, min = 0),
        actionButton("demo_Back", t("nav_back", input$lang)),
        actionButton("demo_Next", t("nav_next", input$lang))
      )
      
      
      ### PAGE 18: FINAL SUBMISSION ############################
    } else if(p == 18){
      fluidPage(
        h3("Soumission des réponses"),
        p("Cliquez ci-dessous pour enregistrer dans farmer_responses.csv."),
        actionButton("submitAll","Enregistrer & Voir Résultats", class="btn-success"),
        actionButton("p17Back","Retour")
      )
      
      ### PAGE 19: SUBMIT PAGE #################################################
    } else if (p == 19) {
      tagList(
        h3(t("Submit_header", input$lang)),
        p(t("Submit_info", input$lang)),
        actionButton("submitAll", t("submit_button", input$lang), class = "btn-success"),
        actionButton("p18_Back", t("nav_back", input$lang))
      )
    
  
  ### PAGE 20: FINAL PAGE ##################################################
  } else if (p == 20) {
  tagList(
    h3(t("Final_header", input$lang)),
    p(t("Final_text", input$lang)),
    actionButton("restartApp", t("restart", input$lang))
  )
}
  })





  observeRegionSelection(input, output, session, input$lang)

  
  
  
  
  ##Buttons####
  
  
  observeEvent(input$btnStart, {
    req(input$consent)
    
    data_row <- data.frame(
      session_token = session_token,
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      language = input$lang,
      stringsAsFactors = FALSE
    )
    
    if (mode == "offline") {
      saveRawInputs(data_row)
    }
    
    currentPage(1)
  })
  
  ###Page PRESENTATION ####


  observeEvent(input$PRESENTATION_Next, {
    lang <- input$lang %||% "French"
    
    if (is.null(input$role) || !(input$role %in% c("farmer", "other"))) {
      showNotification(t("missing_role", lang), type = "error")
      return()
    }
    
    if (is.null(input$farmEnterprises) || length(input$farmEnterprises) == 0) {
      showNotification(t("missing_farmEnterprises", lang), type = "error")
      return()
    }
    
    if (is.null(input$perceivedThreats) || length(input$perceivedThreats) == 0) {
      showNotification(t("missing_perceivedThreats", lang), type = "error")
      return()
    }
    

    
    data_row <- data.frame(

      role = input$role %||% "",
      farm_enterprises = csv_collapse(input$farmEnterprises),
      perceived_threats = csv_collapse(input$perceivedThreats),

      stringsAsFactors = FALSE
    )
    
   saveRawInputs(data_row)
    
    clearLocalStorageInputs(session, c(
     "role", "farm_enterprises", "perceived_threats", "supply_opinion"
    ))
    
    log_event(paste("Updated Page 2 for:", session_token))
    currentPage(2)
  })
  

  
  ###page Supply #####
  
  observeEvent(input$Supply_Back, {
    currentPage(1)
  })
  
  observeEvent(input$Supply_Next, {
    lang <- input$lang %||% "French"
    
    # Optional validation: make sure something is selected (or warn)
    if (is.null(input$supply_newconcepts) || length(input$supply_newconcepts) == 0) {
      showNotification(t("missing_supply_newconcepts", lang), type = "error")
      return()
    }
    if (is.null(input$supply_approach) || input$supply_approach == "") {
      showNotification(t("missing_supply_approach", lang), type = "error")
      return()
    }
    if (is.null(input$supply_opinion) || input$supply_opinion == "") {
      showNotification(t("missing_supply_opinion", lang), type = "error")
      return()
    }
    
    data_row <- data.frame(
      
      supply_newconcepts = csv_collapse(input$supply_newconcepts),
      supply_approach = input$supply_approach,
      supply_opinion = input$supply_opinion,
      stringsAsFactors = FALSE
    )
    
    saveRawInputs(data_row)
    clearLocalStorageInputs(session, c(
      "supply_newconcepts", "supply_approach", "supply_opinion"
    ))
    log_event(paste("Updated Page 3 for:", session_token))
    currentPage(3)
  })
  
  ###page food #####
  observeEvent(input$food_Back, {
    currentPage(2)
  })
  
  observeEvent(input$food_Next, {
    lang <- input$lang %||% "French"
    
    if (is.null(input$food_newconcepts) || length(input$food_newconcepts) == 0) {
      showNotification(t("missing_food_newconcepts", lang), type = "error")
      return()
    }
    if (is.null(input$food_approach) || input$food_approach == "") {
      showNotification(t("missing_food_approach", lang), type = "error")
      return()
    }
    if (is.null(input$food_opinion) || input$food_opinion == "") {
      showNotification(t("missing_food_opinion", lang), type = "error")
      return()
    }
   
    
    data_row <- data.frame(
      
      food_newconcepts = csv_collapse(input$food_newconcepts),
      food_approach = input$food_approach,
      food_opinion = input$food_opinion,
      stringsAsFactors = FALSE
    )
    
   saveRawInputs(data_row)
    
    clearLocalStorageInputs(session, c(
      "food_newconcepts", "food_approach", "food_opinion"
    ))
    
    log_event(paste("Updated Page 4 for:", session_token))
    currentPage(4)
  })
  
  ###page econ #####
  observeEvent(input$econ_Next, {
    lang <- input$lang %||% "French"
    
    if (is.null(input$econ_newconcepts) || length(input$econ_newconcepts) == 0) {
      showNotification(t("missing_econ_newconcepts", lang), type = "error")
      return()
    }
    if (is.null(input$econ_approach) || input$econ_approach == "") {
      showNotification(t("missing_econ_approach", lang), type = "error")
      return()
    }
    if (is.null(input$econ_opinion) || input$econ_opinion == "") {
      showNotification(t("missing_econ_opinion", lang), type = "error")
      return()
    }
    if (is.null(input$econ_validation_opinion) || input$econ_validation_opinion == "") {
      showNotification(t("missing_econ_validation_opinion", lang), type = "error")
      return()
    }
    
  
      data_row <- data.frame(
        
        econ_newconcepts = paste(input$econ_newconcepts, collapse = ";"),
        econ_approach = input$econ_approach,
        econ_opinion = input$econ_opinion,
        econ_validation_opinion = input$econ_validation_opinion,
        stringsAsFactors = FALSE
      )
      saveRawInputs(data_row)
    
    clearLocalStorageInputs(session, c(
      "econ_newconcepts", "econ_approach", "econ_opinion", "econ_validation_opinion"
    ))
    log_event(paste("Saved Page econ for:", session_token))
    currentPage(5)
  })
  
  observeEvent(input$econ_Back, {
    currentPage(3)
  })

  ###Page intSec ####
  observeEvent(input$intSec_Next, {
    lang <- input$lang %||% "French"
    
    if (is.null(input$intSec_newconcepts) || length(input$intSec_newconcepts) == 0) {
      showNotification(t("missing_intSec_newconcepts", lang), type = "error")
      return()
    }
    if (is.null(input$intSec_approach) || input$intSec_approach == "") {
      showNotification(t("missing_intSec_approach", lang), type = "error")
      return()
    }
    if (is.null(input$intSec_opinion) || input$intSec_opinion == "") {
      showNotification(t("missing_intSec_opinion", lang), type = "error")
      return()
    }
    if (is.null(input$intSec_validation) || input$intSec_validation == "") {
      showNotification(t("missing_intSec_validation", lang), type = "error")
      return()
    }
    
    if (mode == "offline") {
      data_row <- data.frame(
        intSec_newconcepts = paste(input$intSec_newconcepts, collapse = ";"),
        intSec_approach = input$intSec_approach,
        intSec_opinion = input$intSec_opinion,
        intSec_validation = input$intSec_validation,
        intSec_validation_comment = input$intSec_validation_A3 %||% "",
        stringsAsFactors = FALSE
      )
      saveRawInputs(data_row)
    }
    
    clearLocalStorageInputs(session, c(
      "intSec_newconcepts", "intSec_approach", "intSec_opinion",
      "intSec_validation", "intSec_validation_A3"
    ))
    
    log_event(paste("Saved Page 5 (intSec) for:", session_token))
    currentPage(6)
  })
  
  observeEvent(input$intSec_Back, {
    currentPage(4)
  })
  
  
  ### defense ####
  observeEvent(input$defense_Next, {
    lang <- input$lang %||% "French"
    
    if (is.null(input$defense_newconcepts) || length(input$defense_newconcepts) == 0) {
      showNotification(t("missing_defense_newconcepts", lang), type = "error")
      return()
    }
    if (is.null(input$defense_approach) || input$defense_approach == "") {
      showNotification(t("missing_defense_approach", lang), type = "error")
      return()
    }
    if (is.null(input$defense_opinion) || input$defense_opinion == "") {
      showNotification(t("missing_defense_opinion", lang), type = "error")
      return()
    }
    if (is.null(input$defense_validation) || input$defense_validation == "") {
      showNotification(t("missing_defense_validation", lang), type = "error")
      return()
    }
    
   
      data_row <- data.frame(
        defense_newconcepts = paste(input$defense_newconcepts, collapse = ";"),
        defense_approach = input$defense_approach,
        defense_opinion = input$defense_opinion,
        defense_validation = input$defense_validation,
        defense_validation_comment = input$defense_validation2 %||% "",
        stringsAsFactors = FALSE
      )
      saveRawInputs(data_row)
    
    
    clearLocalStorageInputs(session, c(
      "defense_newconcepts", "defense_approach", "defense_opinion",
      "Defense_validation", "commentaires"
    ))
    
    log_event(paste("Saved Page 6 (defense) for:", session_token))
    currentPage(7)
  })
  
  observeEvent(input$defense_Back, {
    currentPage(5)
  })
  
  
  
  
  ###Map Page ####
  ###
  observeEvent(input$map_Back, {
    currentPage(6)
  })
  
  observeEvent(input$map_Next, {
    lang <- input$lang %||% "French"
    req(input$lang, input$country, input$region2, input$Soil_Type_Description, input$numberSoilTypes)
    
    if (is.null(input$country) || input$country == "") {
      showNotification("Veuillez sélectionner un pays.", type = "error")
      return()
    }
    if (is.null(input$region2) || input$region2 == "") {
      showNotification("Veuillez sélectionner une commune.", type = "error")
      return()
    }
    
    
    lat <- input$soilMap_click$lat %||% NA
    lon <- input$soilMap_click$lng %||% NA
    
    if (is.null(input$Soil_Type_Description) || input$Soil_Type_Description == "") {
      showNotification(t("missing_Soil_Type_Description", lang), type = "error")
      return()
    }
    
    if (is.null(input$numberSoilTypes) || input$numberSoilTypes == "") {
      showNotification(t("missing_numberSoilTypes", lang), type = "error")
      return()
    }
    
    data_row <- data.frame(
      number_soil_types = input$numberSoilTypes %||% "",
      timestamp_map = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      language = input$lang,
      country = input$country,
      region2 = input$region2,
      Soil_Type_Description = input$Soil_Type_Description %||% "",
      lat = lat,
      lon = lon,
      stringsAsFactors = FALSE
    )
    
    saveRawInputs(data_row)
    
    log_event(paste("Saved Page 1 data for:", session_token))
    currentPage(8)
  })
  
  ### Page Erosion ####
  observeEvent(input$erosion_Next, {
    lang <- input$lang %||% "French"
    
    if (is.null(input$E_K) || length(input$E_K) == 0) {
      showNotification(t("missing_E_K", lang), type = "error")
      return()
    }
    if (is.null(input$E_approach) || input$E_approach == "") {
      showNotification(t("missing_E_approach", lang), type = "error")
      return()
    }
    if (is.null(input$E_opinion) || input$E_opinion == "") {
      showNotification(t("missing_E_opinion", lang), type = "error")
      return()
    }
    if (is.null(input$E_legislation) || input$E_legislation == "") {
      showNotification(t("missing_E_legislation", lang), type = "error")
      return()
    }
    
    data_row <- data.frame(
      E_K = paste(input$E_K, collapse = ";"),
      E_approach = input$E_approach,
      E_opinion = input$E_opinion,
      E_legislation = input$E_legislation,
      stringsAsFactors = FALSE
    )
    
     saveRawInputs(data_row)
    clearLocalStorageInputs(session, c("E_K", "E_approach", "E_opinion", "E_legislation"))
    log_event(paste("Saved Page 8 (Erosion) for:", session_token))
    currentPage(9)
  })
  
  observeEvent(input$erosion_Back, {
    currentPage(7)
  })
  
  ### Page Acidification ####
  observeEvent(input$acid_Next, {
    lang <- input$lang %||% "French"
    
    if (is.null(input$A_K) || length(input$A_K) == 0) {
      showNotification(t("missing_A_K", lang), type = "error")
      return()
    }
    if (is.null(input$A_approach) || input$A_approach == "") {
      showNotification(t("missing_A_approach", lang), type = "error")
      return()
    }
    if (is.null(input$A_opinion) || input$A_opinion == "") {
      showNotification(t("missing_A_opinion", lang), type = "error")
      return()
    }
    if (is.null(input$A_pH) || input$A_pH == "") {
      showNotification(t("missing_A_pH", lang), type = "error")
      return()
    }
    
    data_row <- data.frame(
      A_K = paste(input$A_K, collapse = ";"),
      A_approach = input$A_approach,
      A_opinion = input$A_opinion,
      A_pH = input$A_pH,
      stringsAsFactors = FALSE
    )
    
    saveRawInputs(data_row)
    clearLocalStorageInputs(session, c("A_K", "A_approach", "A_opinion", "A_pH"))
    log_event(paste("Saved Page 9 (Acidification) for:", session_token))
    currentPage(10)
  })
  
  observeEvent(input$acid_Back, {
    currentPage(8)
  })
  
  
  ###Sal inisation Page ####
  #
  observeEvent(input$sal_Next, {
    lang <- input$lang %||% "French"
    if (is.null(input$S_K) || length(input$S_K) == 0) {
      showNotification(t("missing_S_K", lang), type = "error")
      return()
    }
    
    saveRawInputs(data.frame(
      S_K       = csv_collapse(input$S_K),
      S_approach= input$S_approach,
      S_opinion = input$S_opinion,
      S_type    = input$S_type,
      stringsAsFactors = FALSE
    ))
    currentPage(12)
  })
  
  observeEvent(input$sal_Back, currentPage(10))
  
  

  ### Biodiversity Page ####
observeEvent(input$bio_Next, {
  lang <- input$lang %||% "French"
  
  if (is.null(input$HL_K) || length(input$HL_K) == 0) {
    showNotification(t("missing_HL_K", lang), type = "error")
    return()
  }
  if (is.null(input$HL_approach)) {
    showNotification(t("missing_HL_approach", lang), type = "error")
    return()
  }
  if (is.null(input$HL_opinion)) {
    showNotification(t("missing_HL_opinion", lang), type = "error")
    return()
  }
  
  saveRawInputs(data.frame(
    HL_K       = csv_collapse(input$HL_K),
    HL_approach= input$HL_approach,
    HL_opinion = input$HL_opinion,
    HL_val1    = csv_collapse(input$HL_val1),
    HL_val2    = input$HL_val2,
    stringsAsFactors = FALSE
  ))
  
  currentPage(13)
})

observeEvent(input$bio_Back, currentPage(11))
 
observeEvent(input$nm_Next, {
  lang <- input$lang %||% "French"
  
  if (is.null(input$NM_K) || length(input$NM_K) == 0) {
    showNotification(t("missing_NM_K", lang), type = "error")
    return()
  }
  if (is.null(input$NM_approach)) {
    showNotification(t("missing_NM_approach", lang), type = "error")
    return()
  }
  if (is.null(input$NM_opinion)) {
    showNotification(t("missing_NM_opinion", lang), type = "error")
    return()
  }
  
  saveRawInputs(data.frame(
    NM_K       = csv_collapse(input$NM_K),
    NM_approach= input$NM_approach,
    NM_opinion = input$NM_opinion,
    NM_forms   = csv_collapse(input$NM_forms),
    NM_system  = input$NM_system,
    stringsAsFactors = FALSE
  ))
  
  currentPage(14)
})

observeEvent(input$nm_Back, currentPage(12))

 ### Water Management Page ####
observeEvent(input$nm_Next, {
  lang <- input$lang %||% "French"
  
  if (is.null(input$NM_K) || length(input$NM_K) == 0) {
    showNotification(t("missing_NM_K", lang), type = "error")
    return()
  }
  if (is.null(input$NM_approach)) {
    showNotification(t("missing_NM_approach", lang), type = "error")
    return()
  }
  if (is.null(input$NM_opinion)) {
    showNotification(t("missing_NM_opinion", lang), type = "error")
    return()
  }
  
  saveRawInputs(data.frame(
    NM_K       = csv_collapse(input$NM_K),
    NM_approach= input$NM_approach,
    NM_opinion = input$NM_opinion,
    NM_forms   = csv_collapse(input$NM_forms),
    NM_system  = input$NM_system,
    stringsAsFactors = FALSE
  ))
  
  currentPage(14)
})

observeEvent(input$nm_Back, currentPage(12))

 ### Carbon Page ####
observeEvent(input$dc_Next, {
  lang <- input$lang %||% "French"
  saveRawInputs(data.frame(
    DC_K = csv_collapse(input$DC_K),
    DC_approach = input$DC_approach,
    DC_opinion = input$DC_opinion,
    DC_practices = csv_collapse(input$DC_practices),
    DC_carbon_credits = input$DC_carbon_credits,
    stringsAsFactors = FALSE
  ))
  currentPage(16)
})
observeEvent(input$dc_Back, currentPage(14))

### Threats Page ####

observeEvent(input$threats_Next, {
  lang <- input$lang %||% "French"
  saveRawInputs(data.frame(
    Threat_Val_E = input$Threat_Val_E,
    Threat_Val_E_comment = input$Threat_Val_E_comment,
    Threat_Val_A = input$Threat_Val_A,
    Threat_Val_A_comment = input$Threat_Val_A_comment,
    stringsAsFactors = FALSE
  ))
  currentPage(17)
})
observeEvent(input$threats_Back, currentPage(15))

### Demographics Page ####
observeEvent(input$demo_Next, {
  saveRawInputs(data.frame(
    Age = input$Age,
    education_level = input$education_level,
    Land_ownership = csv_collapse(input$Land_ownership),
    Land_area = input$Land_area,
    stringsAsFactors = FALSE
  ))
  currentPage(18)
})
observeEvent(input$demo_Back, currentPage(16))

### Final Submission Page ####
observeEvent(input$p18_Back, currentPage(17))
observeEvent(input$submitAll, {
  log_event(paste("Final submission by", session_token))
  currentPage(19)
})

observeEvent(input$restartApp, {
  session$reload()
})
#END
  

}

shinyApp(ui = ui, server = server)





