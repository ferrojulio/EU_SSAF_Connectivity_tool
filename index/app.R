library(shiny)

# Load translations

translations <- readRDS("index_translations.rds")

# translations <- read.csv("Index_translations.csv")
# saveRDS(translations, "index_translations.rds")

# Helper translation function
t <- function(key, lang = "French") {
  lang <- if (lang %in% colnames(translations)) lang else "French"
  row <- which(translations$unique_ID == key)[1]
  
  if (length(row) == 0 || is.na(row)) return(paste0("[[", key, "]]"))
  
  val <- translations[[lang]][row]
  if (is.na(val) || val == "") return(paste0("[[", key, "]]"))
  return(val)
}



ui <- fluidPage(
  tags$head(
    tags$link(rel = "icon", href = "favicon.ico", type = "image/x-icon"),
    tags$link(rel = "stylesheet", href = "app.css")
  ),
  
  div(
    class = "top-bar",
    div(style = "min-width:160px;",
        selectInput("lang",
                    label = "Langue / Language / Idioma",
                    choices = c("French", "Spanish", "English"),
                    selected = "French",
                    width   = "160px")),
    div(class = "logo-bar",
        tags$img(src = "Logos.png", alt = "USYD and ESDR3C Lab Logos"))
  ),
  
  div(class = "container-fluid",
      fluidRow(column(width = 10, offset = 1, br(), uiOutput("mainUI")))),
  
  uiOutput("footerUI")
)


server <- function(input, output, session) {
  output$mainUI <- renderUI({
    lang <- input$lang %||% "French"
    
    tagList(
      h2(t("page0_intro_header", input$lang)),
      h4(t("page0_intro_header3", input$lang)),
      p(t("page0_intro_text", input$lang)),

      h4(t("page0_intro_header4", input$lang)),
      tags$ul(
        tags$li(t("page0_info1", input$lang)),
        tags$li(t("page0_info2", input$lang)),
        tags$li(t("page0_info3", input$lang)),
        tags$li(t("page0_info4", input$lang)),
        tags$li(t("page0_info5", input$lang)),
        tags$li(t("page0_info6", input$lang)),
        tags$li(t("page0_info7", input$lang))
      ),
      p(t("page0_info8", lang)),
      div(class = "btn-group", style = "margin: 30px 0; width: 100%;",
          tags$link(rel = "stylesheet", href = "app.css"),
          tags$a(href = "/farmers", class = "btn btn-primary", t("page0_Farmer", input$lang)),
 #        tags$a(href = "/landmanagers", class = "btn btn-primary", t("page0_Land", input$lang)),
          tags$a(href = "/policymakers", class = "btn btn-primary", t("page0_Policy", input$lang))
      )

      
    )
  })
  
  output$footerUI <- renderUI({
    lang <- input$lang %||% "French"
    
    div(class = "footer",
        div(class = "page-wrapper",
            h4(t("Footer_header", lang)),
            p(t("Footer_1", lang)),
            p(t("Footer1.1", lang)),
            p(t("Footer_2", lang)),
            p(t("Footer2.1", lang))
        )
    )
  })
}


shinyApp(ui, server)
