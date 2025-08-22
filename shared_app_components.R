# shared_app_components.R

# Common UI Components
common_head_ui <- function(app_prefix) {
  tags$head(
    tags$link(rel = "icon", href = "favicon.ico", type = "image/x-icon"),
    tags$link(rel = "stylesheet", href = "app.css"),
    tags$script(src = "app.js"),
    tags$script(HTML(paste0("window.appPrefix = '", app_prefix, "';")))
  )
}

common_top_bar_ui <- function() {
  div(
    class = "top-bar",
    div(
      style = "min-width:160px;",
      selectInput("lang",
                  label = "Langue / Language / Idioma",
                  choices = c("French", "Spanish", "English"),
                  selected = "French",
                  width   = "160px")
    ),
    div(
      class = "logo-bar",
      tags$img(src = "Logos.png",  alt = "USYD and ESDR3C Lab Logos")
    )
  )
}

common_footer_ui <- function() {
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
}

# Common Server Logic Setup
setup_app_session_and_progress <- function(input, output, session, dbtable_name, appPrefix, total_pages_reactive = NULL) {
  session_token <- reactiveVal(NULL)
  currentPage <- reactiveVal(0)

  # Restore session token
  observeEvent(input$restoredSessionToken, {
    restored_token <- input$restoredSessionToken
    if (!is.null(restored_token) && restored_token != "" && restored_token != "[]") {
      session_token(restored_token)
      print(paste("Restored session token:", session_token()))
    }
  }, priority = 10)

  # Generate new token if needed
  observe({
    if (is.null(session_token())) {
      session_token(generateUserID()) # Assuming generateUserID is in utils.R
      message("⚠️ session_token was null — generated new token.")
    }
  })

  # Restore page
  observeEvent(input$restoredPage, {
    restored <- suppressWarnings(as.numeric(input$restoredPage))
    if (!is.na(restored)) currentPage(restored)
  })

  # Page change observer
  observeEvent(currentPage(), {
    lang <- isolate(input$lang %||% "French")
    session$sendCustomMessage("setAppPrefix", appPrefix) # Set appPrefix in JS
    session$sendCustomMessage("setSessionToken", list(key = paste0(appPrefix, "_sessionToken"), value = session_token()))
    session$sendCustomMessage("scrollTop", list())
    user_lang <- if (!is.null(input$lang)) input$lang else "French"
    session$sendCustomMessage("disconnectedAlert", t("disconnected_alert", user_lang)) # Assuming t is available
    session$sendCustomMessage('pageChanged', list(page = currentPage(), appPrefix = appPrefix))

    # Logic for redirecting to page 0 if session is invalid
    if (currentPage() != 0 && (is.null(session_token()) || session_token() == "")) {
      currentPage(0)
      session$sendCustomMessage("clearLocalStorageKey", list(key = paste0(appPrefix, "_sessionToken")))
      showNotification("Invalid session. Redirecting to start page.", type = "warning", duration = 5)
      return()
    }
  })

  # Browser back/forward button
  observeEvent(input$browserBackPage, {
    target <- suppressWarnings(as.integer(input$browserBackPage))
    if (!is.na(target)) currentPage(target)
  })

  # Progress bar UI
  output$progressUI <- renderUI({
    p <- currentPage()
    total_pages <- if (!is.null(total_pages_reactive)) total_pages_reactive() else 18 # Default to 18 if not provided

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

  # Return reactive values
  list(
    session_token = session_token,
    currentPage = currentPage
  )
}

# safe_isolate_string function (common to both)
safe_isolate_string <- function(x) {
  val <- isolate(x)
  if (is.null(val) || is.na(val)) return("")
  as.character(val)
}
