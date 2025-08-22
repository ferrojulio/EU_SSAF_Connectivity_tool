#
# utils.R — Shared utilities ####
# These are functions needed across all 

library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(stringi)
library(readr)


#
`%||%` <- function(a, b) { if (is.null(a) || (is.character(a) && length(a)==0) || identical(a, "")) b else a }

is_true  <- function(x) isTRUE(x)                             # safe TRUE test
nzchr    <- function(x) nzchar(x %||% "")                     # non-empty string
len      <- function(x) if (is.null(x)) 0L else length(x)     # safe length




connect_to_db <- function(mode = "online") {
  tryCatch({
    if (identical(mode, "online")) {
      DBI::dbConnect(RPostgres::Postgres(),
        host = host, port = port, user = user, password = password,
        dbname = dbname, options = "-c search_path=public")
    } else {
      DBI::dbConnect(RSQLite::SQLite(), "TestDatabase.sqlite")
    }
  }, error = function(e) {
    message("❌ DB connection failed: ", e$message)
    NULL
  })
}




#Translations ####

t_original <- function(key, lang = "French") { # Removed translations_df argument
  # Access translations from the global environment
  if (!exists("translations", envir = .GlobalEnv)) {
    message("⚠️ Global 'translations' object not found for key: ", key)
    return(paste0("[[", key, "]]"))
  }
  
  translations_df <- get("translations", envir = .GlobalEnv)

  lang <- if (lang %in% colnames(translations_df)) lang else "English"
  row <- which(translations_df$unique_ID == key)[1]

  if (length(row) == 0 || is.na(row)) return(paste0("[[", key, "]]"))

  val <- translations_df[[lang]][row]
  if (is.na(val) || val == "") return(paste0("[[", key, "]]"))
  return(val)
}

#Dev test ####
checkTranslationKeys <- function(used_keys, translations_df) {
  missing_keys <- setdiff(used_keys, translations_df$unique_ID)
  if (length(missing_keys) > 0) {
    warning("Missing translation keys:\n", paste(missing_keys, collapse = "\n"))
  } else {
    message("✅ All translation keys found.")
  }
}

used_translation_keys <- c(
  "title_farmers", "nav_start", "nav_back", "nav_next",
  "page0_intro_header", "page0_intro_text", "consent_text",
  "missing_supply_newconcepts", "missing_supply_approach", "missing_supply_opinion",
  "econ_newconcepts_Q", "econ_opinion_A4"  # etc...
)


#
# UI COMPONENTS ####




clearLocalStorageInputs <- function(session, input_ids) {
  session$sendCustomMessage("clearCurrentInputs", list(keys = input_ids))
}




###knowledge Q####
exclusiveCheckboxScript <- function(inputId, exclusiveValue) {
  script <- sprintf('  
    $(document).on("click", "input[type=checkbox][value=\'%s\']", function() {
      if ($(this).is(":checked")) {
        $("input[type=checkbox][name=\'%s\']").each(function(){
          if ($(this).val() !== "%s") {
            $(this).prop("checked", false).trigger("change");
          }
        });
      }
    });
    

    $(document).on("click", "input[type=checkbox][name=\'%s\']", function() {
      if ($(this).val() !== "%s" && $(this).is(":checked")) {
        $("input[type=checkbox][value=\'%s\']").prop("checked", false).trigger("change");
      }
    });
  ', exclusiveValue, inputId, exclusiveValue, inputId, exclusiveValue, exclusiveValue)
  return(tags$script(HTML(script)))
}


#
# UTILITY FUNCTIONS ####
#

# Generate unique session ID 
generateUserID <- function() {
  paste0("user_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(1000:9999, 1))
}

# Restricted text input
textInput2 <- function(inputId, label, value = "", width = NULL, placeholder = NULL, maxlength = 350) {
  shiny::textInput(
    inputId = inputId,
    label = label,
    value = value,
    width = width,
    placeholder = placeholder
  )
}

is_valid_text <- function(x, lang = "French", maxlen = 350) {
  if (!is.character(x)) return(FALSE)
  x <- iconv(x, from = "", to = "UTF-8")  # handles odd encodings from UI
  pattern <- "^[\\p{L}\\p{Z}\\p{M}'-]+$"  
  
  is_valid <- !is.null(x) &&
    nchar(x, allowNA = TRUE) <= maxlen &&
    stringi::stri_detect_regex(x, pattern)
  
  if (!is_valid) {
    msg <- switch(lang,
                  "French"  = "Veuillez entrer uniquement des lettres, accents, espaces ou tirets.",
                  "Spanish" = "Por favor, introduzca solo letras, acentos, espacios o guiones.",
                  "English" = "Please enter only letters, accents, spaces, or hyphens.",
                  "Invalid input."
    )
    return(msg)
  }
  
  return(TRUE)
}


# Check if value is numeric
digitize <- function(x) {
  suppressWarnings(!is.na(as.numeric(x)))
}

# Clean text input (for SQL or validation)
Sanitize <- function(text) {
  if (is.null(text) || text == "") return("No answer")
  text <- tolower(text)
  text <- gsub("'", "", text)
  text <- gsub("-", " ", text)
  return(text)
}

#
## UI Progress Bar (Bootstrap style)####
#

progressBar <- function(id, value, total = NULL, display_pct = FALSE, size = NULL,
                        status = NULL, striped = FALSE, title = NULL,
                        range_value = NULL, commas = TRUE, unit_mark = "%" ) {
  
  if (!is.null(total)) {
    percent <- round(value / total * 100)
  } else {
    value <- round(value)
    percent <- if (!is.null(range_value)) {
      scales::rescale(value, from = range_value, to = c(0, 100))
    } else value
  }
  
  value_display <- value
  total_display <- total
  
  if (!is.null(total)) {
    total <- tags$span(
      class = "progress-number",
      tags$b(value_display, id = paste0(id, "-value")),
      "/",
      tags$span(id = paste0(id, "-total"), total_display)
    )
  }
  
  tagPB <- tags$div(
    class = "progress-group",
    title, total,
    tags$div(
      class = "progress",
      tags$div(
        id = id,
        class = paste("progress-bar",
                      if (!is.null(status)) paste0("bg-", status),
                      if (striped) "progress-bar-striped"),
        style = paste0("width:", percent, "%;"),
        role = "progressbar",
        if (display_pct) paste0(percent, unit_mark)
      )
    )
  )
  
  tagList(
    singleton(tags$head(tags$style(".progress-number {position: absolute; right: 200px;}"))),
    tagPB
  )
}

#
# Optional: Log event to text file (can be removed if not needed) ####
# 

log_event <- function(message, file = "event_log.txt") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- paste0("[", timestamp, "] ", message, "\n")
  cat(line, file = file, append = TRUE)
}

.t_global <- NULL
set_t_function <- function(tf) { .t_global <<- tf }


renderTranslatedCheckbox <- function(
  id, label_key, value_choices, label_keys, lang, current_input, session, ...
) {
  dots   <- list(...)
  t_func <- if (!is.null(dots$t_func)) dots$t_func else .t_global
  if (is.null(t_func)) {
    t_func <- function(key, lang) paste0("[[", key, "]]")  # safe fallback
  }

  label <- tryCatch(t_func(label_key, lang), error = function(e) paste0("[[", label_key, "]]"))

  choices <- tryCatch({
    setNames(
      value_choices,
      vapply(label_keys, function(k) {
        tryCatch(t_func(k, lang), error = function(e) paste0("[[", k, "]]"))
      }, character(1))
    )
  }, error = function(e) {
    message("❌ setNames failed in ", id, ": ", e$message)
    setNames(value_choices, value_choices) 
  })

  checkboxGroupInput(
    id,
    label = label,
    choices = choices,
    selected = current_input %||% character(0)
  )
}
