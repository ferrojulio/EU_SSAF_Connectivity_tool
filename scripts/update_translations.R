
# Read the CSV file
translations_df <- read.csv("/srv/shiny-server/landmanagers/landmanagers_translations.csv", encoding = "UTF-8", stringsAsFactors = FALSE)

# Rename keys
translations_df$unique_ID[translations_df$unique_ID == "page0_intro_header"] <- "landmanagers_intro_header"
translations_df$unique_ID[translations_df$unique_ID == "page0_intro_text"] <- "landmanagers_intro_text"

# Keys to append (from the deleted Landmanagers_translations_new.csv)
new_keys_data <- data.frame(
  unique_ID = c("missing_supply_approach", "missing_supply_opinion", "disconnected_alert"),
  French = c("Veuillez sélectionner une approche pour continuer. ", "Veuillez donner votre avis pour continuer.", "La connexion a été perdue. Veuillez vérifier votre connexion Internet et réessayer."),
  English = c("Please select an approach to continue.", "Please provide your opinion to continue.", "Connection lost. Please check your internet connection and try again."),
  Spanish = c("Por favor, seleccione un enfoque para continuar.", "Por favor, proporcione su opinión para continuar.", "Conexión perdida. Por favor, compruebe su conexión a internet e inténtelo de nuevo."),
  stringsAsFactors = FALSE
)

# Append new keys, ensuring no duplicates
# First, remove any existing rows for these new_keys_data unique_IDs to prevent duplicates
translations_df <- translations_df[!translations_df$unique_ID %in% new_keys_data$unique_ID, ]
translations_df <- rbind(translations_df, new_keys_data)

# Write the modified data frame back to the CSV
write.csv(translations_df, "/srv/shiny-server/landmanagers/landmanagers_translations.csv", row.names = FALSE, fileEncoding = "UTF-8")

message("Translations updated successfully.")
