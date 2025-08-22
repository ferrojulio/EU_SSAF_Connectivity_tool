library(readxl)
translations <- read_excel("/srv/shiny-server/policymakers/Policymakers_translations_new.xlsx")
write.csv(translations, "/srv/shiny-server/policymakers/Policymakers_translations_new.csv", row.names = FALSE, fileEncoding = "UTF-8")