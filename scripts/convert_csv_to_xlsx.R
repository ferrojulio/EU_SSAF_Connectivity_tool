library(readr)
library(writexl)

translations <- read_csv("/srv/shiny-server/landmanagers/Landmanagers_translations_new.csv", col_types = cols())
write_xlsx(translations, "/srv/shiny-server/landmanagers/Landmanagers_translations_new.xlsx")