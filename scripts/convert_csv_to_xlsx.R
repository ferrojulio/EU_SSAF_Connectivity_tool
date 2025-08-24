library(readr)
library(writexl)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
  stop("Usage: Rscript convert_csv_to_xlsx.R <input.csv> <output.xlsx>", call. = FALSE)
}

input_file <- args[1]
output_file <- args[2]

translations <- read_csv(input_file, col_types = cols())
write_xlsx(translations, output_file)
