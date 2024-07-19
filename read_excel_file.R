# read_excel_file.R

library(readxl)

read_excel_file <- function() {
  file_path <- "DATASET_V4_SMO.xlsx"
  if (!file.exists(file_path)) stop("File does not exist")
  dataset <- read_excel(file_path)
  return (dataset)
}