library(readxl)
library(dplyr)
library(ggplot2)

source("read_excel_file.R")
dataset <- read_excel_file()

replace_values_with_na_mutate <- function(df, rules) {
  df %>%
    mutate(across(
      .cols = names(rules),
      .fns = ~ if_else(. %in% rules[[cur_column()]], NA_real_, .)
    ))
}

rules <- list(
  Age = c(9999),
  TBV = c(9999),
  TBV_Ratio = c(9999),
  Mutation = c(9999),
  Corticosteroid = c(9999)
)

dmd <- dataset %>% filter(Disease == 1)
hc <- dataset %>% filter(Disease == 0)

dmd <- replace_values_with_na_mutate(dmd, rules)
hc <- replace_values_with_na_mutate(hc, rules)

 #### test 
test_dmd <- data.frame(
  ID = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  Scannertype = c(2, 0, 1, 1, 1, 1, 1, 2, 2, 0),
  Mutation = c(0, 1, 1, 9999, 1, 1, 2, 3, 3, 3),
  Age = c(40, 30, 50, 60, 70, 80, 90, 100, 110, 120),
  TBV = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000),
  TBV_Ratio = c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0),
  Corticosteroid = c(0, 0, 1, 9999, 1, 0, 2, 1, 0, 0)
)

categorize_corticosteroid <- function(df) {
  df %>%
    mutate(
      corticosteroid_group = case_when(
	Corticosteroid == 0 ~ "No",
	Corticosteroid == 1 ~ "Yes",
	Corticosteroid == 2 ~ "Unknown",
	###TRUE ~ "Other"
      )
    )
}


print_group_percentages <- function(patient_type, df, group_column) {
  if (!(group_column %in% names(df))) {
    stop(paste("Column", group_column, "does not exist in the dataframe."))
  }
  
  # filter out rows with NA values in the group column
  df <- df %>% filter(!is.na(!!sym(group_column)))

  # calculate group percentages and print them
  group_counts <- df %>%
   count(!!sym(group_column)) %>%
   mutate(percentage = round(100 / sum(n) * n, 1))

   #### Write clean message 
   message(patient_type, " results :")
   #message(patient_type, " patients by ", group_column, " :")
   print(group_counts)
   message("\n")
}



#   group_counts <- df %>%
#     count(!!sym(group_column)) %>%
#     mutate(percentage = round(100 / sum(n) * n, 1))

#   message(patient_type, " patients by ", group_column, " group:")
#   print(group_counts)
#   message("\n")

##test_dmd <- categorize_corticosteroid(test_dmd)
dmd <- categorize_corticosteroid(dmd)
hc <- categorize_corticosteroid(hc)

print_group_percentages("DMD", dmd, "corticosteroid_group")
print_group_percentages("HC", hc, "corticosteroid_group")



