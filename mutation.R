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
  Mutation = c(9999)
)

# Create dmd and hc subsets
dmd <- dataset %>% filter(Disease == 1)
hc <- dataset %>% filter(Disease == 0)

dmd <- replace_values_with_na_mutate(dmd, rules)
hc <- replace_values_with_na_mutate(hc, rules)

####scanner_groups <- c("DMDBNL", "DMDBUK", "DMDBA")
 
 #### test 
test_dmd <- data.frame(
  ID = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  Scannertype = c(2, 0, 1, 1, 1, 1, 1, 2, 2, 0),
  Mutation = c(0, 1, 1, 9999, 1, 1, 2, 3, 3, 3),
  Age = c(40, 30, 50, 60, 70, 80, 90, 100, 110, 120),
  TBV = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000),
  TBV_Ratio = c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0)
)
#####


categorize_mutations <- function(df) {
  df %>%
    mutate(
      mutation_group = case_when(
        Mutation == 0 ~ "Healthy",
        Mutation == 1 ~ "Proximal",
        Mutation == 2 ~ "Distal",
        Mutation == 3 ~ "Unknown",
	####TRUE ~ "other"
      )
    )
}


print_group_percentages <- function(patient_type, df, group_column) {
  if (!(group_column %in% names(df))) {
    stop(paste("Column", group_column, "does not exist in the dataframe."))
  }

  # Filter out rows with NA values in the specified group column
  df_filtered <- df[!is.na(df[[group_column]]), ]

  # Check if there are any unique values left in the specified group column
  if (length(unique(df_filtered[[group_column]])) == 0) {
    stop("No unique values found in the specified group column.")
  }

  groups <- unique(df_filtered[[group_column]])
  print(groups)

  total_patients <- nrow(df_filtered)
  print(total_patients)
  print(nrow(df))

  for (group in groups) {
    if (!is.na(group)) {
      patients_count <- nrow(df_filtered[df_filtered[[group_column]] == group, ])
#       print(patients_count)
#       print(total_patients)
      percentage <- round(100 / total_patients * patients_count, 3)
      message("Percentage of ", patient_type, " patients with mutation group ", group, " is: ", percentage, "%")
    }
  }
}



# test_dmd_2 <- categorize_mutations(test_dmd)

# print(test_dmd_2)

# print_group_percentages("dmd_test", test_dmd_2, "mutation_group")


dmd <- categorize_mutations(dmd)
hc <- categorize_mutations(hc)

print_group_percentages("dmd", dmd, "mutation_group")
print_group_percentages("hc", hc, "mutation_group")


# dmd_mutation <- dmd %>%
#   mutate(
#     mutation_group = case_when(
#       Mutation == 0 ~ "Healthy",
#       Mutation == 1 ~ "Proximal",
#       Mutation == 2 ~ "Distal",
#       Mutation == 3 ~ "Unknown"
#     )
#   )

# mutation_groups <- unique(dmd_mutation$mutation_group)

# for (group in mutation_groups) {
#   patients_count <- nrow(dmd_mutation[dmd_mutation$mutation_group == group, ])
#   percentage <- round(100 / nrow(dmd) * patients_count, 1)
#   message("Percentage of dmd patients with mutation group ", group, " is: ", percentage, "%")
# }