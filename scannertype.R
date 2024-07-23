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

scanner_groups <- c("DMDBNL", "DMDBUK", "DMDBA")
 
#  #### test 
# test_dmd <- data.frame(
#   ID = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#   Scannertype = c(2, 0, 1, 1, 1, 1, 1, 2, 2, 0),
#   Mutation = c(0, 1, 1, 9999, 1, 1, 2, 3, 3, 3),
#   Age = c(40, 30, 50, 60, 70, 80, 90, 100, 110, 120),
#   TBV = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000),
#   TBV_Ratio = c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0)
# )

# test_dmd <- replace_values_with_na_mutate(test_dmd, rules)

# test_dmd_scannertype <- test_dmd %>%
# mutate(
# 	  scanner_group = case_when(
# 		Scannertype == 0 ~ "DMDBNL",
# 		Scannertype == 1 ~ "DMDBUK",
# 		Scannertype == 2 ~ "DMDBA",
# 		#TRUE ~ "other"
# 	  )
# )

# for (scanner_group in scanner_groups) {
#   patients_count <- nrow(test_dmd_scannertype[test_dmd_scannertype$scanner_group == scanner_group, ])
#   percentage <- round(100 / nrow(test_dmd) * patients_count, 1)
#   message("Percentage of dmd patients with scanner group ", scanner_group, " is: ", percentage, "%")
# }

dmd_scannertype <- dmd %>%
  mutate(
    scanner_group = case_when(
      Scannertype == 0 ~ "DMDBNL",
      Scannertype == 1 ~ "DMDBUK",
      Scannertype == 2 ~ "DMDBA"
      # TRUE ~ "other"
    )
  )

scanner_groups <- unique(dmd_scannertype$scanner_group)

for (scanner_group in scanner_groups) {
  patients_count <- nrow(dmd_scannertype[dmd_scannertype$scanner_group == scanner_group, ])
  percentage <- round(100 / nrow(dmd) * patients_count, 1)
  message("Percentage of dmd patients with scanner group ", scanner_group, " is: ", percentage, "%")
}

message("\n")

hc_scannertype <- hc %>%
  mutate(
    scanner_group = case_when(
      Scannertype == 0 ~ "DMDBNL",
      Scannertype == 1 ~ "DMDBUK",
      Scannertype == 2 ~ "DMDBA"
      # TRUE ~ "other"
    )
  )

scanner_groups <- unique(hc_scannertype$scanner_group)

for (scanner_group in scanner_groups) {
  patients_count <- nrow(hc_scannertype[hc_scannertype$scanner_group == scanner_group, ])
  percentage <- round(100 / nrow(hc) * patients_count, 1)
  message("Percentage of hc patients with scanner group ", scanner_group, " is: ", percentage, "%")
}