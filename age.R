library(readxl)
library(dplyr)
source("read_excel_file.R")

# file_path <- "DATASET_V4_SMO.xlsx"
# if (!file.exists(file_path)) stop("File does not exist")
# dataset <- read_excel(file_path)

dataset <- read_excel_file()

replace_values_with_na_mutate <- function(df, rules) {
  df %>%
    mutate(across(
      .cols = names(rules),
      .fns = ~ if_else(. %in% rules[[cur_column()]], NA_real_, .)
    ))
}

# replace_values_with_na_mutate <- function(df, rules) {
# 	dplyr::mutate(df, dplyr::across(
# 		.cols = names(rules),
# 		.fns = ~ dplyr::if_else(. %in% rules[[dplyr::cur_column()]], NA_real_, .)
# 	))
# }

rules <- list(
  Age = c(9999),
  TBV = c(9999)
)

# Create dmd and hc subsets
dmd <- dataset %>% filter(Disease == 1)
hc <- dataset %>% filter(Disease == 0)

dmd <- replace_values_with_na_mutate(dmd, rules)
hc <- replace_values_with_na_mutate(hc, rules)


dmd_first_visit <- dmd %>% filter(Visit == 1)
dmd_second_visit <- dmd %>% filter(Visit == 2)
hc_first_visit <- hc %>% filter(Visit == 1)
hc_second_visit <- hc %>% filter(Visit == 2)

# print(dmd_first_visit$ID)

# message("DMD second and first visit: ", nrow(dmd))
# message("HC second and first visit: ", nrow(hc))


# message("DMD first visit:", nrow(dmd_first_visit))
# message("DMD second visit:", nrow(dmd_second_visit), "\n")

# message("HC first visit:", nrow(hc_first_visit))
# message("HC second visit:", nrow(hc_second_visit), "\n")



# Example data for DMD patients and healthy controls
# test_hc_first_visit <- data.frame(
#   ID = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
#   Age = c(4, 6, 12, 14, 19, 21, 25, 27, 30, 32, 35),
# )
# Example data for DMD patients and healthy controls
test_hc_first_visit <- data.frame(
  ID = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
  Age = c(4, 6, 12, 14, 19, 21, 25, 27, 30, 32, 35)
)


# categorize patients based on their age
# 0 - 5 years old
# 6 - 12 years old
# 13 - 19 years old
# 20 - 25 years old
# 26+ years old
test_hc_first_visit_ <- test_hc_first_visit %>%
  mutate(
    age_group = case_when(
      Age <= 5 ~ "0 - 5 years old",
      Age <= 12 ~ "6 - 12 years old",
      Age <= 19 ~ "13 - 19 years old",
      Age <= 25 ~ "20 - 25 years old",
      TRUE ~ "26+ years old"
    )
  )



# print(test_hc_first_visit_)
# message("patients below 5 years old:", nrow(test_hc_first_visit_[test_hc_first_visit_$age_group == "0 - 5 years old", ]))
# message("6 - 12 years old patients:", nrow(test_hc_first_visit_[test_hc_first_visit_$age_group == "6 - 12 years old", ]))
# message("12 years old patients:", nrow(test_hc_first_visit_[test_hc_first_visit_$age_group == "6 - 12 years old", ]))
# message("13 - 19 years old patients:", nrow(test_hc_first_visit_[test_hc_first_visit_$age_group == "13 - 19 years old", ]))
# message("20 - 25 years old patients:", nrow(test_hc_first_visit_[test_hc_first_visit_$age_group == "20 - 25 years old", ]))
# message("26+ years old patients:", nrow(test_hc_first_visit_[test_hc_first_visit_$age_group == "26+ years old", ]))

# patients under 5 years old
test_patients_0_5 <- nrow(test_hc_first_visit_[test_hc_first_visit_$age_group == "0 - 5 years old", ])
test_percentage_0_5 <- round((100 / nrow(dmd) * test_patients_0_5), 1)

test_patients_6_12 <- nrow(test_hc_first_visit_[test_hc_first_visit_$age_group == "6 - 12 years old", ])
test_percentage_6_12 <- round((100 / nrow(dmd) * test_patients_6_12), 1)

test_patients_13_19 <- nrow(test_hc_first_visit_[test_hc_first_visit_$age_group == "13 - 19 years old", ])
test_percentage_13_19 <- round((100 / nrow(dmd) * test_patients_13_19), 1)

test_patients_20_25 <- nrow(test_hc_first_visit_[test_hc_first_visit_$age_group == "20 - 25 years old", ])
test_percentage_20_25 <- round((100 / nrow(dmd) * test_patients_20_25), 1)

test_patients_26 <- nrow(test_hc_first_visit_[test_hc_first_visit_$age_group == "26+ years old", ])
test_percentage_26 <- round((100 / nrow(dmd) * test_patients_26), 1)

# message("0 - 5 years old patients in percentage:", test_percentage_0_5, "%")
# message("6 - 12 years old patients in percentage:", test_percentage_6_12, "%")
# message("13 - 19 years old patients in percentage:", test_percentage_13_19, "%")
# message("20 - 25 years old patients in percentage:", test_percentage_20_25, "%")
# message("26+ years old patients in percentage:", test_percentage_26, "%")



##########

dmd_first_visit_ages <- dmd_first_visit %>%
  mutate(
    age_group = case_when(
      Age <= 5 ~ "0 - 5 years old",
      Age <= 12 ~ "6 - 12 years old",
      Age <= 19 ~ "13 - 19 years old",
      Age <= 25 ~ "20 - 25 years old",
      TRUE ~ "26+ years old"
    )
  )

hc_first_visit_ages <- hc_first_visit %>%
  mutate(
    age_group = case_when(
      Age <= 5 ~ "0 - 5 years old",
      Age <= 12 ~ "6 - 12 years old",
      Age <= 19 ~ "13 - 19 years old",
      Age <= 25 ~ "20 - 25 years old",
      TRUE ~ "26+ years old"
    )
  )

age_groups <- c("0 - 5 years old", "6 - 12 years old", "13 - 19 years old", "20 - 25 years old", "26+ years old")

# DMD

message("DMD first visit ages:")
for (age_group in age_groups) {
  message(age_group, " dmd patients:", nrow(dmd_first_visit_ages[dmd_first_visit_ages$age_group == age_group, ]))
}

message("\n")

message("DMD Patients first visit in percentages")
for (age_group in age_groups) {
	patients_count <- nrow(dmd_first_visit_ages[dmd_first_visit_ages$age_group == age_group, ])
	percentage <- round((100 / nrow(dmd) * patients_count), 1)
	message(age_group, " patients in percentage:", percentage, "%")
}

#### HC

message("\n")

message("HC first visit ages:")
for (age_group in age_groups) {
	message(age_group, " hc patients:", nrow(hc_first_visit_ages[hc_first_visit_ages$age_group == age_group, ]))
}

message("\n")

message("HC Patients first visit in percentages")

for (age_group in age_groups) {
	patients_count <- nrow(hc_first_visit_ages[hc_first_visit_ages$age_group == age_group, ])
	percentage <- round((100 / nrow(hc) * patients_count), 1)
	message(age_group, " patients in percentage:", percentage, "%")
}