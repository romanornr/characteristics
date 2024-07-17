library(readxl)
library(dplyr)
library(ggplot2)

file_path <- "DATASET_V3_SMO.xlsx"
if (!file.exists(file_path)) stop("File does not exist")
dataset <- read_excel(file_path)

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
  TBV = c(9999),
  TBV_Ratio = c(9999)
)

# Create dmd and hc subsets
dmd <- dataset %>% filter(Disease == 1)
hc <- dataset %>% filter(Disease == 0)

dmd <- replace_values_with_na_mutate(dmd, rules)
hc <- replace_values_with_na_mutate(hc, rules)

# Subset dmd patients from the Netherlands who are adults, kids and UK kids
dmd_adults_nl <- dmd[grepl("^DMDBA", dmd$ID), ]
dmd_kids_nl <- dmd[grepl("^DMDBNL", dmd$ID), ]
dmd_kids_uk <- dmd[grepl("^DMDBUK", dmd$ID), ]

# message("DMD amount of adults:", nrow(dmd_adults_nl))
# message("DMD amount of NL kids:", nrow(dmd_kids_nl))
# message("DMD amount of UK kids:", nrow(dmd_kids_uk), "\n")

#Subset HC patients from the Netherlands who are adults, kids and UK kids
hc_adults_nl <- hc[grepl("^DMDBA", hc$ID), ]
hc_kids_nl <- hc[grepl("^DMDBNL", hc$ID), ]
hc_kids_uk <- hc[grepl("^DMDBUK", hc$ID), ]

# message("HC amount of adults:", nrow(hc_adults_nl))
# message("HC amount of NL kids:", nrow(hc_kids_nl))
# message("HC amount of UK kids:", nrow(hc_kids_uk), "\n")




#################################################################
# Calculate mean of the age of DMD and HC patients
##################################################################
mean_dmd_age <- round(mean(dmd$Age, na.rm = TRUE), 1)
mean_age_hc <- round(mean(hc$Age, na.rm = TRUE), 1)

message("Mean age of DMD:", mean_dmd_age)
message("Mean age of HC:", mean_age_hc, "\n")



### Combine kids from NL and UK
dmd_kids <- rbind(dmd_kids_nl, dmd_kids_uk)
hc_kids <- rbind(hc_kids_nl, hc_kids_uk)

### Combine adults from NL and UK
dmd_adults <- rbind(dmd_adults_nl, hc_adults_nl)
hc_adults <- rbind(hc_adults_nl)

# Calculate the mean of the age of DMD and HC patients which is both first visit and second
mean_dmd <- round(mean(dmd$Age, na.rm = TRUE), 1)
mean_hc <- round(mean(hc$Age, na.rm = TRUE), 1)

message("Mean age DMD patients: ", mean_dmd)
message("Mean age HC patients: ", mean_hc, "\n")


# Calculate mean of the age of DMD kids and HC kids in the combined kids dataset
mean_dmd_kids_age <- round(mean(dmd_kids$Age, na.rm = TRUE), 1)
mean_hc_kids_age <- round(mean(hc_kids$Age, na.rm = TRUE), 1)

message("Mean age of DMD kids:", mean_dmd_kids_age)
message("Mean age of HC kids:", mean_hc_kids_age, "\n")

# Calculate mean of the age of DMD and HC patients in the Netherlands and UK
mean_dmd_adults_nl_age <- round(mean(dmd_adults_nl$Age, na.rm = TRUE), 1)
mean_dmd_kids_nl_age <- round(mean(dmd_kids_nl$Age, na.rm = TRUE), 1)
mean_dmd_kids_uk_age <- round(mean(dmd_kids_uk$Age, na.rm = TRUE), 1)

message("Mean age of DMD adults in NL:", mean_dmd_adults_nl_age)
message("Mean age of DMD kids in NL:", mean_dmd_kids_nl_age)
message("Mean age of DMD kids in UK:", mean_dmd_kids_uk_age, "\n")

mean_hc_adults_nl_age <- round(mean(hc_adults_nl$Age, na.rm = TRUE), 1)
mean_hc_kids_nl_age <- round(mean(hc_kids_nl$Age, na.rm = TRUE), 1)
mean_hc_kids_uk_age <- round(mean(hc_kids_uk$Age, na.rm = TRUE), 1)

message("Mean age of HC adults in NL:", mean_hc_adults_nl_age)
message("Mean age of HC kids in NL:", mean_hc_kids_nl_age)
message("Mean age of HC kids in UK:", mean_hc_kids_uk_age, "\n")


# Calculate the standart deviation of the age of DMD and HC patients
sd_dmd_age <- round(sd(dmd$Age, na.rm = TRUE), 1)
sd_hc_age <- round(sd(hc$Age, na.rm = TRUE), 1)

message("SD of DMD age:", sd_dmd_age)
message("SD of HC age:", sd_hc_age, "\n")

# calculate the standard deviation of dmd kids and hc kids
sd_dmd_kids_age <- round(sd(dmd_kids$Age, na.rm = TRUE), 1)
sd_hc_kids_age <- round(sd(hc_kids$Age, na.rm = TRUE), 1)

message("SD of DMD kids age:", sd_dmd_kids_age)
message("SD of HC kids age:", sd_hc_kids_age, "\n")


# Calculate the standart deviation of the age
# of DMD and HC patients in the Netherlands and UK
sd_dmd_adults_nl_age <- round(sd(dmd_adults_nl$Age, na.rm = TRUE), 1)
sd_dmd_kids_nl_age <- round(sd(dmd_kids_nl$Age, na.rm = TRUE), 1)
sd_dmd_kids_uk_age <- round(sd(dmd_kids_uk$Age, na.rm = TRUE), 1)

message("SD of DMD adults in NL:", sd_dmd_adults_nl_age)
message("SD of DMD kids in NL:", sd_dmd_kids_nl_age)
message("SD of DMD kids in UK:", sd_dmd_kids_uk_age, "\n")

hc_adults_nl_age <- round(sd(hc_adults_nl$Age, na.rm = TRUE), 1)
hc_kids_nl_age <- round(sd(hc_kids_nl$Age, na.rm = TRUE), 1)
hc_kids_uk_age <- round(sd(hc_kids_uk$Age, na.rm = TRUE), 1)

message("SD of HC adults in NL:", hc_adults_nl_age)
message("SD of HC kids in NL:", hc_kids_nl_age)
message("SD of HC kids in UK:", hc_kids_uk_age, "\n")
