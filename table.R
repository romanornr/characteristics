library(readxl)
library(dplyr)


# # Check if dplyr is installed and loaded
# if (!requireNamespace("dplyr", quietly = TRUE)) {
# 	stop("dplyr is not installed")
# }

# # Load dplyr if it's not already loaded
# if (!"package:dplyr" %in% search()) {
# 	library(dplyr)
# 	message("dplyr loaded")}

file_path <- "DATASET_V2_RG.xlsx"
if (!file.exists(file_path)) stop("File does not exist")
dataset <- read_excel(file_path)


# replace_values_with_na_mutate <- function(df, rules) {
#   df %>%
#     mutate(across(
#       .cols = names(rules),
#       .fns = ~ if_else(. %in% rules[[cur_column()]], NA_real_, .)
#     ))
# }

replace_values_with_na_mutate <- function(df, rules) {
	dplyr::mutate(df, dplyr::across(
		.cols = names(rules),
		.fns = ~ dplyr::if_else(. %in% rules[[dplyr::cur_column()]], NA_real_, .)
	))
}

rules <- list(
  Age = c(9999)
)


dmd <- dataset[dataset$Disease == 1, ]

# Create HC (healthy controls) subset (Disease == 0)
hc <- dataset[dataset$Disease == 0, ]

message("DMD group amount of patients: ", nrow(dmd), "\n")
message("HC group amount of patients: ", nrow(hc), "\n")

# dmd$Age[dmd$Age == 9999] <- NA
# hc$Age[hc$Age == 9999] <- NA

dmd <- replace_values_with_na_mutate(dmd, rules)
hc <- replace_values_with_na_mutate(hc, rules)

# # Print column names to check if the data is loaded correctly
# message("Columns in DMD and HC:")
# print(names(dmd))

# # Print the first 12 rows of DMD
# message("First 12 rows of DMD:")
# print(head(dmd, 12))

# Subset dmd patients from the Netherlands who are adults, kids and UK kids
dmd_adults_nl <- dmd[grepl("^DMDBA", dmd$ID), ]
dmd_kids_nl <- dmd[grepl("^DMDBNL", dmd$ID), ]
dmd_kids_uk <- dmd[grepl("^DMDBUK", dmd$ID), ]

message("DMD amount of adults:", nrow(dmd_adults_nl))
message("DMD amount of NL kids:", nrow(dmd_kids_nl))
message("DMD amount of UK kids:", nrow(dmd_kids_uk), "\n")

#Subset HC patients from the Netherlands who are adults, kids and UK kids
hc_adults_nl <- hc[grepl("^DMDBA", hc$ID), ]
hc_kids_nl <- hc[grepl("^DMDBNL", hc$ID), ]
hc_kids_uk <- hc[grepl("^DMDBUK", hc$ID), ]

message("HC amount of adults:", nrow(hc_adults_nl))
message("HC amount of NL kids:", nrow(hc_kids_nl))
message("HC amount of UK kids:", nrow(hc_kids_uk), "\n")


# Calculate mean of the age of DMD and HC patients
mean_dmd_age <- round(mean(dmd$Age, na.rm = TRUE), 1)
mean_age_hc <- round(mean(hc$Age, na.rm = TRUE), 1)

message("Mean age of DMD:", mean_dmd_age)
message("Mean age of HC:", mean_age_hc, "\n")


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


# Create histograms, Q-Q plots and boxplots for the age of DMD and HC patients
hist(dmd_adults_nl$Age, main = "Age distribution of DMD adults patients", xlab = "Age", col = "blue", breaks = 20)
hist(dmd_kids_nl$Age, main = "Age distribution of DMD kids patients in NL", xlab = "Age", col = "red", breaks = 20)
hist(dmd_kids_uk$Age, main = "Age distribution of DMD kids patients in UK", xlab = "Age", col = "green", breaks = 20)

hist(hc_adults_nl$Age, main = "Age distribution of HC adults patients in NL", xlab = "Age", col = "blue", breaks = 20)
hist(hc_kids_nl$Age, main = "Age distribution of HC kids patients in NL", xlab = "Age", col = "red", breaks = 20)
hist(hc_kids_uk$Age, main = "Age distribution of HC kids patients in UK", xlab = "Age", col = "green", breaks = 20)


# Create Q-Q plots for the age of DMD and HC patients
qqnorm(dmd_adults_nl$Age, main = "Q-Q plot of DMD adults patients in NL"); qqline(dmd_adults_nl$Age)
qqnorm(dmd_kids_nl$Age, main = "Q-Q plot of DMD kids patients in NL"); qqline(dmd_kids_nl$Age)
qqnorm(dmd_kids_uk$Age, main = "Q-Q plot of DMD kids patients in UK"); qqline(dmd_kids_uk$Age)

qqnorm(hc_adults_nl$Age, main = "Q-Q plot of HC adults patients in NL"); qqline(hc_adults_nl$Age)
qqnorm(hc_kids_nl$Age, main = "Q-Q plot of HC kids patients in NL"); qqline(hc_kids_nl$Age)
qqnorm(hc_kids_uk$Age, main = "Q-Q plot of HC kids patients in UK"); qqline(hc_kids_uk$Age)


# boxplot of the age of DMD and HC patients
par(mfrow = c(1, 3))
boxplot(dmd_adults_nl$Age, ylab = "age", main = "Boxplot of DMD adults patients in NL", col = "blue")
boxplot(dmd_kids_nl$Age, main = "Boxplot of DMD kids patients in NL", col = "red")
boxplot(dmd_kids_uk$Age, main = "Boxplot of DMD kids patients in UK", col = "green")

par(mfrow = c(1, 3))
boxplot(hc_adults_nl$Age, ylab= "age", main = "Boxplot of HC adults patients in NL", col = "blue")
boxplot(hc_kids_nl$Age, main = "Boxplot of HC kids patients in NL", col = "red")
boxplot(hc_kids_uk$Age, main = "Boxplot of HC kids patients in UK", col = "green")


# Perform Shapiro-Wilk normality test for the age of DMD and HC patients
dmd_adults_nl_shapiro <- shapiro.test(dmd_adults_nl$Age)
print(dmd_adults_nl_shapiro)

dmd_kids_nl_shapiro <- shapiro.test(dmd_kids_nl$Age)
print(dmd_kids_nl_shapiro)

dmd_kids_uk_shapiro <- shapiro.test(dmd_kids_uk$Age)
print(dmd_kids_uk_shapiro)


hc_adults_nl_shapiro <- shapiro.test(hc_adults_nl$Age)
print(hc_adults_nl_shapiro)

hc_kids_nl_shapiro <- shapiro.test(hc_kids_nl$Age)
print(hc_kids_nl_shapiro)

hc_kids_uk_shapiro <- shapiro.test(hc_kids_uk$Age)
print(hc_kids_uk_shapiro)


# # Print first 5 rows of adults_netherlands
# message("First 5 rows of adults_netherlands:")
# print(head(dmd_adults_netherlands, 5))

# message("First 5 rows of kids_netherlands:")
# print(head(dmd_kids_netherlands, 5))

# message("First 6 rows of kids_uk:")
# print(head(dmd_kids_uk, 5))
