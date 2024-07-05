library(readxl)
library(dplyr)
library(ggplot2)


file_path <- "DATASET_V2_RG.xlsx"
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
  TBV = c(9999)
)

# Create dmd and hc subsets
# dmd <- dataset %>% filter(Disease == 1)
# hc <- dataset %>% filter(Disease == 0)
dmd <- dataset[dataset$Disease == 1, ]
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
par(mfrow = c(1, 3))
hist(dmd_adults_nl$Age, main = "Age distribution of DMD adults patients", xlab = "Age", col = "blue", breaks = 20)
hist(dmd_kids_nl$Age, main = "Age distribution of DMD kids patients in NL", xlab = "Age", col = "red", breaks = 20)
hist(dmd_kids_uk$Age, main = "Age distribution of DMD kids patients in UK", xlab = "Age", col = "green", breaks = 20)

hist(hc_adults_nl$Age, main = "Age distribution of HC adults patients in NL", xlab = "Age", col = "blue", breaks = 20)
hist(hc_kids_nl$Age, main = "Age distribution of HC kids patients in NL", xlab = "Age", col = "red", breaks = 20)
hist(hc_kids_uk$Age, main = "Age distribution of HC kids patients in UK", xlab = "Age", col = "green", breaks = 20)


# Create Q-Q plots for the age of DMD and HC patients
par(mfrow = c(1, 3))
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


# ScannerType
# note, can also be done with filter
# such as: dmd_adults_nl_8ch <- dmd_adults_nl %>% filter(ScannerType == 0)
# 0 = 8 channel headcoil
# 1 = 32 channel headcoil
# 9 = missing data  
dmd_adults_nl_8ch <- dmd_adults_nl[dmd_adults_nl$ScannerType == 0, ]
dmd_adults_nl_32ch <- dmd_adults_nl[dmd_adults_nl$ScannerType == 1, ]
dmd_adults_nl_missing <- dmd_adults_nl[dmd_adults_nl$ScannerType == 9, ]
message("DMD adults in NL with 8 channel headcoil:", nrow(dmd_adults_nl_8ch))
message("DMD adults in NL with 32 channel headcoil:", nrow(dmd_adults_nl_32ch))
message("DMD adults in NL with missing data:", nrow(dmd_adults_nl_missing), "\n")


dmd_kids_nl_8ch <- dmd_kids_nl[dmd_kids_nl$ScannerType == 0, ]
dmd_kids_nl_32ch <- dmd_kids_nl[dmd_kids_nl$ScannerType == 1, ]
dmd_kids_nl_missing <- dmd_kids_nl[dmd_kids_nl$ScannerType == 9, ]
message("DMD kids in NL with 8 channel headcoil:", nrow(dmd_kids_nl_8ch))
message("DMD kids in NL with 32 channel headcoil:", nrow(dmd_kids_nl_32ch))
message("DMD kids in NL with missing data:", nrow(dmd_kids_nl_missing), "\n")

dmd_kids_uk_8ch <- dmd_kids_uk[dmd_kids_uk$ScannerType == 0, ]
dmd_kids_uk_32ch <- dmd_kids_uk[dmd_kids_uk$ScannerType == 1, ]
dmd_kids_uk_missing <- dmd_kids_uk[dmd_kids_uk$ScannerType == 9, ]
message("DMD kids in UK with 8 channel headcoil:", nrow(dmd_kids_uk_8ch))
message("DMD kids in UK with 32 channel headcoil:", nrow(dmd_kids_uk_32ch))
message("DMD kids in UK with missing data:", nrow(dmd_kids_uk_missing), "\n")


hc_adults_nl_8ch <- hc_adults_nl[hc_adults_nl$ScannerType == 0, ]
hc_adults_nl_32ch <- hc_adults_nl[hc_adults_nl$ScannerType == 1, ]
hc_adults_nl_missing <- hc_adults_nl[hc_adults_nl$ScannerType == 9, ]

message("HC adults in NL with 8 channel headcoil:", nrow(hc_adults_nl_8ch))
message("HC adults in NL with 32 channel headcoil:", nrow(hc_adults_nl_32ch))
message("HC adults in NL with missing data:", nrow(hc_adults_nl_missing), "\n")

hc_kids_nl_8ch <- hc_kids_nl[hc_kids_nl$ScannerType == 0, ] 
hc_kids_nl_32ch <- hc_kids_nl[hc_kids_nl$ScannerType == 1, ]
hc_kids_nl_missing <- hc_kids_nl[hc_kids_nl$ScannerType == 9, ]

message("HC kids in NL with 8 channel headcoil:", nrow(hc_kids_nl_8ch))
message("HC kids in NL with 32 channel headcoil:", nrow(hc_kids_nl_32ch))
message("HC kids in NL with missing data:", nrow(hc_kids_nl_missing), "\n") 

hc_kids_uk_8ch <- hc_kids_uk[hc_kids_uk$ScannerType == 0, ]
hc_kids_uk_32ch <- hc_kids_uk[hc_kids_uk$ScannerType == 1, ]
hc_kids_uk_missing <- hc_kids_uk[hc_kids_uk$ScannerType == 9, ]

message("HC kids in UK with 8 channel headcoil:", nrow(hc_kids_uk_8ch))
message("HC kids in UK with 32 channel headcoil:", nrow(hc_kids_uk_32ch))
message("HC kids in UK with missing data:", nrow(hc_kids_uk_missing), "\n")

# # Plot of brain volume on the y-axis and the age on the x-axis
# # healthy controls are in blue and the DMD patients are in red
# # TBV column is the total brain volume
# dmd_patients <- rbind(dmd_adults_nl, dmd_kids_nl, dmd_kids_uk) %>% mutate(group = "red")
# hc_patients <- rbind(hc_adults_nl, hc_kids_nl, hc_kids_uk) %>% mutate(group = "blue")
# all_patients <- replace_values_with_na_mutate(rbind(dmd_patients, hc_patients), rules)

# all_dmd_patients_first_visit <- dmd_patients %>% filter(Visit == 1)
# all_dmd_patients_second_visit <- dmd_patients %>% filter(Visit == 2)
# all_hc_patients_first_visit <- hc_patients %>% filter(Visit == 1)
# all_hc_patients_second_visit <- hc_patients %>% filter(Visit == 2)


# # Combine first and second visit data for plotting lines
# dmd_patients_visits <- rbind(all_dmd_patients_first_visit, all_dmd_patients_second_visit)
# hc_patients_visits <- rbind(all_hc_patients_first_visit, all_hc_patients_second_visit)


# # Each patient has a unique ID, which is in the column called "ID", so we can use this to identify the patients
# # We plot the brain volume on the y-axis and the age on the x-axis
# # We color the points based on the group (DMD or HC)
# # We have a subset of dmd patients and healthy controls first and second visit
# # We can also use this to compare the brain volume of the same patients between their first visit and second visit 
# # Draw a line from their first visit which is a dot to their second visit which is another dot
# # But the dots of of the patient at their first visit has to connect to their second visit
# # Not draw a line from one close dot to another close dot
# # It should connect the dots of the same patient


# Example data for DMD patients and healthy controls
hc_patients_first_visit <- data.frame(
  ID = c(4, 4, 5, 5, 6, 6),
  Age = c(9, 10, 11, 12, 13, 14),
  TBV = c(1500, 1490, 1450, 1440, 1420, 1410),
  Visit = c(1, 2, 1, 2, 1, 2),
  group = "blue"
)

hc_patients_second_visit <- data.frame(
  ID = c(4, 4, 5, 5, 6, 6),
  Age = c(11, 14, 16, 19, 23, 28),
  TBV = c(2000, 3000, 4000, 5000, 6000, 9000),
  Visit = c(2, 2, 2, 2, 2, 2),
  group = "blue"
)



dmd_patients_first_visit <- data.frame(
  ID = c(1, 2, 3, 4, 5, 6),
  Age = c(10, 11, 12, 13, 14, 15),
  TBV = c(5000, 6000, 7000, 8000, 9000, 10000),
  Visit = c(1, 1, 1, 1, 1, 1),
  group = "DMD"
)

dmd_patients_second_visit <- data.frame(
  ID = c(1, 2, 3, 4, 5, 6),
  Age = c(20, 21, 22, 33, 34, 40),
  TBV = c(4000, 5000, 6000, 7000, 8000, 9000),
  Visit = c(1, 2, 2, 2, 2, 2),
  group = "DMD"
)




# Combine first and second visit data for plotting lines
dmd_patients_visits <- rbind(dmd_patients_first_visit, dmd_patients_second_visit)
hc_patients_visits <- rbind(hc_patients_first_visit, hc_patients_second_visit)

# Combine first and second visit data for plotting lines
dmd_patients_visits <- rbind(dmd_patients_first_visit, dmd_patients_second_visit)
hc_patients_visits <- rbind(hc_patients_first_visit, hc_patients_second_visit)

# Create the scatter plot
test <- ggplot() +
  geom_point(data = dmd_patients_first_visit, aes(x = Age, y = TBV, color = group), size = 2, shape = 16) +
  geom_point(data = dmd_patients_second_visit, aes(x = Age, y = TBV, color = group), size = 2, shape = 17) +
  geom_line(data = dmd_patients_visits, aes(x = Age, y = TBV, group = ID, color = group), linetype = "dashed") +
  
  # Plot HC patients
  #geom_line(data = dmd_patients_visits, aes(x = Age, y = TBV, group = ID), color = "red", linetype = "dashed") +
  
  # # Plot HC patients
  # geom_point(data = hc_patients_first_visit, aes(x = Age, y = TBV, color = group), size = 2) +
  # geom_point(data = hc_patients_second_visit, aes(x = Age, y = TBV, color = group), size = 2) +
  # geom_line(data = hc_patients_visits, aes(x = Age, y = TBV, group = ID), color = "blue", linetype = "dashed") +
  
  # Labels and Theme
  labs(title = "Brain Volume (TBV) vs Age",
       x = "Age (years)",
       y = "Total Brain Volume in cm^3 (TBV)",
       color = "Group") +
  scale_color_manual(values = c("DMD" = "red", "blue" = "blue")) +
  theme_classic()

# Save the plot to a file
ggsave("scatter_plot.png", plot = test)


# # Now make it a scatterplot 
# ggplot(all_patients, aes(x = Age, y = TBV, color = group)) +
#   geom_point(
#     na.rm = TRUE,
#   )
#   labs(title = "Scatterplot of brain volume on the y-axis and the age on
#   x-axis",
#     x = "Age",
#     y = "TBV",
#     color = "Group") +
#     theme(plot.title = element_text(hjust = 0.5)) +
#     scale_color_manual(values = c("blue", "red")) +
#     geom_smooth(method = "lm", se = FALSE, color = "black") +
#     theme_minimal()

#ggplot(data = all_patients)
#print(all_patients)