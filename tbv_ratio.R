library(readxl)
library(dplyr)
library(ggplot2)
library(car)

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

# Mean of the TVB ratio
dmd_mean_tbv_ratio <- round(mean(dmd$TBV_Ratio, na.rm = TRUE), 1)
hc_mean_tbv_ratio <- round(mean(hc$TBV_Ratio, na.rm = TRUE), 1)
message("Mean of the TBV ratio of DMD patients:", dmd_mean_tbv_ratio)
message("Mean of the TBV ratio of HC patients:", hc_mean_tbv_ratio)

message("\n")

# Standard deviaton
hc_sd_tbv_ratio <- round(sd(hc$TBV_Ratio, na.rm = TRUE), 1)
dmd_sd_tbv_ratio <- round(sd(dmd$TBV_Ratio, na.rm = TRUE), 1)
message("Standard deviation of the TBV ratio of DMD patients:", dmd_sd_tbv_ratio)
message("Standard deviation of the TBV ratio of HC patients:", hc_sd_tbv_ratio)

# Histogram
par(mfrow = c(1, 2))
hist(dmd$TBV_Ratio, main = "TBV ratio of DMD patients", xlab = "TBV ratio DMD", col = "red", breaks = 20)
hist(hc$TBV_Ratio, main = "TBV ratio of HC patients", xlab = "TBV ratio HC", col = "green", breaks = 20)


## QQ plot
par(mfrow = c(1, 2))
qqnorm(dmd$TBV_Ratio, main = "QQ plot of TBV ratio of DMD patients")
qqline(dmd$TBV_Ratio, col = "red")
qqnorm(hc$TBV_Ratio, main = "QQ plot of TBV ratio of HC patients")
qqline(hc$TBV_Ratio, col = "green")

# Boxplot
par(mfrow = c(1, 2))
boxplot(dmd$TBV_Ratio, main = "TBV ratio of DMD patients", col = "red")
boxplot(hc$TBV_Ratio, main = "TBV ratio of HC patients", col = "green")


# # Colored Histogram with Different Number of Bins
# hist(mtcars$mpg, breaks=12, col="red")

# Density plot with na.rm = TRUE
par(mfrow = c(1, 2))
plot(density(dmd$TBV_Ratio, na.rm = TRUE), main = "Density TBV ratio of DMD", col = "red")
plot(density(hc$TBV_Ratio, na.rm = TRUE), main = "Density TBV ratio of HC", col = "green")

# plot(density(dmd$TBV_Ratio), main = "TBV ratio of DMD patients", col = "red")
# plot(density(hc$TBV_Ratio), main = "TBV ratio of HC patients", col = "green")

# Scatter plot
par(mfrow = c(1, 2))
plot(dmd$Age, dmd$TBV_Ratio, main = "Scatter Age and TBV ratio of DMD", col = "red")
plot(hc$Age, hc$TBV_Ratio, main = "Scatter Age and TBV ratio of HC", col = "green")

# Correlation
correlation_dmd <- round(cor(dmd$Age, dmd$TBV_Ratio, use = "complete.obs"), 2)
correlation_hc <- round(cor(hc$Age, hc$TBV_Ratio, use = "complete.obs"), 2)
message("Correlation between Age and TBV ratio of DMD:", correlation_dmd)
message("Correlation between Age and TBV ratio of HC:", correlation_hc)

# Linear regression
lm_dmd <- lm(dmd$TBV_Ratio ~ dmd$Age)
lm_hc <- lm(hc$TBV_Ratio ~ hc$Age)
message("Linear regression of DMD:", summary(lm_dmd))
message("Linear regression of HC:", summary(lm_hc))

# Plot linear regression
par(mfrow = c(1, 2))
plot(dmd$Age, dmd$TBV_Ratio, main = "Linear regression of DMD", col = "red")
abline(lm_dmd, col = "blue")
plot(hc$Age, hc$TBV_Ratio, main = "Linear regression of HC", col = "green")
abline(lm_hc, col = "blue")

message("\n")


print(shapiro.test(dmd$TBV))
print(shapiro.test(hc$TBV))