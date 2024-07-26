library(readxl)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(lme4)
library(lmerTest)
library(splines)
library(rms)
library(mgcv)


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



# # Example data for DMD patients and healthy controls
# hc_patients_first_visit <- data.frame(
#   ID = c(4, 4, 5, 5, 6, 6),
#   Age = c(9, 10, 11, 12, 13, 14),
#   TBV = c(1500, 1490, 1450, 1440, 1420, 1410),
#   Visit = c(1, 2, 1, 2, 1, 2),
#   Disease = c(0, 0, 0, 0, 0, 0)
# )

# hc_patients_second_visit <- data.frame(
#   ID = c(4, 4, 5, 5, 6, 6),
#   Age = c(11, 14, 16, 19, 23, 28),
#   TBV = c(2000, 3000, 4000, 5000, 6000, 9000),
#   Visit = c(2, 2, 2, 2, 2, 2),
#   Disease = c(0, 0, 0, 0, 0, 0)
# )

# dmd_patients_first_visit <- data.frame(
#   ID = c(1, 2, 3, 4, 5, 6),
#   Age = c(10, 11, 12, 13, 14, 15),
#   TBV = c(5000, 6000, 7000, 8000, 9000, 10000),
#   Visit = c(1, 1, 1, 1, 1, 1),
#   Disease = c(1, 1, 1, 1, 1, 1)
# )


# Combine the two datasets (dmd and hc) into one data frame named all_patients
all_patients <- rbind(dmd, hc)

#all_patients <- rbind(dmd_patients_first_visit, hc_patients_first_visit)

# mutate all_patients to add another column with group
# Assuming 'Disease' column where 1 indicates DMD and other values indicate HC
# Give dmd the group color red and hc the group color green
all_patients <- all_patients %>%
  mutate(group = ifelse(Disease == 1, "DMD", "HC"))


# Scatterplot of TBV ratio vs Age 
f <- ggplot(all_patients, aes(x = Age, y = TBV, color = group)) +
  geom_point(na.rm = TRUE) +
  labs(title = "Scatterplot of Brain Volume", x = "Age", y = "TBV", color = "Group") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_color_manual(values = c("DMD" = "red", "HC" = "green")) +
  #theme_ft_rc()
  theme_classic()

  ggsave("plots/tbv_ratio_age.png", plot = f)

# Fit a linear model to predict TBV_Ratio based on Age
model <- lm(TBV_Ratio ~ Age, data = all_patients)

# Print the model summary and coefficients
summary(model)
# Print the coefficients of the model
coef(model)

# Plot the diagnostic plots for the linear model
# 1: Residuals vs Fitted
# 2: Normal Q-Q
plot(model, c(1, 2))

# For the quadratic model, we can add a new column Age^2 to the data frame
all_patients <- all_patients %>%
  mutate(Age2 = Age^2)
  
all_patients %>%
  select(ID, Age, Age2) %>%
  mutate(Age2_formatted = sprintf("%.2f", Age2)) %>%
  print(n = Inf)



model.Age2 <- lm(TBV_Ratio ~ Age + Age2, data = all_patients)
summary(model.Age2)

# Check if all necessary columns exist in the dataset
required_columns <- c("ID", "Disease", "Age", "Age2", "Scannertype", "Headcoil", "Mutation", "Corticosteroid")
missing_columns <- setdiff(required_columns, colnames(all_patients))

if (length(missing_columns) > 0) {
  stop(paste("The following required columns are missing in the dataset:", paste(missing_columns, collapse = ", ")))
}

## print patient ID and group column
patient_with_group <- all_patients %>%
  select(ID, group) %>%
  arrange(ID)

# print(patient_with_group)


# model_tbv_ratio <- lmer(TBV_Ratio ~ Age + Age2 + group + Scannertype + Headcoil + Mutation + Corticosteroid + (1 | ID), data = all_patients)
# #model_tbv_ratio <- lmer(TBV_Ratio ~ Age + Age2 + group + Scannertype + Headcoil + Mutation + Corticosteroid, data = all_patients)
# print(summary(model_tbv_ratio))


# Notes
# In R, when you use the lmer function from the lme4 package to fit a linear mixed-effects model
# the summary output does not include p-values for the fixed effects by default
# This is because calculating p-values for mixed models is complex and can be controversial
# However, we can obtain p-values for the fixed effects using the lmerTest package, which extends lme4 to provide p-values based on Satterthwaite's degrees of freedom method
# To use lmerTest, you need to install and load the package, and then fit the model using lmerTest::lmer instead of lmer
# Fit the model using lmerTest::lmer
model_tbv_ratio <- lmer(TBV_Ratio ~ Age + Age2 + group + Scannertype + Headcoil + Mutation + Corticosteroid + (1 | ID), data = all_patients)
# Print the summary, which now includes p-values
print(summary(model_tbv_ratio))
message("\n")

message("Model with interaction effect")

model_tbv_ratio_ie <- lmer(TBV_Ratio ~ group * Age + I(Age^2) + Scannertype + Headcoil + Mutation + Corticosteroid + (1 | ID), data = all_patients)
print(summary(model_tbv_ratio_ie))



message("\n")
message("Model with random intercept and slope")
model2 <- lm(TBV_Ratio ~ group * Age + (Age^2), data = all_patients)
summary(model2)

message("\n")
message("Model with random intercept for ID")
# Fit a linear mixed-effects model with random intercept for ID
model3 <- lmer(TBV_Ratio ~ group + Age + (Age^2) + Scannertype + (1 | ID), data = all_patients)
print(summary(model3))

message("\n")
message("Model 4")
model4 <- lmer(TBV_Ratio ~ group + Age + (Age^2) + Headcoil + (1 | ID), data = all_patients)
print(summary(model4))

message("\n")
message("Model 5")
model5 <- lmer(TBV_Ratio ~ group + Age + (Age^2) + Mutation + (1 | ID), data = all_patients)
print(summary(model5))

message("\n")
message("Model 6")
model6 <- lmer(TBV_Ratio ~ group + Age + (Age^2) + Corticosteroid + (1 | ID), data = all_patients)
print(summary(model6))

message("\n")
message("Model 7")
model7 <- lmer(TBV_Ratio ~ group + Age + I(Age^2) + group:Age + Scannertype + Headcoil + Mutation + Corticosteroid + (1 | ID), data = all_patients)
print(summary(model7))

message("\n")
message("Model 8")
model8 <- lmer(TBV_Ratio ~ group + Age + I(Age^2) + group:(Age^2) + Scannertype + Headcoil + Mutation + Corticosteroid + (1 | ID), data = all_patients)
print(summary(model8))

message("\n")
message("Model 9")
model9 <- lmer(TBV_Ratio ~ group * ns(Age, df = 4) + I(Age^2) + Scannertype + (1 | ID), data = all_patients)
print(summary(model9))

message("\n")

# use rms package 
model_tbv_ratio_splines <- lmer(TBV_Ratio ~ group + rcs(Age, 4) + I(Age^2) + Scannertype + Headcoil + Mutation + Corticosteroid + (1 | ID), data = all_patients)
print(summary(model_tbv_ratio_splines))

message("\n")
model_tbv_ratio_splines_2 <- lmer(TBV_Ratio ~ group + rcs(Age, 4) + I(Age^2) + Scannertype + (1 | ID), data = all_patients)
print(summary(model_tbv_ratio_splines_2))



model_tbv_ratio_splines_3 <- lmer(TBV_Ratio ~ group + rcs(Age, 4) + I(Age^2) + (1 | ID), data = all_patients)
print(summary(model_tbv_ratio_splines_3))


message("\n")
message("-----------------------------------------------------------")

model9_AIC_value <- AIC(model9)
model9_BIC_value <- BIC(model9)
model9_logLik_value <- logLik(model9)

message("Model 9 AIC value: ", model9_AIC_value)
message("Model 9 BIC value: ", model9_BIC_value)
message("Model 9 logLik value: ", model9_logLik_value)

message("\n")

model_tbv_ratio_splines_AIC_value <- AIC(model_tbv_ratio_splines)
model_tbv_ratio_splines_BIC_value <- BIC(model_tbv_ratio_splines)
model_tbv_ratio_splines_logLik_value <- logLik(model_tbv_ratio_splines)

message("Model TBV Ratio Splines AIC value: ", model_tbv_ratio_splines_AIC_value)
message("Model TBV Ratio Splines BIC value: ", model_tbv_ratio_splines_BIC_value)
message("Model TBV Ratio Splines logLik value: ", model_tbv_ratio_splines_logLik_value)

message("\n")

model_tbv_ratio_splines_2_AIC_value <- AIC(model_tbv_ratio_splines_2)
model_tbv_ratio_splines_2_BIC_value <- BIC(model_tbv_ratio_splines_2)
model_tbv_ratio_splines_2_logLik_value <- logLik(model_tbv_ratio_splines_2)

message("Model TBV Ratio Splines 2 AIC value: ", model_tbv_ratio_splines_2_AIC_value)
message("Model TBV Ratio Splines 2 BIC value: ", model_tbv_ratio_splines_2_BIC_value)
message("Model TBV Ratio Splines 2 logLik value: ", model_tbv_ratio_splines_2_logLik_value)

message("\n")

model_tbv_ratio_splines_3_AIC_value <- AIC(model_tbv_ratio_splines_3)
model_tbv_ratio_splines_3_BIC_value <- BIC(model_tbv_ratio_splines_3)
model_tbv_ratio_splines_3_logLik_value <- logLik(model_tbv_ratio_splines_3)

message("Model TBV Ratio Splines 3 AIC value: ", model_tbv_ratio_splines_3_AIC_value)
message("Model TBV Ratio Splines 3 BIC value: ", model_tbv_ratio_splines_3_BIC_value)
message("Model TBV Ratio Splines 3 logLik value: ", model_tbv_ratio_splines_3_logLik_value)


# Check for non-numeric or missing values in the Age column
if (any(is.na(all_patients$Age)) || any(!is.finite(all_patients$Age))) {
  # Handle missing or non-numeric values in Age
  all_patients <- all_patients %>%
    filter(!is.na(Age) & is.finite(Age))
}


fit <- gam(TBV_Ratio ~ s(Age, k = 4), data = all_patients)

# fit_dmd <- gam(TBV ~ s(Age, k = 4), data = subset(all_patients, group == "DMD"))
# fit_hc <- gam(TBV ~ s(Age, k = 4), data = subset(all_patients, group == "HC"))

fit_dmd <- gam(TBV_Ratio ~ s(Age, k = 4), data = subset(all_patients, group == "DMD"))
fit_hc <- gam(TBV_Ratio ~ s(Age, k = 4), data = subset(all_patients, group == "HC"))

# Ensure the Age column is numeric
all_patients$Age <- as.numeric(all_patients$Age)
#age_range <- seq(min(all_patients$Age, na.rm = TRUE), max(all_patients$Age, na.rm = TRUE), length.out = 100)
age_range <- seq(min(all_patients$Age), max(all_patients$Age), length.out = 100)

# # Create a new data frame for prediction
# new_data <- data.frame(
#   Age = age_range,
#   Group = rep("HC", length(age_range)) # Predicting for DMD group, repeat for HC if needed
# )

pred_dmd <- predict(fit_dmd, newdata = data.frame(Age = age_range), se.fit = TRUE)
dmd_data <- data.frame(
  Age = age_range,
  TBV = pred_dmd$fit,
  se = pred_dmd$se.fit,
  upper = pred_dmd$fit + 1.96 * pred_dmd$se.fit,
  lower = pred_dmd$fit - 1.96 * pred_dmd$se.fit,
  group = "DMD"
)


pred_hc <- predict(fit_hc, newdata = data.frame(Age = age_range), se.fit = TRUE)
hc_data <- data.frame(
  Age = age_range,
  TBV = pred_hc$fit,
  se = pred_hc$se.fit,
  upper = pred_hc$fit + 1.96 * pred_hc$se.fit,
  lower = pred_hc$fit - 1.96 * pred_hc$se.fit,
  group = "HC"
)

pred_data <- rbind(dmd_data, hc_data)

# Now create the plot
p <- ggplot() +
  geom_point(data = all_patients, aes(x = Age, y = TBV_Ratio, color = group), na.rm = TRUE) +
  geom_line(data = pred_data, aes(x = Age, y = TBV, color = group)) +
  geom_ribbon(data = pred_data, aes(x = Age, ymin = lower, ymax = upper, fill = group), alpha = 0.2) +
  labs(title = "Spline plot TBV Ratio vs Age", x = "Age", y = "TBV Ratio", color = "Group", fill = "Group") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("DMD" = "red", "HC" = "green")) +
  scale_fill_manual(values = c("DMD" = "red", "HC" = "green")) +
  theme_classic()

# Now create the plot
p <- ggplot() +
  geom_point(data = all_patients, aes(x = Age, y = TBV_Ratio, color = group), na.rm = TRUE) +
  geom_line(data = pred_data, aes(x = Age, y = TBV, color = group)) +
  geom_ribbon(data = pred_data, aes(x = Age, ymin = lower, ymax = upper, fill = group), alpha = 0.2) +
  labs(title = "Spline plot TBV Ratio vs Age", x = "Age", y = "TBV Ratio", color = "Group", fill = "Group") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("DMD" = "red", "HC" = "green")) +
  scale_fill_manual(values = c("DMD" = "red", "HC" = "green")) +
  theme_classic()


# predictions <- predict(fit, newdata = new_data, se.fit = TRUE)
# # Add predictions and confidence intervals to the new data frame
# new_data$TBV <- predictions$fit
# new_data$se <- predictions$se.fit
# new_data$upper <- new_data$TBV + 1.96 * new_data$se
# new_data$lower <- new_data$TBV - 1.96 * new_data$se


# new_data_dmd <- data.frame(
#   Age = age_range,
#   Group = rep("DMD", length(age_range)) # Predicting for DMD group, repeat for HC if needed
# )

# new_data_hc <- data.frame(
#   Age = age_range,
#   Group = rep("HC", length(age_range)) # Predicting for DMD group, repeat for HC if needed
# )

# # Plot the spline fit with confidence intervals
# p <- ggplot() +
#   geom_point(data = all_patients, aes(x = Age, y = TBV_Ratio, color = group), na.rm = TRUE) +
#   geom_line(data = new_data, aes(x = Age, y = TBV, color = Group)) +
#   geom_ribbon(data = new_data, aes(x = Age, ymin = lower, ymax = upper, fill = Group), alpha = 0.2) +
#   labs(title = "Spline plot TBV Ratio vs Age", x = "Age", y = "TBV Ratio", color = "Group", fill = "Group") +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   scale_color_manual(values = c("DMD" = "red", "HC" = "green")) +
#   scale_fill_manual(values = c("DMD" = "red", "HC" = "green")) +
#   theme_classic()

# # # Print the first few rows of new_data to check its contents
# # print(head(new_data))

# # p <- ggplot() +
# #   geom_point(data = all_patients, aes(x = Age, y = TBV_Ratio, color = group), na.rm = TRUE) +
# #   geom_line(data = new_data, aes(x = Age, y = TBV), color = "blue") +
# #   geom_ribbon(data = new_data, aes(x = Age, ymin = lower, ymax = upper), fill = "gray", alpha = 0.2) +
# #   labs(title = "Spline plot TBV Ratio vs Age", x = "Age", y = "TBV Ratio", color = "Group") +
# #   theme(plot.title = element_text(hjust = 0.5)) +
# #   scale_color_manual(values = c("DMD" = "red", "HC" = "green")) +
# #   theme_classic()



 ggsave("plots/spline_plot_tbv_ratio_age.png", plot = p)