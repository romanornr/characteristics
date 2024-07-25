library(readxl)
library(dplyr)
library(ggplot2)
library(hrbrthemes)

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






# # print patient with group table, disease table and id column 
# patient_with_group <- all_patients %>%
#   select(ID, Disease, group) %>%
#   arrange(Disease)

# disease_table <- all_patients %>%
#   select(Disease, ID) %>%
#   distinct() %>%
#   arrange(Disease)


# print(patient_with_group)
# print(disease_table)


