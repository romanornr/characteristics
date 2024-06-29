library(readxl)

file_path <- "DATASET_V2_RG.xlsx"
if (!file.exists(file_path)) stop("File does not exist")
dataset <- read_excel(file_path)

# Create dmd subset (Disease == 1)
dmd <- dataset[dataset$Disease == 1, ]

# Create HC (healthy controls) subset (Disease == 0)
hc <- dataset[dataset$Disease == 0, ]

message("DMD group amount of patients:", nrow(dmd), "\n")
message("HC group amount of patients:", nrow(dmd), "\n")

# Print column names to check if the data is loaded correctly
message("Columns in DMD and HC:")
print(names(dmd))

# Print the first 12 rows of DMD
message("First 12 rows of DMD:")
print(head(dmd, 12))

# Subset dmd patients from the Netherlands who are adults
dmd_adults_netherlands <- dmd[grepl("^DMDBA", dmd$ID), ]
# Subset dmd patients from the Netherlands who are kids
dmd_kids_netherlands <- dmd[grepl("^DMDBNL", dmd$ID), ]
# Subset dmd patients from the UK who are kids
dmd_kids_uk <- dmd[grepl("^DMDBUK", dmd$ID), ]

# Print first 5 rows of adults_netherlands
message("First 5 rows of adults_netherlands:")
print(head(dmd_adults_netherlands, 5))

message("First 5 rows of kids_netherlands:")
print(head(dmd_kids_netherlands, 5))

message("First 5 rows of kids_uk:")
print(head(dmd_kids_uk, 5))
