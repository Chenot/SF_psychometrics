## demographics.R
# Author: Quentin Chenot
# Date: 2025-11-06
# Description: This script calculates demographic statistics from the dataset.
#
# Variables analyzed:
#   - Age (continuous)
#   - Education Level (continuous, in years)
#   - Video Game Experience (continuous, in years)
#   - Sex (categorical: man/woman)
#   - Handedness (categorical: left-handed/right-handed)
#
# Output:
#   Returns a list with all demographic statistics

################################################################################
## SETUP
################################################################################

# Set working directory to script location
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  this_file <- rstudioapi::getSourceEditorContext()$path
  this_dir <- dirname(this_file)
  setwd(this_dir)
}

# Source utility functions
source("utils.R")

# Load required packages
required_packages <- c("dplyr", "rstudioapi")
load_packages(required_packages)

# Path management
this_file <- rstudioapi::getSourceEditorContext()$path
this_dir <- dirname(this_file)
setwd(this_dir)
project_dir <- dirname(dirname(this_dir))

# Load data (non-z-scored for demographics)
df_final <- load_data(project_dir, z_scored = FALSE)

################################################################################
## CALCULATE DEMOGRAPHICS
################################################################################

# Sample size and missing data
n_total <- nrow(df_final)
demo_vars <- c("Age", "EducationLevel", "VGexp", "Sex", "Handedness")
missing_summary <- sapply(demo_vars, function(var) {
  sum(is.na(df_final[[var]]))
})

# Continuous variables summary statistics
summary_stats <- df_final %>%
  summarise(
    # Age
    MeanAge = mean(Age, na.rm = TRUE),
    SdAge = sd(Age, na.rm = TRUE),
    MedianAge = median(Age, na.rm = TRUE),
    MinAge = min(Age, na.rm = TRUE),
    MaxAge = max(Age, na.rm = TRUE),
    
    # Education Level
    MeanEL = mean(EducationLevel, na.rm = TRUE),
    SdEL = sd(EducationLevel, na.rm = TRUE),
    MedianEL = median(EducationLevel, na.rm = TRUE),
    MinEL = min(EducationLevel, na.rm = TRUE),
    MaxEL = max(EducationLevel, na.rm = TRUE),
    
    # Video Game Experience
    MeanVGexp = mean(VGexp, na.rm = TRUE),
    SdVGexp = sd(VGexp, na.rm = TRUE),
    MedianVGexp = median(VGexp, na.rm = TRUE),
    MinVGexp = min(VGexp, na.rm = TRUE),
    MaxVGexp = max(VGexp, na.rm = TRUE)
  )

# Categorical variables: Sex
sex_counts <- df_final %>%
  count(Sex) %>%
  mutate(Percentage = n / sum(n) * 100)

# Categorical variables: Handedness
handedness_counts <- df_final %>%
  count(Handedness) %>%
  mutate(Percentage = n / sum(n) * 100)

################################################################################
## STORE RESULTS
################################################################################

# Create results list
demographics_results <- list(
  n_total = n_total,
  missing_summary = missing_summary,
  summary_stats = summary_stats,
  sex_counts = sex_counts,
  handedness_counts = handedness_counts
)

# Print results
demographics_results