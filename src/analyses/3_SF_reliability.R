## 3_SF_reliability.R
# Author: Quentin Chenot
# Date: 2025-11-06
# Description: This script analyzes the test-retest reliability of Space Fortress performance
#              using Intraclass Correlation Coefficient (ICC).
#
# Research Question:
#   Is Space Fortress performance reliable across multiple game sessions?
#
# Methodology:
#   - ICC(2,1): Two-way random effects, agreement, single rater/measurement
#     * Assesses reliability of a single game session
#     * Model: Each participant and each game session are random effects
#   - ICC(2,k): Two-way random effects, agreement, average of k raters
#     * Assesses reliability of the average across 5 game sessions
#     * k = 5 (number of game sessions)
#
# Interpretation (Koo & Li, 2016):
#   - ICC < 0.50: Poor reliability
#   - 0.50 ≤ ICC < 0.75: Moderate reliability
#   - 0.75 ≤ ICC < 0.90: Good reliability
#   - ICC ≥ 0.90: Excellent reliability
#
# Reference:
#   Koo, T. K., & Li, M. Y. (2016). A guideline of selecting and reporting 
#   intraclass correlation coefficients for reliability research. 
#   Journal of Chiropractic Medicine, 15(2), 155-163.
#
# Output:
#   Returns a list with ICC statistics

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
required_packages <- c("irr", "dplyr")
load_packages(required_packages)

# Path management
project_dir <- dirname(dirname(getwd()))

# Load data
df_final <- load_data(project_dir, z_scored = FALSE)

################################################################################
## DATA PREPARATION
################################################################################

# Select Space Fortress multi-game columns
sf_columns <- c("SF_multi_01", "SF_multi_02", "SF_multi_03", "SF_multi_04", "SF_multi_05")

# Check if columns exist
missing_cols <- sf_columns[!sf_columns %in% names(df_final)]
if (length(missing_cols) > 0) {
  stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
}

# Extract ICC data
df_icc <- df_final[, sf_columns]

# Check for missing data
n_complete <- sum(complete.cases(df_icc))
n_total <- nrow(df_icc)
n_missing <- n_total - n_complete

# Descriptive statistics for each session
session_stats <- data.frame(
  session = 1:length(sf_columns),
  mean = sapply(df_icc, mean, na.rm = TRUE),
  sd = sapply(df_icc, sd, na.rm = TRUE),
  min = sapply(df_icc, min, na.rm = TRUE),
  max = sapply(df_icc, max, na.rm = TRUE),
  n = sapply(df_icc, function(x) sum(!is.na(x)))
)

print(session_stats)

################################################################################
## ICC ANALYSIS
################################################################################

# Compute ICC for single measurement
icc_single <- irr::icc(df_icc, model = "twoway", type = "agreement", unit = "single")

# Interpret single measurement reliability
icc_val <- icc_single$value
if (icc_val < 0.50) {
  interpretation_single <- "Poor reliability"
} else if (icc_val < 0.75) {
  interpretation_single <- "Moderate reliability"
} else if (icc_val < 0.90) {
  interpretation_single <- "Good reliability"
} else {
  interpretation_single <- "Excellent reliability"
}

# Compute ICC for average measurement
icc_average <- irr::icc(df_icc, model = "twoway", type = "agreement", unit = "average")

# Interpret average measurement reliability
icc_val_avg <- icc_average$value
if (icc_val_avg < 0.50) {
  interpretation_average <- "Poor reliability"
} else if (icc_val_avg < 0.75) {
  interpretation_average <- "Moderate reliability"
} else if (icc_val_avg < 0.90) {
  interpretation_average <- "Good reliability"
} else {
  interpretation_average <- "Excellent reliability"
}

################################################################################
## STORE RESULTS
################################################################################

# Create results list
reliability_results <- list(
  n_total = n_total,
  n_complete = n_complete,
  n_missing = n_missing,
  n_sessions = length(sf_columns),
  session_stats = session_stats,
  
  # Single measurement ICC
  icc_single_value = icc_single$value,
  icc_single_lbound = icc_single$lbound,
  icc_single_ubound = icc_single$ubound,
  icc_single_fvalue = icc_single$Fvalue,
  icc_single_df1 = icc_single$df1,
  icc_single_df2 = icc_single$df2,
  icc_single_pvalue = icc_single$p.value,
  interpretation_single = interpretation_single,
  
  # Average measurement ICC
  icc_average_value = icc_average$value,
  icc_average_lbound = icc_average$lbound,
  icc_average_ubound = icc_average$ubound,
  icc_average_fvalue = icc_average$Fvalue,
  icc_average_df1 = icc_average$df1,
  icc_average_df2 = icc_average$df2,
  icc_average_pvalue = icc_average$p.value,
  interpretation_average = interpretation_average
)

# Print confirmation
print(reliability_results)