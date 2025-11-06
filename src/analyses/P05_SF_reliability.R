## P05_SF_reliability.R
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
# Outputs:
#   - ICC results printed to console
#   - Formatted text for manuscript
#   - LaTeX table (optional)
#
# Reference:
#   Koo, T. K., & Li, M. Y. (2016). A guideline of selecting and reporting 
#   intraclass correlation coefficients for reliability research. 
#   Journal of Chiropractic Medicine, 15(2), 155-163.

################################################################################
## SETUP
################################################################################

# Source utility functions
source("utils.R")

# Load required packages
required_packages <- c("irr", "dplyr", "rstudioapi")
load_packages(required_packages)

# Path management
this_file <- rstudioapi::getSourceEditorContext()$path
this_dir <- dirname(this_file)
setwd(this_dir)
project_dir <- dirname(dirname(this_dir))

# Load data
df_final <- load_data(project_dir, z_scored = FALSE)

################################################################################
## DATA PREPARATION
################################################################################

print_section("DATA PREPARATION FOR RELIABILITY ANALYSIS")

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

cat("Space Fortress reliability data:\n")
cat(sprintf("  Total participants: %d\n", n_total))
cat(sprintf("  Complete cases: %d (%.1f%%)\n", n_complete, 100 * n_complete / n_total))
cat(sprintf("  Missing data: %d (%.1f%%)\n", n_missing, 100 * n_missing / n_total))
cat(sprintf("  Number of game sessions: %d\n", length(sf_columns)))

# Descriptive statistics for each session
cat("\nDescriptive statistics by game session:\n")
cat(rep("-", 70), "\n", sep = "")
for (i in 1:length(sf_columns)) {
  session_data <- df_icc[, i]
  cat(sprintf("Session %d: M = %6.2f, SD = %6.2f, Min = %6.2f, Max = %6.2f, n = %d\n",
              i,
              mean(session_data, na.rm = TRUE),
              sd(session_data, na.rm = TRUE),
              min(session_data, na.rm = TRUE),
              max(session_data, na.rm = TRUE),
              sum(!is.na(session_data))))
}
cat("\n")

################################################################################
## ICC ANALYSIS: SINGLE MEASUREMENT
################################################################################

print_section("ICC ANALYSIS: SINGLE MEASUREMENT")

cat("Computing ICC(2,1): Two-way random effects, agreement, single measurement\n")
cat("This assesses the reliability of a SINGLE game session.\n\n")

# Compute ICC for single measurement
icc_single <- irr::icc(df_icc, model = "twoway", type = "agreement", unit = "single")

# Print results
cat(rep("-", 70), "\n", sep = "")
cat("ICC(2,1) RESULTS\n")
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("ICC:           %.3f\n", icc_single$value))
cat(sprintf("95%% CI:        [%.3f, %.3f]\n", icc_single$lbound, icc_single$ubound))
cat(sprintf("F-statistic:   F(%.0f, %.0f) = %.3f\n", 
            icc_single$df1, icc_single$df2, icc_single$Fvalue))
cat(sprintf("p-value:       %s\n", format_p_value(icc_single$p.value)))
cat(sprintf("Subjects:      %d\n", icc_single$subjects))
cat(sprintf("Raters:        %d\n", icc_single$raters))

# Interpret reliability
cat("\nInterpretation (Koo & Li, 2016):\n")
icc_val <- icc_single$value
if (icc_val < 0.50) {
  interpretation <- "Poor reliability"
} else if (icc_val < 0.75) {
  interpretation <- "Moderate reliability"
} else if (icc_val < 0.90) {
  interpretation <- "Good reliability"
} else {
  interpretation <- "Excellent reliability"
}
cat(sprintf("  → %s (ICC = %.3f)\n", interpretation, icc_val))
cat(rep("-", 70), "\n\n", sep = "")

################################################################################
## ICC ANALYSIS: AVERAGE MEASUREMENT
################################################################################

print_section("ICC ANALYSIS: AVERAGE MEASUREMENT")

cat("Computing ICC(2,k): Two-way random effects, agreement, average measurement\n")
cat("This assesses the reliability of the AVERAGE across 5 game sessions.\n\n")

# Compute ICC for average measurement
icc_average <- irr::icc(df_icc, model = "twoway", type = "agreement", unit = "average")

# Print results
cat(rep("-", 70), "\n", sep = "")
cat("ICC(2,k) RESULTS (k = 5)\n")
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("ICC:           %.3f\n", icc_average$value))
cat(sprintf("95%% CI:        [%.3f, %.3f]\n", icc_average$lbound, icc_average$ubound))
cat(sprintf("F-statistic:   F(%.0f, %.0f) = %.3f\n", 
            icc_average$df1, icc_average$df2, icc_average$Fvalue))
cat(sprintf("p-value:       %s\n", format_p_value(icc_average$p.value)))
cat(sprintf("Subjects:      %d\n", icc_average$subjects))
cat(sprintf("Raters:        %d\n", icc_average$raters))

# Interpret reliability
cat("\nInterpretation (Koo & Li, 2016):\n")
icc_val_avg <- icc_average$value
if (icc_val_avg < 0.50) {
  interpretation_avg <- "Poor reliability"
} else if (icc_val_avg < 0.75) {
  interpretation_avg <- "Moderate reliability"
} else if (icc_val_avg < 0.90) {
  interpretation_avg <- "Good reliability"
} else {
  interpretation_avg <- "Excellent reliability"
}
cat(sprintf("  → %s (ICC = %.3f)\n", interpretation_avg, icc_val_avg))
cat(rep("-", 70), "\n\n", sep = "")
