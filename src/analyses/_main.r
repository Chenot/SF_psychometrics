## _main.r
# Author: Quentin Chenot
# Date: 2025-11-17
# Description: Main script to run all analyses.
#              This script executes the complete analysis pipeline including:
#              - Demographics
#              - Space Fortress data distribution
#              - Test-retest reliability (ICC)
#              - Concurrent validity (correlations with Executive Functions)
#              - Relationship with covariates
#              - Regression model
#
# Output:
#   - Console output with formatted tables and statistics
#   - Formatted text ready for manuscript
#   - LaTeX tables for publication
#   - All figures saved to results/figures/
#
# Usage:
#   Run this script to execute all analyses.
#   Results are displayed in console and stored in the workspace.
#
# Note:
#   This script sources 0_all_results.R which runs all individual analysis scripts
#   (1_demographics.R, 2_SF_distribution.R, 3_SF_reliability.R, etc.)

################################################################################
## SETUP
################################################################################

# Set working directory to script location
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  this_file <- rstudioapi::getSourceEditorContext()$path
  this_dir <- dirname(this_file)
  setwd(this_dir)
}

################################################################################
## RUN ALL ANALYSES
################################################################################

# Run global script (try to run it 2 times if error)
source("0_all_results.R")