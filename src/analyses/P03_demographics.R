## P03_demographics.R
# Author: Quentin Chenot
# Date: 2025-11-06
# Description: This script extracts and summarizes demographic information from the dataset.
#
# Research Question:
#   What are the demographic characteristics of the sample?
#
# Variables analyzed:
#   - Age (continuous)
#   - Education Level (continuous, in years)
#   - Video Game Experience (continuous, in years)
#   - Sex (categorical: man/woman)
#   - Handedness (categorical: left-handed/right-handed)
#
# Outputs:
#   - Descriptive statistics table printed to console
#   - Formatted text for manuscript
#   - LaTeX table code

################################################################################
## SETUP
################################################################################

# Source utility functions
source("utils.R")

# Load required packages
required_packages <- c("dplyr", "tidyr", "rstudioapi")
load_packages(required_packages)

# Path management
this_file <- rstudioapi::getSourceEditorContext()$path
this_dir <- dirname(this_file)
setwd(this_dir)
project_dir <- dirname(dirname(this_dir))

# Load data (non-z-scored for demographics)
df_final <- load_data(project_dir, z_scored = FALSE)

################################################################################
## SAMPLE SIZE
################################################################################

print_section("SAMPLE INFORMATION")

n_total <- nrow(df_final)
cat(sprintf("Total sample size: %d participants\n", n_total))

# Check for missing data in demographic variables
demo_vars <- c("Age", "EducationLevel", "VGexp", "Sex", "Handedness")
missing_summary <- sapply(demo_vars, function(var) {
  sum(is.na(df_final[[var]]))
})

if (sum(missing_summary) > 0) {
  cat("\nMissing data:\n")
  for (var in names(missing_summary[missing_summary > 0])) {
    cat(sprintf("  %s: %d missing (%.1f%%)\n", 
                var, missing_summary[var], 
                100 * missing_summary[var] / n_total))
  }
} else {
  cat("No missing demographic data\n")
}

################################################################################
## CONTINUOUS VARIABLES
################################################################################

print_section("CONTINUOUS DEMOGRAPHIC VARIABLES")

# Calculate summary statistics
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

# Create formatted table
cat("\nSummary Statistics:\n")
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("%-20s %10s %10s %10s\n", "Variable", "Age", "Education", "VG Exp"))
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("%-20s %10.2f %10.2f %10.2f\n", "Mean", 
            summary_stats$MeanAge, summary_stats$MeanEL, summary_stats$MeanVGexp))
cat(sprintf("%-20s %10.2f %10.2f %10.2f\n", "SD", 
            summary_stats$SdAge, summary_stats$SdEL, summary_stats$SdVGexp))
cat(sprintf("%-20s %10.2f %10.2f %10.2f\n", "Median", 
            summary_stats$MedianAge, summary_stats$MedianEL, summary_stats$MedianVGexp))
cat(sprintf("%-20s %10.2f %10.2f %10.2f\n", "Min", 
            summary_stats$MinAge, summary_stats$MinEL, summary_stats$MinVGexp))
cat(sprintf("%-20s %10.2f %10.2f %10.2f\n", "Max", 
            summary_stats$MaxAge, summary_stats$MaxEL, summary_stats$MaxVGexp))
cat(rep("-", 70), "\n\n", sep = "")

################################################################################
## CATEGORICAL VARIABLES
################################################################################

print_section("CATEGORICAL DEMOGRAPHIC VARIABLES")

# Sex distribution
sex_counts <- df_final %>%
  count(Sex) %>%
  mutate(Percentage = n / sum(n) * 100)

cat("Sex distribution:\n")
cat(rep("-", 70), "\n", sep = "")
for (i in 1:nrow(sex_counts)) {
  cat(sprintf("  %-15s: %3d (%.1f%%)\n", 
              sex_counts$Sex[i], sex_counts$n[i], sex_counts$Percentage[i]))
}

# Handedness distribution
handedness_counts <- df_final %>%
  count(Handedness) %>%
  mutate(Percentage = n / sum(n) * 100)

cat("\nHandedness distribution:\n")
cat(rep("-", 70), "\n", sep = "")
for (i in 1:nrow(handedness_counts)) {
  cat(sprintf("  %-15s: %3d (%.1f%%)\n", 
              handedness_counts$Handedness[i], 
              handedness_counts$n[i], 
              handedness_counts$Percentage[i]))
}
cat("\n")

################################################################################
## FORMATTED OUTPUT FOR MANUSCRIPT
################################################################################

print_section("FORMATTED TEXT FOR MANUSCRIPT")

cat("Copy-paste the following into your manuscript:\n\n")
cat(rep("-", 70), "\n", sep = "")

# Get sex counts
n_men <- sex_counts$n[sex_counts$Sex == "man"]
n_women <- sex_counts$n[sex_counts$Sex == "woman"]
pct_women <- sex_counts$Percentage[sex_counts$Sex == "woman"]

manuscript_text <- sprintf(
  "The sample consisted of %d participants (%d men, %d women; %.1f%% women) aged %d to %d years (M = %.1f, SD = %.1f). Education level ranged from %d to %d years (M = %.1f, SD = %.1f), and video game experience ranged from %.0f to %.0f years (M = %.1f, SD = %.1f). Most participants were right-handed (%d/%d, %.1f%%).",
  n_total, n_men, n_women, pct_women,
  as.integer(summary_stats$MinAge), as.integer(summary_stats$MaxAge),
  summary_stats$MeanAge, summary_stats$SdAge,
  as.integer(summary_stats$MinEL), as.integer(summary_stats$MaxEL),
  summary_stats$MeanEL, summary_stats$SdEL,
  summary_stats$MinVGexp, summary_stats$MaxVGexp,
  summary_stats$MeanVGexp, summary_stats$SdVGexp,
  handedness_counts$n[handedness_counts$Handedness == "right-handed"],
  n_total,
  handedness_counts$Percentage[handedness_counts$Handedness == "right-handed"]
)

cat(strwrap(manuscript_text, width = 70), sep = "\n")
cat("\n", rep("-", 70), "\n\n", sep = "")
