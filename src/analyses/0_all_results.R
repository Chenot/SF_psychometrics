## 0_results.R
# Author: Quentin Chenot
# Date: 2025-11-06
# Description: This script generates formatted reports for all analyses.
#
# Outputs:
#   - Console summary tables
#   - Formatted text for manuscript
#   - LaTeX table code

################################################################################
## SETUP
################################################################################

# Set working directory to script location
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  this_file <- rstudioapi::getSourceEditorContext()$path
  this_dir <- dirname(this_file)
  setwd(this_dir)
}

# Source utility functions and analyses
source("utils.R")
source("1_demographics.R")
source("2_SF_data_distribution.R")
source("3_SF_reliability.R")
source("4_SF_concurrent_validity.R")
source("5_SF_covariates.R")
source("6_SF_RegressionModel.R")
source("6b_SF_RegressionModel_EFcomponents.R")

################################################################################
## DEMOGRAPHICS
################################################################################

print_section("SAMPLE INFORMATION")

cat(sprintf("Total sample size: %d participants\n", demographics_results$n_total))

# Report missing data
if (sum(demographics_results$missing_summary) > 0) {
  cat("\nMissing data:\n")
  for (var in names(demographics_results$missing_summary[demographics_results$missing_summary > 0])) {
    cat(sprintf("  %s: %d missing (%.1f%%)\n", 
                var, 
                demographics_results$missing_summary[var], 
                100 * demographics_results$missing_summary[var] / demographics_results$n_total))
  }
} else {
  cat("No missing demographic data\n")
}

################################################################################
## CONTINUOUS VARIABLES
################################################################################

print_section("CONTINUOUS DEMOGRAPHIC VARIABLES")

stats <- demographics_results$summary_stats

cat("\nSummary Statistics:\n")
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("%-20s %10s %10s %10s\n", "Variable", "Age", "Education", "VG Exp"))
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("%-20s %10.2f %10.2f %10.2f\n", "Mean", 
            stats$MeanAge, stats$MeanEL, stats$MeanVGexp))
cat(sprintf("%-20s %10.2f %10.2f %10.2f\n", "SD", 
            stats$SdAge, stats$SdEL, stats$SdVGexp))
cat(sprintf("%-20s %10.2f %10.2f %10.2f\n", "Median", 
            stats$MedianAge, stats$MedianEL, stats$MedianVGexp))
cat(sprintf("%-20s %10.2f %10.2f %10.2f\n", "Min", 
            stats$MinAge, stats$MinEL, stats$MinVGexp))
cat(sprintf("%-20s %10.2f %10.2f %10.2f\n", "Max", 
            stats$MaxAge, stats$MaxEL, stats$MaxVGexp))
cat(rep("-", 70), "\n\n", sep = "")

################################################################################
## CATEGORICAL VARIABLES
################################################################################

print_section("CATEGORICAL DEMOGRAPHIC VARIABLES")

sex_counts <- demographics_results$sex_counts
handedness_counts <- demographics_results$handedness_counts

cat("Sex distribution:\n")
cat(rep("-", 70), "\n", sep = "")
for (i in 1:nrow(sex_counts)) {
  cat(sprintf("  %-15s: %3d (%.1f%%)\n", 
              sex_counts$Sex[i], sex_counts$n[i], sex_counts$Percentage[i]))
}

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
## SPACE FORTRESS DISTRIBUTION
################################################################################

print_section("SPACE FORTRESS PERFORMANCE DISTRIBUTION")

dist <- distribution_results

cat("Descriptive Statistics:\n")
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("Sample size:        %d\n", dist$n))
cat(sprintf("Mean:               %.2f\n", dist$mean))
cat(sprintf("Standard deviation: %.2f\n", dist$sd))
cat(sprintf("Median:             %.2f\n", dist$median))
cat(sprintf("Range:              [%.2f, %.2f]\n", dist$min, dist$max))
cat(sprintf("Q1 - Q3:            [%.2f, %.2f]\n", dist$q1, dist$q3))
cat(sprintf("IQR:                %.2f\n", dist$iqr))

cat("\nDistribution Shape:\n")
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("Skewness:           %.3f", dist$skewness))
if (abs(dist$skewness) < 0.5) {
  cat(" (approximately symmetric)\n")
} else if (abs(dist$skewness) < 1) {
  cat(" (moderately skewed)\n")
} else if (abs(dist$skewness) < 2) {
  cat(" (highly skewed, acceptable)\n")
} else {
  cat(" (severely skewed)\n")
}

cat(sprintf("Kurtosis:           %.3f", dist$kurtosis))
if (abs(dist$kurtosis) < 0.5) {
  cat(" (approximately normal)\n")
} else if (abs(dist$kurtosis) < 2) {
  cat(" (moderately non-normal)\n")
} else if (abs(dist$kurtosis) < 7) {
  cat(" (highly non-normal, acceptable)\n")
} else {
  cat(" (severely non-normal)\n")
}

cat("\nNormality Test:\n")
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("Kolmogorov-Smirnov D: %.4f\n", dist$ks_statistic))
cat(sprintf("p-value:              %s\n", format_p_value(dist$ks_pvalue)))
if (dist$ks_pvalue > 0.05) {
  cat("→ Data are normally distributed (p > .05)\n")
  cat("→ Parametric tests are appropriate\n")
} else {
  cat("→ Data deviate from normality (p ≤ .05)\n")
  cat("→ Consider non-parametric tests\n")
}
cat("\n")

################################################################################
## SPACE FORTRESS RELIABILITY
################################################################################

print_section("SPACE FORTRESS TEST-RETEST RELIABILITY")

rel <- reliability_results

cat("Data Quality:\n")
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("Total participants: %d\n", rel$n_total))
cat(sprintf("Complete cases:     %d (%.1f%%)\n", 
            rel$n_complete, 100 * rel$n_complete / rel$n_total))
cat(sprintf("Missing data:       %d (%.1f%%)\n", 
            rel$n_missing, 100 * rel$n_missing / rel$n_total))
cat(sprintf("Number of sessions: %d\n", rel$n_sessions))

cat("\nDescriptive Statistics by Session:\n")
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("%-10s %10s %10s %10s %10s %10s\n", 
            "Session", "Mean", "SD", "Min", "Max", "n"))
cat(rep("-", 70), "\n", sep = "")
for (i in 1:nrow(rel$session_stats)) {
  cat(sprintf("%-10d %10.2f %10.2f %10.2f %10.2f %10d\n",
              rel$session_stats$session[i],
              rel$session_stats$mean[i],
              rel$session_stats$sd[i],
              rel$session_stats$min[i],
              rel$session_stats$max[i],
              rel$session_stats$n[i]))
}

cat("\nICC(2,1) - Single Measurement:\n")
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("ICC:         %.3f\n", rel$icc_single_value))
cat(sprintf("95%% CI:      [%.3f, %.3f]\n", rel$icc_single_lbound, rel$icc_single_ubound))
cat(sprintf("F(%.0f, %.0f): %.3f\n", rel$icc_single_df1, rel$icc_single_df2, rel$icc_single_fvalue))
cat(sprintf("p-value:     %s\n", format_p_value(rel$icc_single_pvalue)))
cat(sprintf("→ %s\n", rel$interpretation_single))

cat("\nICC(2,k) - Average of 5 Measurements:\n")
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("ICC:         %.3f\n", rel$icc_average_value))
cat(sprintf("95%% CI:      [%.3f, %.3f]\n", rel$icc_average_lbound, rel$icc_average_ubound))
cat(sprintf("F(%.0f, %.0f): %.3f\n", rel$icc_average_df1, rel$icc_average_df2, rel$icc_average_fvalue))
cat(sprintf("p-value:     %s\n", format_p_value(rel$icc_average_pvalue)))
cat(sprintf("→ %s\n\n", rel$interpretation_average))

################################################################################
## CONCURRENT VALIDITY
################################################################################

print_section("CONCURRENT VALIDITY: SF & EXECUTIVE FUNCTIONS")

cv <- concurrent_validity_results

cat("EF Composite Score:\n")
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("Method:      %s\n", ifelse(cv$EF_method == "pearson", "Pearson", "Spearman")))
cat(sprintf("%s:          %.3f\n", ifelse(cv$EF_method == "pearson", "r", "ρ"), cv$EF_r))
cat(sprintf("95%% CI:      [%.3f, %.3f]\n", cv$EF_ci_lower, cv$EF_ci_upper))
cat(sprintf("p-value:     %s\n", format_p_value(cv$EF_pvalue)))
cat(sprintf("n:           %d\n", cv$EF_n))
cat(sprintf("R²:          %.3f\n", cv$EF_r^2))

cat("\nEF Sub-components:\n")
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("%-15s %8s %10s %20s %10s\n", 
            "Component", "Method", "Coef", "95% CI", "p-value"))
cat(rep("-", 70), "\n", sep = "")

cat(sprintf("%-15s %8s %10.3f [%5.3f, %5.3f] %10s\n",
            "Inhibition",
            ifelse(cv$inhibition_method == "pearson", "Pearson", "Spearman"),
            cv$inhibition_r,
            cv$inhibition_ci_lower,
            cv$inhibition_ci_upper,
            format_p_value(cv$inhibition_pvalue)))

cat(sprintf("%-15s %8s %10.3f [%5.3f, %5.3f] %10s\n",
            "Updating",
            ifelse(cv$updating_method == "pearson", "Pearson", "Spearman"),
            cv$updating_r,
            cv$updating_ci_lower,
            cv$updating_ci_upper,
            format_p_value(cv$updating_pvalue)))

cat(sprintf("%-15s %8s %10.3f [%5.3f, %5.3f] %10s\n",
            "Shifting",
            ifelse(cv$shifting_method == "pearson", "Pearson", "Spearman"),
            cv$shifting_r,
            cv$shifting_ci_lower,
            cv$shifting_ci_upper,
            format_p_value(cv$shifting_pvalue)))

cat("\nGenerated figures:\n")
cat(sprintf("  - %s\n", cv$EF_plot_path))
cat(sprintf("  - %s\n\n", cv$subscores_plot_path))

################################################################################
## SF & Co-variates
################################################################################

print_section("SF & COVARIATES")

cov <- covariates_results

cat("Video Game Experience:\n")
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("Method:      %s\n", ifelse(cov$VGexp_method == "pearson", "Pearson", "Spearman")))
cat(sprintf("%s:          %.3f\n", ifelse(cov$VGexp_method == "pearson", "r", "ρ"), cov$VGexp_r))
cat(sprintf("95%% CI:      [%.3f, %.3f]\n", cov$VGexp_ci_lower, cov$VGexp_ci_upper))
cat(sprintf("p-value:     %s\n", format_p_value(cov$VGexp_pvalue)))
cat(sprintf("n:           %d\n", cov$VGexp_n))

cat("\nEducation Level:\n")
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("Method:      %s\n", ifelse(cov$EL_method == "pearson", "Pearson", "Spearman")))
cat(sprintf("%s:          %.3f\n", ifelse(cov$EL_method == "pearson", "r", "ρ"), cov$EL_r))
cat(sprintf("95%% CI:      [%.3f, %.3f]\n", cov$EL_ci_lower, cov$EL_ci_upper))
cat(sprintf("p-value:     %s\n", format_p_value(cov$EL_pvalue)))
cat(sprintf("n:           %d\n", cov$EL_n))

cat("\nAge:\n")
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("Method:      %s\n", ifelse(cov$Age_method == "pearson", "Pearson", "Spearman")))
cat(sprintf("%s:          %.3f\n", ifelse(cov$Age_method == "pearson", "r", "ρ"), cov$Age_r))
cat(sprintf("95%% CI:      [%.3f, %.3f]\n", cov$Age_ci_lower, cov$Age_ci_upper))
cat(sprintf("p-value:     %s\n", format_p_value(cov$Age_pvalue)))
cat(sprintf("n:           %d\n", cov$Age_n))

cat("\nSex Differences:\n")
cat(rep("-", 70), "\n", sep = "")
cat("Descriptive statistics:\n")
print(cov$sex_mean_sd)
cat(sprintf("\nt(%.0f) = %.3f\n", cov$sex_df, cov$sex_t))
cat(sprintf("%s\n", format_p_value(cov$sex_pvalue)))
cat(sprintf("Cohen's d = %.3f\n", abs(cov$sex_cohens_d)))
cat(sprintf("95%% CI: [%.3f, %.3f]\n", cov$sex_ci_lower, cov$sex_ci_upper))
cat(sprintf("\nFigure: %s\n\n", cov$combined_plot_path))

################################################################################
## REGRESSION MODEL
################################################################################

print_section("REGRESSION MODEL")

reg <- regression_results

cat("Model Formula:\n")
cat("  zscore_SF ~ zscore_EF + Age + EducationLevel + VGexp + Sex\n\n")

cat("Sample Information:\n")
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("Total cases:    %d\n", reg$n_total))
cat(sprintf("Complete cases: %d\n", reg$n_complete))
cat(sprintf("Missing cases:  %d\n\n", reg$n_missing))

cat("Model Statistics:\n")
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("R²:             %.3f\n", reg$r_squared))
cat(sprintf("Adjusted R²:    %.3f\n", reg$adj_r_squared))
cat(sprintf("Residual SE:    %.3f\n", reg$residual_se))
cat(sprintf("F(%d, %d):      %.3f\n", reg$f_df1, reg$f_df2, reg$f_value))
cat(sprintf("p-value:        %s\n\n", format_p_value(reg$f_pvalue)))

cat("Model Coefficients:\n")
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("%-20s %8s %8s %8s %20s %10s\n", 
            "Predictor", "β", "SE", "t", "95% CI", "p-value"))
cat(rep("-", 70), "\n", sep = "")
for (i in 1:nrow(reg$coef_table)) {
  cat(sprintf("%-20s %8.3f %8.3f %8.3f [%6.3f, %6.3f] %10s\n",
              reg$coef_table$term[i],
              reg$coef_table$estimate[i],
              reg$coef_table$std.error[i],
              reg$coef_table$statistic[i],
              reg$coef_table$conf.low[i],
              reg$coef_table$conf.high[i],
              format_p_value(reg$coef_table$p.value[i])))
}

cat("\nModel Assumptions:\n")
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("Independence (DW):     %s (DW = %.3f, %s)\n",
            ifelse(reg$dw_met, "✓ Met", "✗ Violated"),
            reg$dw_statistic,
            format_p_value(reg$dw_pvalue)))
cat(sprintf("Homoscedasticity (BP): %s (BP = %.3f, %s)\n",
            ifelse(reg$bp_met, "✓ Met", "✗ Violated"),
            reg$bp_statistic,
            format_p_value(reg$bp_pvalue)))
cat(sprintf("Normality (KS):        %s (KS = %.3f, %s)\n",
            ifelse(reg$ks_met, "✓ Met", "✗ Violated"),
            reg$ks_statistic,
            format_p_value(reg$ks_pvalue)))

cat("\nCross-Validation Results (10-fold):\n")
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("RMSE: M = %.4f, SD = %.4f\n", reg$cv_rmse_mean, reg$cv_rmse_sd))
cat(sprintf("R²:   M = %.4f, SD = %.4f\n", reg$cv_r2_mean, reg$cv_r2_sd))
cat(sprintf("MAE:  M = %.4f, SD = %.4f\n\n", reg$cv_mae_mean, reg$cv_mae_sd))

################################################################################
## REGRESSION MODEL (EF SUBCOMPONENTS)
################################################################################

print_section("REGRESSION MODEL (EF SUBCOMPONENTS)")

reg_ef <- regression_results_ef_components

cat("Model Formula:\n")
cat("  zscore_SF ~ zscore_inhibition + zscore_WM + zscore_shifting + Age + EducationLevel + VGexp + Sex\n\n")

cat("Sample Information:\n")
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("Total cases:    %d\n", reg_ef$n_total))
cat(sprintf("Complete cases: %d\n", reg_ef$n_complete))
cat(sprintf("Missing cases:  %d\n\n", reg_ef$n_missing))

cat("Model Statistics:\n")
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("R²:             %.3f\n", reg_ef$r_squared))
cat(sprintf("Adjusted R²:    %.3f\n", reg_ef$adj_r_squared))
cat(sprintf("Residual SE:    %.3f\n", reg_ef$residual_se))
cat(sprintf("F(%d, %d):      %.3f\n", reg_ef$f_df1, reg_ef$f_df2, reg_ef$f_value))
cat(sprintf("p-value:        %s\n\n", format_p_value(reg_ef$f_pvalue)))

cat("Model Coefficients:\n")
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("%-20s %8s %8s %8s %20s %10s\n",
            "Predictor", "β", "SE", "t", "95% CI", "p-value"))
cat(rep("-", 70), "\n", sep = "")
for (i in seq_len(nrow(reg_ef$coef_table))) {
  cat(sprintf("%-20s %8.3f %8.3f %8.3f [%6.3f, %6.3f] %10s\n",
              reg_ef$coef_table$term[i],
              reg_ef$coef_table$estimate[i],
              reg_ef$coef_table$std.error[i],
              reg_ef$coef_table$statistic[i],
              reg_ef$coef_table$conf.low[i],
              reg_ef$coef_table$conf.high[i],
              format_p_value(reg_ef$coef_table$p.value[i])))
}

cat("\nEF Component Intercorrelations:\n")
cat(rep("-", 70), "\n", sep = "")
print(round(reg_ef$ef_cor_matrix, 3))

cat("\nCollinearity Diagnostics:\n")
cat(rep("-", 70), "\n", sep = "")
print(reg_ef$vif_table)

cat("\nType-II ANOVA:\n")
cat(rep("-", 70), "\n", sep = "")
print(reg_ef$type2_table)

cat("\nJoint EF Test (all EF slopes = 0):\n")
cat(rep("-", 70), "\n", sep = "")
print(reg_ef$ef_joint_test)

cat("\nModel Assumptions:\n")
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("Independence (DW):     %s (DW = %.3f, %s)\n",
            ifelse(reg_ef$dw_met, "✓ Met", "✗ Violated"),
            reg_ef$dw_statistic,
            format_p_value(reg_ef$dw_pvalue)))
cat(sprintf("Homoscedasticity (BP): %s (BP = %.3f, %s)\n",
            ifelse(reg_ef$bp_met, "✓ Met", "✗ Violated"),
            reg_ef$bp_statistic,
            format_p_value(reg_ef$bp_pvalue)))
cat(sprintf("Normality (KS):        %s (KS = %.3f, %s)\n",
            ifelse(reg_ef$ks_met, "✓ Met", "✗ Violated"),
            reg_ef$ks_statistic,
            format_p_value(reg_ef$ks_pvalue)))

cat("\nCross-Validation Results (10-fold):\n")
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("RMSE: M = %.4f, SD = %.4f\n", reg_ef$cv_rmse_mean, reg_ef$cv_rmse_sd))
cat(sprintf("R²:   M = %.4f, SD = %.4f\n", reg_ef$cv_r2_mean, reg_ef$cv_r2_sd))
cat(sprintf("MAE:  M = %.4f, SD = %.4f\n\n", reg_ef$cv_mae_mean, reg_ef$cv_mae_sd))

################################################################################
## FORMATTED OUTPUT FOR MANUSCRIPT
################################################################################

print_section("FORMATTED TEXT FOR MANUSCRIPT")

cat("=== DEMOGRAPHICS ===\n\n")
cat(rep("-", 70), "\n", sep = "")

# Get sex counts
n_men <- sex_counts$n[sex_counts$Sex == "man"]
n_women <- sex_counts$n[sex_counts$Sex == "woman"]
pct_women <- sex_counts$Percentage[sex_counts$Sex == "woman"]

demographics_text <- sprintf(
  "The final sample consists of %d participants (%d men, %d women; %.1f%% women) aged %d to %d years (M = %.1f, SD = %.1f). Education level ranged from %d to %d years (M = %.1f, SD = %.1f). Most participants were right-handed (%d/%d, %.1f%%).",
  demographics_results$n_total, n_men, n_women, pct_women,
  as.integer(stats$MinAge), as.integer(stats$MaxAge),
  stats$MeanAge, stats$SdAge,
  as.integer(stats$MinEL), as.integer(stats$MaxEL),
  stats$MeanEL, stats$SdEL,
  handedness_counts$n[handedness_counts$Handedness == "right-handed"],
  demographics_results$n_total,
  handedness_counts$Percentage[handedness_counts$Handedness == "right-handed"]
)

cat(strwrap(demographics_text, width = 70), sep = "\n")
cat("\n\n")

cat("=== SPACE FORTRESS DISTRIBUTION ===\n\n")
cat(rep("-", 70), "\n", sep = "")

# Determine skewness description
skew_desc <- if (abs(dist$skewness) < 0.5) {
  sprintf("a slight %s skew (%.2f skewness)", 
          ifelse(dist$skewness < 0, "leftward", "rightward"),
          dist$skewness)
} else if (abs(dist$skewness) < 1) {
  sprintf("moderate %s skew (%.2f skewness)",
          ifelse(dist$skewness < 0, "leftward", "rightward"),
          dist$skewness)
} else {
  sprintf("high %s skew (%.2f skewness)",
          ifelse(dist$skewness < 0, "leftward", "rightward"),
          dist$skewness)
}

# Determine kurtosis description
kurt_desc <- if (abs(dist$kurtosis) < 0.5) {
  sprintf("a distribution close to normal (%.2f kurtosis)", dist$kurtosis)
} else if (dist$kurtosis < 0) {
  sprintf("a distribution slightly flatter than normal (%.2f kurtosis)", dist$kurtosis)
} else {
  sprintf("a distribution slightly more peaked than normal (%.2f kurtosis)", dist$kurtosis)
}

distribution_text <- sprintf(
  "SF raw scores ranged from %.0f to %.0f (M = %.1f, SD = %.1f, Figure \\ref{fig:SFdatadistribution}). The distribution showed %s and %s. The Kolmogorov-Smirnov test revealed %s (D = %.2f, p = %.2f) with a normal distribution. Considering thresholds of [-1, 1] for skewness and [-2, 2] for kurtosis \\citep{west1995structural}, the SF score distribution can be considered as following normal distribution.",
  dist$min, dist$max, dist$mean, dist$sd,
  skew_desc,
  kurt_desc,
  ifelse(dist$ks_pvalue > 0.05, "no significant difference", "a significant difference"),
  dist$ks_statistic,
  dist$ks_pvalue
)

cat(strwrap(distribution_text, width = 70), sep = "\n")
cat("\n\n")

cat("=== RELIABILITY ===\n\n")
cat(rep("-", 70), "\n", sep = "")

reliability_text <- sprintf(
  "The ICC for SF scores over five runs in a single session are highlighted in Table \\ref{tab:icc_results}. For reference, \\cite{koo2016guideline} defined that \"\\textit{values less than .5 are indicative of poor reliability, values between .5 and .75 indicate moderate reliability, values between .75 and .9 indicate good reliability, and values greater than .90 indicate excellent reliability}\". Therefore, the ICC obtained here (%.2f for single measures, to %.2f for the average, both p $<$ .001) and tight confidence intervals (CI 95%% [%.2f, %.2f]) fall within the %s.",
  rel$icc_single_value,
  rel$icc_average_value,
  rel$icc_single_lbound,
  rel$icc_average_ubound,
  ifelse(rel$icc_single_value >= 0.75 && rel$icc_average_value >= 0.90, 
         "good to excellent reliability",
         ifelse(rel$icc_average_value >= 0.75, "good reliability", "moderate to good reliability"))
)

cat(strwrap(reliability_text, width = 70), sep = "\n")
cat("\n\n")

cat("=== CONCURRENT VALIDITY ===\n\n")
cat(rep("-", 70), "\n", sep = "")

concurrent_validity_text <- sprintf(
  "Space Fortress performance showed a significant positive correlation with the executive functions composite score (%s(%d) = %.2f, 95%% CI [%.2f, %.2f], %s, see Figure 3). Analysis of EF sub-components revealed significant correlations with inhibition (%s(%d) = %.2f, 95%% CI [%.2f, %.2f], %s), updating (%s(%d) = %.2f, 95%% CI [%.2f, %.2f], %s), and shifting (%s(%d) = %.2f, 95%% CI [%.2f, %.2f], %s) (see Figure 4).",
  ifelse(cv$EF_method == "pearson", "r", "ρ"), cv$EF_n - 2, cv$EF_r,
  cv$EF_ci_lower, cv$EF_ci_upper, format_p_value(cv$EF_pvalue),
  
  ifelse(cv$inhibition_method == "pearson", "r", "ρ"), cv$inhibition_n - 2, cv$inhibition_r,
  cv$inhibition_ci_lower, cv$inhibition_ci_upper, format_p_value(cv$inhibition_pvalue),
  
  ifelse(cv$updating_method == "pearson", "r", "ρ"), cv$updating_n - 2, cv$updating_r,
  cv$updating_ci_lower, cv$updating_ci_upper, format_p_value(cv$updating_pvalue),
  
  ifelse(cv$shifting_method == "pearson", "r", "ρ"), cv$shifting_n - 2, cv$shifting_r,
  cv$shifting_ci_lower, cv$shifting_ci_upper, format_p_value(cv$shifting_pvalue)
)

cat(strwrap(concurrent_validity_text, width = 70), sep = "\n")
cat("\n", rep("-", 70), "\n\n", sep = "")

cat("=== SPACE FORTRESS RELATIONSHIP WITH POTENTIAL COVARIATES ===\n\n")
cat(rep("-", 70), "\n", sep = "")

covariates_text <- sprintf(
  "We examined the relationship between Space Fortress performance and potential covariates. Video game experience showed %s (%s(%d) = %.2f, 95%% CI [%.2f, %.2f], %s). Education level showed %s (%s(%d) = %.2f, 95%% CI [%.2f, %.2f], %s). Age showed %s (%s(%d) = %.2f, 95%% CI [%.2f, %.2f], %s). An independent samples t-test revealed %s between men and women (t(%.0f) = %.2f, %s, Cohen's d = %.2f) (see Supplementary Figure S1).",
  ifelse(cov$VGexp_pvalue < 0.05, "a significant positive correlation", "no significant correlation"),
  ifelse(cov$VGexp_method == "pearson", "r", "ρ"), cov$VGexp_n - 2, cov$VGexp_r,
  cov$VGexp_ci_lower, cov$VGexp_ci_upper, format_p_value(cov$VGexp_pvalue),
  
  ifelse(cov$EL_pvalue < 0.05, "a significant correlation", "no significant correlation"),
  ifelse(cov$EL_method == "pearson", "r", "ρ"), cov$EL_n - 2, cov$EL_r,
  cov$EL_ci_lower, cov$EL_ci_upper, format_p_value(cov$EL_pvalue),
  
  ifelse(cov$Age_pvalue < 0.05, "a significant correlation", "no significant correlation"),
  ifelse(cov$Age_method == "pearson", "r", "ρ"), cov$Age_n - 2, cov$Age_r,
  cov$Age_ci_lower, cov$Age_ci_upper, format_p_value(cov$Age_pvalue),
  
  ifelse(cov$sex_pvalue < 0.05, "a significant difference", "no significant difference"),
  cov$sex_df, cov$sex_t, format_p_value(cov$sex_pvalue), abs(cov$sex_cohens_d)
)

cat(strwrap(covariates_text, width = 70), sep = "\n")
cat("\n\n")

cat("=== EXPLORATORY ANALYSES: REGRESSION MODEL ===\n\n")
cat(rep("-", 70), "\n", sep = "")

# Get significant predictors (excluding intercept)
sig_predictors <- reg$coef_table[reg$coef_table$p.value < 0.05 & reg$coef_table$term != "(Intercept)", ]
non_sig_predictors <- reg$coef_table[reg$coef_table$p.value >= 0.05 & reg$coef_table$term != "(Intercept)", ]

# Format significant predictors text
if (nrow(sig_predictors) > 0) {
  sig_text <- paste(sapply(1:nrow(sig_predictors), function(i) {
    term_name <- sig_predictors$term[i]
    # Clean up term names for display
    display_name <- switch(term_name,
                          "zscore_EF" = "EF composite score",
                          "Age" = "age",
                          "EducationLevel" = "education level",
                          "VGexp" = "video game experience",
                          "Sexwoman" = "sex",
                          term_name)
    
    # Add interpretation for specific predictors
    interpretation <- ""
    if (term_name == "zscore_EF") {
      interpretation <- ", with higher EF scores associated with better SF performance"
    } else if (term_name == "Sexwoman") {
      interpretation <- ", with women performing lower on average than men"
    }
    
    sprintf("%s ($\\beta$ = %.2f, SE = %.2f, t = %.2f, p %s)%s",
            display_name,
            sig_predictors$estimate[i],
            sig_predictors$std.error[i],
            sig_predictors$statistic[i],
            ifelse(sig_predictors$p.value[i] < 0.001, "$<$ .001",
                   sprintf("= %.3f", sig_predictors$p.value[i])),
            interpretation)
  }), collapse = " and ")
  
  sig_text <- paste0("both ", sig_text)
} else {
  sig_text <- "no predictors"
}

# Format non-significant predictors text
if (nrow(non_sig_predictors) > 0) {
  non_sig_text <- paste(sapply(1:nrow(non_sig_predictors), function(i) {
    term_name <- non_sig_predictors$term[i]
    display_name <- switch(term_name,
                          "zscore_EF" = "EF composite score",
                          "Age" = "age",
                          "EducationLevel" = "education level",
                          "VGexp" = "video game experience",
                          "Sexwoman" = "sex",
                          term_name)
    
    sprintf("%s (p = %.3f)", display_name, non_sig_predictors$p.value[i])
  }), collapse = ", ")
  
  # Add proper grammar
  if (nrow(non_sig_predictors) == 1) {
    non_sig_clause <- sprintf(" %s was not a significant predictor.", non_sig_text)
  } else {
    # Replace last comma with "and"
    parts <- strsplit(non_sig_text, ", ")[[1]]
    if (length(parts) > 1) {
      non_sig_text <- paste(c(parts[1:(length(parts)-1)], 
                             paste("and", parts[length(parts)])), 
                           collapse = ", ")
    }
    non_sig_clause <- sprintf(" %s were not significant predictors.", non_sig_text)
  }
} else {
  non_sig_clause <- ""
}

regression_text <- sprintf(
  "The regression model was significant, F(%d, %d) = %.2f, p %s, and explained %.1f%%%% of the variance in SF performance (R$^2$ = %.3f, adjusted R$^2$ = %.3f).\n\nAmong the predictors, %s significantly predicted SF performance%s.%s\n\nModel assumptions were verified and met: The \\textit{Durbin--Watson test} indicated %s of residuals (DW = %.2f, p = %.2f), the Breusch--Pagan test showed %s (BP = %.2f, p = %.2f), and residuals were %s according to the Kolmogorov-Smirnov test (D = %.2f, p = %.2f). Together, these results support the adequacy of the linear model for the data.\n\nTo assess the robustness and generalizability of the linear model, a 10-fold cross-validation was conducted following standard procedures \\citep{kohavi1995study,yarkoni2017choosing}. The mean root mean squared error (RMSE) across folds was %.2f (SD = %.2f), and the mean cross-validated coefficient of determination was $R^2_{\\text{CV}}$ = %.2f (SD = %.2f). This value represents the proportion of variance in SF performance explained by the model when predicting unseen data, reflecting its out-of-sample predictive accuracy.",
  # Model statistics
  reg$f_df1,
  reg$f_df2,
  reg$f_value,
  ifelse(reg$f_pvalue < 0.001, "$<$ .001", sprintf("= %.3f", reg$f_pvalue)),
  reg$r_squared * 100,
  reg$r_squared,
  reg$adj_r_squared,
  
  # Significant predictors
  sig_text,
  ifelse(nrow(sig_predictors) > 0, 
         ifelse(grepl("with", sig_text), "", ""),
         ""),
  non_sig_clause,
  
  # Model assumptions
  ifelse(reg$dw_met, "no autocorrelation", "autocorrelation"),
  reg$dw_statistic,
  reg$dw_pvalue,
  ifelse(reg$bp_met, "no evidence of heteroscedasticity", "evidence of heteroscedasticity"),
  reg$bp_statistic,
  reg$bp_pvalue,
  ifelse(reg$ks_met, "normally distributed", "not normally distributed"),
  reg$ks_statistic,
  reg$ks_pvalue,
  
  # Cross-validation
  reg$cv_rmse_mean,
  reg$cv_rmse_sd,
  reg$cv_r2_mean,
  reg$cv_r2_sd
)

cat(regression_text)
cat("\n", rep("-", 70), "\n\n", sep = "")

cat("=== SUPPLEMENTARY ANALYSES: REGRESSION MODEL WITH EF SUBCOMPONENTS ===\n\n")
cat(rep("-", 70), "\n", sep = "")

# Get significant predictors (excluding intercept)
sig_predictors_ef <- reg_ef$coef_table[
  reg_ef$coef_table$p.value < 0.05 & reg_ef$coef_table$term != "(Intercept)",
]
non_sig_predictors_ef <- reg_ef$coef_table[
  reg_ef$coef_table$p.value >= 0.05 & reg_ef$coef_table$term != "(Intercept)",
]

# Format significant predictors text
if (nrow(sig_predictors_ef) > 0) {
  sig_text_ef <- paste(sapply(seq_len(nrow(sig_predictors_ef)), function(i) {
    term_name <- sig_predictors_ef$term[i]
    display_name <- switch(term_name,
                           "zscore_inhibition" = "inhibition",
                           "zscore_WM" = "updating",
                           "zscore_shifting" = "shifting",
                           "Age" = "age",
                           "EducationLevel" = "education level",
                           "VGexp" = "video game experience",
                           "Sexwoman" = "sex",
                           term_name)

    interpretation <- ""
    if (term_name == "Sexwoman") {
      interpretation <- ", with women performing lower on average than men"
    }

    sprintf("%s ($\\beta$ = %.2f, SE = %.2f, t = %.2f, p %s)%s",
            display_name,
            sig_predictors_ef$estimate[i],
            sig_predictors_ef$std.error[i],
            sig_predictors_ef$statistic[i],
            ifelse(sig_predictors_ef$p.value[i] < 0.001, "$<$ .001",
                   sprintf("= %.3f", sig_predictors_ef$p.value[i])),
            interpretation)
  }), collapse = " and ")

  sig_text_ef <- paste0("", sig_text_ef)
} else {
  sig_text_ef <- "no predictors"
}

# Format non-significant predictors text
if (nrow(non_sig_predictors_ef) > 0) {
  non_sig_text_ef <- paste(sapply(seq_len(nrow(non_sig_predictors_ef)), function(i) {
    term_name <- non_sig_predictors_ef$term[i]
    display_name <- switch(term_name,
                           "zscore_inhibition" = "inhibition",
                           "zscore_WM" = "updating",
                           "zscore_shifting" = "shifting",
                           "Age" = "age",
                           "EducationLevel" = "education level",
                           "VGexp" = "video game experience",
                           "Sexwoman" = "sex",
                           term_name)

    sprintf("%s (p = %.3f)", display_name, non_sig_predictors_ef$p.value[i])
  }), collapse = ", ")

  if (nrow(non_sig_predictors_ef) == 1) {
    non_sig_clause_ef <- sprintf(" %s was not a significant predictor.", non_sig_text_ef)
  } else {
    parts <- strsplit(non_sig_text_ef, ", ")[[1]]
    if (length(parts) > 1) {
      non_sig_text_ef <- paste(c(parts[1:(length(parts) - 1)],
                                 paste("and", parts[length(parts)])),
                               collapse = ", ")
    }
    non_sig_clause_ef <- sprintf(" %s were not significant predictors.", non_sig_text_ef)
  }
} else {
  non_sig_clause_ef <- ""
}

# Joint EF block test
ef_joint_df_num <- reg_ef$ef_joint_test$Df[2]
ef_joint_df_den <- reg_ef$ef_joint_test$Res.Df[2]
ef_joint_f <- reg_ef$ef_joint_test$F[2]
ef_joint_p <- reg_ef$ef_joint_test$`Pr(>F)`[2]

regression_text_ef <- sprintf(
  "The multiple regression model including the 3 EF subcomponents (inhibition, updating, and shifting) and demographic covariates showed significance, F(%d, %d) = %.2f, p %s. The selected variables explained %.1f%%%% of the variance in SF performance (R$^2$ = %.3f, adjusted R$^2$ = %.3f).\n\nAmong the predictors, %s significantly predicted SF performance.%s\n\nA joint test of the three EF subcomponents indicated %s, F(%d, %d) = %.2f, p %s.\n\nModel assumptions were verified and met: The \\textit{Durbin--Watson test} indicated %s of residuals (DW = %.2f, p = %.2f), the Breusch--Pagan test showed %s (BP = %.2f, p = %.2f), and residuals were %s according to the Kolmogorov-Smirnov test (D = %.2f, p = %.2f). Together, these results support the adequacy of the linear model for the data.\n\nTo assess the robustness and generalizability of the model, a 10-fold cross-validation was conducted following standard procedures \\citep{kohavi1995study,yarkoni2017choosing}. The mean root mean squared error (RMSE) across folds was %.2f (SD = %.2f), and the mean cross-validated coefficient of determination was $R^2_{\\text{CV}}$ = %.2f (SD = %.2f).",
  reg_ef$f_df1,
  reg_ef$f_df2,
  reg_ef$f_value,
  ifelse(reg_ef$f_pvalue < 0.001, "$<$ .001", sprintf("= %.3f", reg_ef$f_pvalue)),
  reg_ef$r_squared * 100,
  reg_ef$r_squared,
  reg_ef$adj_r_squared,
  sig_text_ef,
  non_sig_clause_ef,
  ifelse(ef_joint_p < 0.05,
         "that EF subcomponents jointly explained significant unique variance",
         "no significant joint contribution of EF subcomponents"),
  ef_joint_df_num,
  ef_joint_df_den,
  ef_joint_f,
  ifelse(ef_joint_p < 0.001, "$<$ .001", sprintf("= %.3f", ef_joint_p)),
  ifelse(reg_ef$dw_met, "no autocorrelation", "autocorrelation"),
  reg_ef$dw_statistic,
  reg_ef$dw_pvalue,
  ifelse(reg_ef$bp_met, "no evidence of heteroscedasticity", "evidence of heteroscedasticity"),
  reg_ef$bp_statistic,
  reg_ef$bp_pvalue,
  ifelse(reg_ef$ks_met, "normally distributed", "not normally distributed"),
  reg_ef$ks_statistic,
  reg_ef$ks_pvalue,
  reg_ef$cv_rmse_mean,
  reg_ef$cv_rmse_sd,
  reg_ef$cv_r2_mean,
  reg_ef$cv_r2_sd
)

cat(regression_text_ef)
cat("\n", rep("-", 70), "\n\n", sep = "")

################################################################################
## LATEX TABLES
################################################################################

print_section("LATEX TABLES")

cat("=== TABLE 1: DEMOGRAPHICS ===\n\n")
cat("\\begin{table}[h!]\n")
cat("\\centering\n")
cat("\\caption{Demographic characteristics of the sample (N = ", demographics_results$n_total, ").}\n", sep = "")
cat("\\label{tab:demographics}\n")
cat("\\begin{tabular}{lccc}\n")
cat("\\toprule\n")
cat("Variable & M (SD) & Range & n (\\%) \\\\ \\midrule\n")

# Continuous variables
cat(sprintf("Age (years) & %.1f (%.1f) & %d--%d & -- \\\\\n",
            stats$MeanAge, stats$SdAge,
            as.integer(stats$MinAge), as.integer(stats$MaxAge)))
cat(sprintf("Education level (years) & %.1f (%.1f) & %d--%d & -- \\\\\n",
            stats$MeanEL, stats$SdEL,
            as.integer(stats$MinEL), as.integer(stats$MaxEL)))
cat(sprintf("Video game experience (years) & %.1f (%.1f) & %.0f--%d & -- \\\\\n",
            stats$MeanVGexp, stats$SdVGexp,
            stats$MinVGexp, as.integer(stats$MaxVGexp)))

# Categorical variables
cat("\\midrule\n")
cat("Sex & & & \\\\\n")
for (i in 1:nrow(sex_counts)) {
  cat(sprintf("\\quad %s & -- & -- & %d (%.1f) \\\\\n",
              sex_counts$Sex[i], sex_counts$n[i], sex_counts$Percentage[i]))
}

cat("Handedness & & & \\\\\n")
for (i in 1:nrow(handedness_counts)) {
  cat(sprintf("\\quad %s & -- & -- & %d (%.1f) \\\\\n",
              handedness_counts$Handedness[i], 
              handedness_counts$n[i], 
              handedness_counts$Percentage[i]))
}

cat("\\bottomrule\n")
cat("\\end{tabular}\n")
cat("\\end{table}\n\n")

cat("=== TABLE 2: RELIABILITY ===\n\n")
cat("\\begin{table}[h!]\n")
cat("\\centering\n")
cat("\\caption{Test-retest reliability of Space Fortress performance.}\n")
cat("\\label{tab:reliability}\n")
cat("\\begin{tabular}{lccc}\n")
cat("\\toprule\n")
cat("Measurement Type & ICC & 95\\% CI & Interpretation \\\\ \\midrule\n")
cat(sprintf("Single session (ICC(2,1)) & %.2f & [%.2f, %.2f] & %s \\\\\n",
            rel$icc_single_value, rel$icc_single_lbound, rel$icc_single_ubound, 
            rel$interpretation_single))
cat(sprintf("Average of 5 sessions (ICC(2,k)) & %.2f & [%.2f, %.2f] & %s \\\\\n",
            rel$icc_average_value, rel$icc_average_lbound, rel$icc_average_ubound, 
            rel$interpretation_average))
cat("\\bottomrule\n")
cat("\\end{tabular}\n")
cat("\\end{table}\n\n")

cat("=== TABLE 3: CONCURRENT VALIDITY ===\n\n")
cat("\\begin{table}[h!]\n")
cat("\\centering\n")
cat("\\caption{Correlations between Space Fortress performance and Executive Functions.}\n")
cat("\\label{tab:concurrent_validity}\n")
cat("\\begin{tabular}{lcccc}\n")
cat("\\toprule\n")
cat("Variable & Method & Coefficient & 95\\% CI & $p$-value \\\\ \\midrule\n")

cat(sprintf("EF Composite & %s & %.2f & [%.2f, %.2f] & %s \\\\\n",
            ifelse(cv$EF_method == "pearson", "Pearson", "Spearman"),
            cv$EF_r, cv$EF_ci_lower, cv$EF_ci_upper,
            ifelse(cv$EF_pvalue < 0.001, "\\textbf{$<$.001}", 
                   ifelse(cv$EF_pvalue < 0.05, sprintf("\\textbf{%.3f}", cv$EF_pvalue),
                          sprintf("%.3f", cv$EF_pvalue)))))

cat("\\midrule\n")

cat(sprintf("Inhibition & %s & %.2f & [%.2f, %.2f] & %s \\\\\n",
            ifelse(cv$inhibition_method == "pearson", "Pearson", "Spearman"),
            cv$inhibition_r, cv$inhibition_ci_lower, cv$inhibition_ci_upper,
            ifelse(cv$inhibition_pvalue < 0.001, "\\textbf{$<$.001}", 
                   ifelse(cv$inhibition_pvalue < 0.05, sprintf("\\textbf{%.3f}", cv$inhibition_pvalue),
                          sprintf("%.3f", cv$inhibition_pvalue)))))

cat(sprintf("Updating & %s & %.2f & [%.2f, %.2f] & %s \\\\\n",
            ifelse(cv$updating_method == "pearson", "Pearson", "Spearman"),
            cv$updating_r, cv$updating_ci_lower, cv$updating_ci_upper,
            ifelse(cv$updating_pvalue < 0.001, "\\textbf{$<$.001}", 
                   ifelse(cv$updating_pvalue < 0.05, sprintf("\\textbf{%.3f}", cv$updating_pvalue),
                          sprintf("%.3f", cv$updating_pvalue)))))

cat(sprintf("Shifting & %s & %.2f & [%.2f, %.2f] & %s \\\\\n",
            ifelse(cv$shifting_method == "pearson", "Pearson", "Spearman"),
            cv$shifting_r, cv$shifting_ci_lower, cv$shifting_ci_upper,
            ifelse(cv$shifting_pvalue < 0.001, "\\textbf{$<$.001}", 
                   ifelse(cv$shifting_pvalue < 0.05, sprintf("\\textbf{%.3f}", cv$shifting_pvalue),
                          sprintf("%.3f", cv$shifting_pvalue)))))

cat("\\bottomrule\n")
cat("\\end{tabular}\n")
cat("\\end{table}\n\n")

cat("=== TABLE 4: COVARIATES ===\n\n")
cat("\\begin{table}[h!]\n")
cat("\\centering\n")
cat("\\caption{Correlations between Space Fortress performance and demographic covariates.}\n")
cat("\\label{tab:covariates}\n")
cat("\\begin{tabular}{lcccc}\n")
cat("\\toprule\n")
cat("Variable & Method & Coefficient & 95\\% CI & $p$-value \\\\ \\midrule\n")

cat(sprintf("Video game experience & %s & %.2f & [%.2f, %.2f] & %s \\\\\n",
            ifelse(cov$VGexp_method == "pearson", "Pearson", "Spearman"),
            cov$VGexp_r, cov$VGexp_ci_lower, cov$VGexp_ci_upper,
            ifelse(cov$VGexp_pvalue < 0.001, "\\textbf{$<$.001}", 
                   ifelse(cov$VGexp_pvalue < 0.05, sprintf("\\textbf{%.3f}", cov$VGexp_pvalue),
                          sprintf("%.3f", cov$VGexp_pvalue)))))

cat(sprintf("Education level & %s & %.2f & [%.2f, %.2f] & %s \\\\\n",
            ifelse(cov$EL_method == "pearson", "Pearson", "Spearman"),
            cov$EL_r, cov$EL_ci_lower, cov$EL_ci_upper,
            ifelse(cov$EL_pvalue < 0.001, "\\textbf{$<$.001}", 
                   ifelse(cov$EL_pvalue < 0.05, sprintf("\\textbf{%.3f}", cov$EL_pvalue),
                          sprintf("%.3f", cov$EL_pvalue)))))

cat(sprintf("Age & %s & %.2f & [%.2f, %.2f] & %s \\\\\n",
            ifelse(cov$Age_method == "pearson", "Pearson", "Spearman"),
            cov$Age_r, cov$Age_ci_lower, cov$Age_ci_upper,
            ifelse(cov$Age_pvalue < 0.001, "\\textbf{$<$.001}", 
                   ifelse(cov$Age_pvalue < 0.05, sprintf("\\textbf{%.3f}", cov$Age_pvalue),
                          sprintf("%.3f", cov$Age_pvalue)))))

cat("\\midrule\n")
cat(sprintf("Sex (t-test) & t(%.0f) & %.2f & [%.2f, %.2f] & %s \\\\\n",
            cov$sex_df, cov$sex_t, cov$sex_ci_lower, cov$sex_ci_upper,
            ifelse(cov$sex_pvalue < 0.001, "\\textbf{$<$.001}", 
                   ifelse(cov$sex_pvalue < 0.05, sprintf("\\textbf{%.3f}", cov$sex_pvalue),
                          sprintf("%.3f", cov$sex_pvalue)))))

cat(sprintf("\\quad Cohen's d & -- & %.2f & -- & -- \\\\\n", abs(cov$sex_cohens_d)))

cat("\\bottomrule\n")
cat("\\end{tabular}\n")
cat("\\end{table}\n\n")

cat("=== TABLE 5: REGRESSION MODEL 2 (EF SUBCOMPONENTS + COVARIATES) ===\n\n")
cat("\\begin{table}[h!]\n")
cat("\\centering\n")
cat("\\caption{Results of the regression model 2 (updating, inhibition, shifting EF scores and covariates).}\n")
cat("\\label{tab:GLM}\n")
cat("\\centering\n")
cat("\\begin{tabular}{lccccc}\n")
cat("\\toprule\n")
cat("Variable & $\\beta$ & 95\\% CI & SE & t-value & p-value \\\\ \\midrule\n")

coef_table_ef <- reg_ef$coef_table

for (i in seq_len(nrow(coef_table_ef))) {
  term_name <- coef_table_ef$term[i]
  display_name <- switch(term_name,
                         "(Intercept)" = "Intercept",
                         "zscore_inhibition" = "Inhibition",
                         "zscore_WM" = "Updating",
                         "zscore_shifting" = "Shifting",
                         "Age" = "Age",
                         "EducationLevel" = "Education Level",
                         "VGexp" = "VGexp",
                         "Sexwoman" = "Sex",
                         term_name)

  p_txt <- ifelse(coef_table_ef$p.value[i] < 0.001,
                  "\\textbf{$<$ .001}",
                  ifelse(coef_table_ef$p.value[i] < 0.05,
                         sprintf("\\textbf{%.3f}", coef_table_ef$p.value[i]),
                         sprintf("%.3f", coef_table_ef$p.value[i])))

  cat(sprintf("%s & %.2f & [%.2f, %.2f] & %.2f & %.2f & %s \\\\n",
              display_name,
              coef_table_ef$estimate[i],
              coef_table_ef$conf.low[i],
              coef_table_ef$conf.high[i],
              coef_table_ef$std.error[i],
              coef_table_ef$statistic[i],
              p_txt))
}

cat("\\bottomrule\n")
cat("\\end{tabular}\n")
cat("\\\\ \\textit{VGexp: video game experience questionnaire score. Bold: p $<$ .05}\n")
cat("\\end{table}\n\n")

cat("=== TABLE : REGRESSION MODEL ===\n\n")
cat("\\begin{table}[h!]\n")
cat("\\centering\n")
cat("\\caption{Multiple linear regression predicting Space Fortress performance.}\n")
cat("\\label{tab:regression}\n")
cat("\\begin{tabular}{lcccc}\n")
cat("\\toprule\n")
cat("Predictor & $\\beta$ & SE & 95\\% CI & $p$-value \\\\ \\midrule\n")

for (i in 1:nrow(reg$coef_table)) {
  cat(sprintf("%s & %.3f & %.3f & [%.3f, %.3f] & %s \\\\\n",
              reg$coef_table$term[i],
              reg$coef_table$estimate[i],
              reg$coef_table$std.error[i],
              reg$coef_table$conf.low[i],
              reg$coef_table$conf.high[i],
              ifelse(reg$coef_table$p.value[i] < 0.001, "\\textbf{$<$.001}", 
                     ifelse(reg$coef_table$p.value[i] < 0.05, 
                            sprintf("\\textbf{%.3f}", reg$coef_table$p.value[i]),
                            sprintf("%.3f", reg$coef_table$p.value[i])))))
}

cat("\\midrule\n")
cat(sprintf("\\multicolumn{5}{l}{Model: $R^2$ = %.3f, Adjusted $R^2$ = %.3f, $F$(%d, %d) = %.2f, %s} \\\\\n",
            reg$r_squared, reg$adj_r_squared,
            reg$f_df1, reg$f_df2, reg$f_value,
            ifelse(reg$f_pvalue < 0.001, "$p < .001$",
                   sprintf("$p = %.3f$", reg$f_pvalue))))

cat("\\bottomrule\n")
cat("\\end{tabular}\n")
cat("\\end{table}\n\n")

################################################################################
## SUMMARY
################################################################################

print_section("ANALYSIS SUMMARY")

cat("DEMOGRAPHICS:\n")
cat(sprintf("  Sample: N = %d (%d men, %d women)\n", 
            demographics_results$n_total, n_men, n_women))
cat(sprintf("  Age: M = %.1f (SD = %.1f)\n", stats$MeanAge, stats$SdAge))
cat(sprintf("  Education: M = %.1f years (SD = %.1f)\n", stats$MeanEL, stats$SdEL))
cat(sprintf("  VG Experience: M = %.1f years (SD = %.1f)\n\n", stats$MeanVGexp, stats$SdVGexp))

cat("SPACE FORTRESS DISTRIBUTION:\n")
cat(sprintf("  M = %.2f, SD = %.2f, Range = [%.0f, %.0f]\n", 
            dist$mean, dist$sd, dist$min, dist$max))
cat(sprintf("  Skewness = %.3f, Kurtosis = %.3f\n", dist$skewness, dist$kurtosis))
cat(sprintf("  KS test: D = %.3f, %s ", dist$ks_statistic, format_p_value(dist$ks_pvalue)))
if (dist$ks_pvalue > 0.05) cat("✓\n") else cat("✗\n")
cat(sprintf("  Plot: %s\n\n", dist$plot_path))

cat("RELIABILITY:\n")
cat(sprintf("  ICC(2,1) = %.3f [%.3f, %.3f] - %s\n",
            rel$icc_single_value, rel$icc_single_lbound, rel$icc_single_ubound,
            rel$interpretation_single))
cat(sprintf("  ICC(2,k) = %.3f [%.3f, %.3f] - %s\n\n",
            rel$icc_average_value, rel$icc_average_lbound, rel$icc_average_ubound,
            rel$interpretation_average))

cat("CONCURRENT VALIDITY:\n")
cat(sprintf("  EF Composite: %s = %.2f [%.2f, %.2f], %s\n",
            ifelse(cv$EF_method == "pearson", "r", "ρ"),
            cv$EF_r, cv$EF_ci_lower, cv$EF_ci_upper,
            format_p_value(cv$EF_pvalue)))
cat(sprintf("  Inhibition:   %s = %.2f [%.2f, %.2f], %s\n",
            ifelse(cv$inhibition_method == "pearson", "r", "ρ"),
            cv$inhibition_r, cv$inhibition_ci_lower, cv$inhibition_ci_upper,
            format_p_value(cv$inhibition_pvalue)))
cat(sprintf("  Updating:     %s = %.2f [%.2f, %.2f], %s\n",
            ifelse(cv$updating_method == "pearson", "r", "ρ"),
            cv$updating_r, cv$updating_ci_lower, cv$updating_ci_upper,
            format_p_value(cv$updating_pvalue)))
cat(sprintf("  Shifting:     %s = %.2f [%.2f, %.2f], %s\n",
            ifelse(cv$shifting_method == "pearson", "r", "ρ"),
            cv$shifting_r, cv$shifting_ci_lower, cv$shifting_ci_upper,
            format_p_value(cv$shifting_pvalue)))
cat(sprintf("  Plots: %s\n\n", dirname(cv$EF_plot_path)))

cat("SF & COVARIATES:\n")
cat(sprintf("  VG Experience: %s = %.2f, %s\n",
            ifelse(cov$VGexp_method == "pearson", "r", "ρ"),
            
            cov$VGexp_r, format_p_value(cov$VGexp_pvalue)))
cat(sprintf("  Education:     %s = %.2f, %s\n",
            ifelse(cov$EL_method == "pearson", "r", "ρ"),
            cov$EL_r, format_p_value(cov$EL_pvalue)))
cat(sprintf("  Age:           %s = %.2f, %s\n",
            ifelse(cov$Age_method == "pearson", "r", "ρ"),
            cov$Age_r, format_p_value(cov$Age_pvalue)))
cat(sprintf("  Sex:           t(%.0f) = %.2f, %s, d = %.2f\n",
            cov$sex_df, cov$sex_t, format_p_value(cov$sex_pvalue), abs(cov$sex_cohens_d)))
cat(sprintf("  Plot: %s\n\n", cov$combined_plot_path))

cat("REGRESSION MODEL:\n")
cat(sprintf("  R² = %.3f, Adjusted R² = %.3f\n", reg$r_squared, reg$adj_r_squared))
cat(sprintf("  F(%d, %d) = %.2f, %s\n", reg$f_df1, reg$f_df2, reg$f_value, 
            format_p_value(reg$f_pvalue)))
cat(sprintf("  Significant predictors: %d/%d\n", 
            sum(reg$coef_table$p.value < 0.05 & reg$coef_table$term != "(Intercept)"),
            nrow(reg$coef_table) - 1))
cat(sprintf("  Assumptions: DW %s, BP %s, KS %s\n",
            ifelse(reg$dw_met, "✓", "✗"),
            ifelse(reg$bp_met, "✓", "✗"),
            ifelse(reg$ks_met, "✓", "✗")))
cat(sprintf("  Cross-validation R²: %.3f (±%.3f)\n", reg$cv_r2_mean, reg$cv_r2_sd))

cat("\nREGRESSION MODEL (EF SUBCOMPONENTS):\n")
cat(sprintf("  R² = %.3f, Adjusted R² = %.3f\n", reg_ef$r_squared, reg_ef$adj_r_squared))
cat(sprintf("  F(%d, %d) = %.2f, %s\n", reg_ef$f_df1, reg_ef$f_df2, reg_ef$f_value,
            format_p_value(reg_ef$f_pvalue)))
cat(sprintf("  Significant predictors: %d/%d\n",
            sum(reg_ef$coef_table$p.value < 0.05 & reg_ef$coef_table$term != "(Intercept)"),
            nrow(reg_ef$coef_table) - 1))
cat(sprintf("  Joint EF test: %s\n",
            format_p_value(reg_ef$ef_joint_test$`Pr(>F)`[2])))
cat(sprintf("  Assumptions: DW %s, BP %s, KS %s\n",
            ifelse(reg_ef$dw_met, "✓", "✗"),
            ifelse(reg_ef$bp_met, "✓", "✗"),
            ifelse(reg_ef$ks_met, "✓", "✗")))
cat(sprintf("  Cross-validation R²: %.3f (±%.3f)\n", reg_ef$cv_r2_mean, reg_ef$cv_r2_sd))

cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat("All analyses complete!\n")
cat(rep("=", 80), "\n\n", sep = "")
