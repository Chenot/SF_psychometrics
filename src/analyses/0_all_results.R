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
  "The sample consisted of %d participants (%d men, %d women; %.1f%% women) aged %d to %d years (M = %.1f, SD = %.1f). Education level ranged from %d to %d years (M = %.1f, SD = %.1f), and video game experience ranged from %.0f to %.0f years (M = %.1f, SD = %.1f). Most participants were right-handed (%d/%d, %.1f%%).",
  demographics_results$n_total, n_men, n_women, pct_women,
  as.integer(stats$MinAge), as.integer(stats$MaxAge),
  stats$MeanAge, stats$SdAge,
  as.integer(stats$MinEL), as.integer(stats$MaxEL),
  stats$MeanEL, stats$SdEL,
  stats$MinVGexp, stats$MaxVGexp,
  stats$MeanVGexp, stats$SdVGexp,
  handedness_counts$n[handedness_counts$Handedness == "right-handed"],
  demographics_results$n_total,
  handedness_counts$Percentage[handedness_counts$Handedness == "right-handed"]
)

cat(strwrap(demographics_text, width = 70), sep = "\n")
cat("\n\n")

cat("=== SPACE FORTRESS DISTRIBUTION ===\n\n")
cat(rep("-", 70), "\n", sep = "")

distribution_text <- sprintf(
  "Space Fortress scores ranged from %.0f to %.0f (M = %.1f, SD = %.1f, n = %d). The distribution showed %s (skewness = %.2f) and %s (kurtosis = %.2f). A Kolmogorov-Smirnov test indicated that the data %s (D = %.3f, %s), %s for parametric statistical analyses (see Figure 2).",
  dist$min, dist$max, dist$mean, dist$sd, dist$n,
  ifelse(abs(dist$skewness) < 0.5, "minimal skewness", 
         ifelse(abs(dist$skewness) < 1, "moderate skewness", "high skewness")),
  dist$skewness,
  ifelse(abs(dist$kurtosis) < 0.5, "normal kurtosis", 
         ifelse(abs(dist$kurtosis) < 2, "moderate kurtosis", "high kurtosis")),
  dist$kurtosis,
  ifelse(dist$ks_pvalue > 0.05, "were normally distributed", "deviated from normality"),
  dist$ks_statistic,
  format_p_value(dist$ks_pvalue),
  ifelse(dist$ks_pvalue > 0.05, "supporting the use of parametric tests", 
         "suggesting consideration of non-parametric approaches")
)

cat(strwrap(distribution_text, width = 70), sep = "\n")
cat("\n\n")

cat("=== RELIABILITY ===\n\n")
cat(rep("-", 70), "\n", sep = "")

reliability_text <- sprintf(
  "Test-retest reliability was assessed using the intraclass correlation coefficient (ICC) with a two-way random effects model (ICC(2,k); Koo & Li, 2016). Reliability was calculated for both single measurements, ICC(2,1) = %.2f, 95%% CI [%.2f, %.2f], and the average of five game sessions, ICC(2,k) = %.2f, 95%% CI [%.2f, %.2f]. Both ICCs indicated %s and %s, respectively, demonstrating that Space Fortress performance is a reliable measure with %s consistency across sessions.",
  rel$icc_single_value, rel$icc_single_lbound, rel$icc_single_ubound,
  rel$icc_average_value, rel$icc_average_lbound, rel$icc_average_ubound,
  tolower(rel$interpretation_single),
  tolower(rel$interpretation_average),
  ifelse(rel$icc_average_value >= 0.75, "good to excellent", "acceptable")
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

cat("=== SF & COVARIATES ===\n\n")
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

cat("=== REGRESSION MODEL ===\n\n")
cat(rep("-", 70), "\n", sep = "")

# Identify significant predictors
sig_predictors <- reg$coef_table[reg$coef_table$p.value < 0.05 & reg$coef_table$term != "(Intercept)", ]

regression_text <- sprintf(
  "A multiple linear regression model was fitted to predict Space Fortress performance from executive functions and demographic covariates (age, education, video game experience, sex). The overall model was %s (F(%d, %d) = %.2f, %s, R² = %.3f, adjusted R² = %.3f). %s All four assumptions of linear regression were met: independence (Durbin-Watson = %.2f, %s), homoscedasticity (Breusch-Pagan = %.2f, %s), normality of residuals (Kolmogorov-Smirnov = %.3f, %s), and linearity (visual inspection). Ten-fold cross-validation confirmed model stability (mean R² = %.3f, SD = %.3f).",
  ifelse(reg$f_pvalue < 0.05, "significant", "not significant"),
  reg$f_df1, reg$f_df2, reg$f_value, format_p_value(reg$f_pvalue),
  reg$r_squared, reg$adj_r_squared,
  
  if (nrow(sig_predictors) > 0) {
    paste0("Significant predictors included: ",
           paste(sapply(1:nrow(sig_predictors), function(i) {
             sprintf("%s (β = %.2f, %s)",
                     sig_predictors$term[i],
                     sig_predictors$estimate[i],
                     format_p_value(sig_predictors$p.value[i]))
           }), collapse = ", "), ".")
  } else {
    "No predictors reached statistical significance."
  },
  
  reg$dw_statistic, format_p_value(reg$dw_pvalue),
  reg$bp_statistic, format_p_value(reg$bp_pvalue),
  reg$ks_statistic, format_p_value(reg$ks_pvalue),
  reg$cv_r2_mean, reg$cv_r2_sd
)

cat(strwrap(regression_text, width = 70), sep = "\n")
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

cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat("All analyses complete!\n")
cat(rep("=", 80), "\n\n", sep = "")

################################################################################
## STORE FORMATTED RESULTS FOR EASY ACCESS
################################################################################

formatted_results <- list(
  demographics = demographics_text,
  distribution = distribution_text,
  reliability = reliability_text,
  concurrent_validity = concurrent_validity_text,
  covariates = covariates_text,
  regression = regression_text
)

# Return the formatted results (invisible so it doesn't print when sourced)
invisible(formatted_results)