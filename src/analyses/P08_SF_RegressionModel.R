## P08_SF_RegressionModel.R
# Author: Quentin Chenot
# Date: 2025-11-06
# Description: This script performs multiple linear regression to predict Space Fortress 
#              performance from Executive Functions and covariates.
#
# Research Question:
#   What factors predict Space Fortress performance?
#   - Executive Functions (primary predictor)
#   - Age
#   - Education Level
#   - Video Game Experience
#   - Sex
#
# Methodology:
#   - Multiple linear regression with all predictors
#   - Model assumptions testing:
#     * Linearity (residuals vs fitted plot)
#     * Normality of residuals (Shapiro-Wilk test, QQ-plot)
#     * Homoscedasticity (Breusch-Pagan test)
#     * Independence (Durbin-Watson test)
#   - 10-fold cross-validation for model validation
#
# Outputs:
#   - Statistical results printed to console
#   - Model diagnostics printed to console
#   - Diagnostic plots displayed
#   - Model coefficients table

################################################################################
## SETUP
################################################################################

# Source utility functions
source("utils.R")

# Load required packages
required_packages <- c("ggplot2", "ggpubr", "dplyr", "broom", "lmtest", "rstudioapi")
load_packages(required_packages)

# Path management
this_file <- rstudioapi::getSourceEditorContext()$path
this_dir <- dirname(this_file)
setwd(this_dir)
project_dir <- dirname(dirname(this_dir))

# Load data
df_final <- load_data(project_dir, z_scored = TRUE)

# Create output directory
figure_path <- file.path(project_dir, "results", "figures")
dir.create(figure_path, recursive = TRUE, showWarnings = FALSE)

################################################################################
## DATA PREPARATION
################################################################################

print_section("DATA PREPARATION")

# Ensure Sex is a factor
df_final$Sex <- as.factor(df_final$Sex)

# Check for missing data in model variables
model_vars <- c("zscore_SF", "zscore_EF", "Age", "EducationLevel", "VGexp", "Sex")
missing_count <- sum(!complete.cases(df_final[, model_vars]))

cat("Model variables:\n")
cat(paste("  -", model_vars, collapse = "\n"), "\n\n")
cat(sprintf("Complete cases: %d/%d\n", nrow(df_final) - missing_count, nrow(df_final)))

if (missing_count > 0) {
  cat(sprintf("WARNING: %d cases with missing data will be excluded\n", missing_count))
}

################################################################################
## REGRESSION MODEL
################################################################################

print_section("MULTIPLE LINEAR REGRESSION MODEL")

cat("Model formula:\n")
cat("  zscore_SF ~ zscore_EF + Age + EducationLevel + VGexp + Sex\n\n")

# Fit the model
model1 <- lm(
  formula = zscore_SF ~ zscore_EF + Age + EducationLevel + VGexp + Sex, 
  data = df_final
)

# Print model summary
model1_summary <- summary(model1)
print(model1_summary)

# Extract and print model-level statistics
cat("\n", rep("-", 70), "\n", sep = "")
cat("MODEL-LEVEL STATISTICS\n")
cat(rep("-", 70), "\n", sep = "")

if (!is.null(model1_summary$fstatistic)) {
  fstat <- model1_summary$fstatistic
  Fval <- fstat[1]
  df1 <- fstat[2]
  df2 <- fstat[3]
  p_model <- pf(Fval, df1, df2, lower.tail = FALSE)
  
  cat(sprintf("Model F-test: F(%d, %d) = %.3f, %s\n", 
              as.integer(df1), as.integer(df2), 
              as.numeric(Fval), 
              format_p_value(p_model)))
  cat(sprintf("Multiple R²: %.3f\n", model1_summary$r.squared))
  cat(sprintf("Adjusted R²: %.3f\n", model1_summary$adj.r.squared))
  cat(sprintf("Residual SE: %.3f on %d degrees of freedom\n", 
              model1_summary$sigma, model1_summary$df[2]))
} else {
  cat("No F-statistic available for this model.\n")
}

# Print tidy coefficients table with confidence intervals
cat("\n", rep("-", 70), "\n", sep = "")
cat("MODEL COEFFICIENTS\n")
cat(rep("-", 70), "\n", sep = "")

# Get coefficients with confidence intervals
coef_table <- broom::tidy(model1, conf.int = TRUE, conf.level = 0.95)
coef_table$p.value.formatted <- sapply(coef_table$p.value, format_p_value)

# Format confidence intervals
coef_table$CI_formatted <- sprintf("[%.3f, %.3f]", coef_table$conf.low, coef_table$conf.high)

# Print formatted table
print(coef_table[, c("term", "estimate", "std.error", "statistic", "CI_formatted", "p.value.formatted")])

# Alternative: More detailed output
cat("\nDetailed coefficients with 95% CI:\n")
cat(rep("-", 70), "\n", sep = "")
for (i in 1:nrow(coef_table)) {
  cat(sprintf("%-20s β = %7.3f, 95%% CI [%6.3f, %6.3f], SE = %.3f, t = %6.3f, %s\n",
              coef_table$term[i],
              coef_table$estimate[i],
              coef_table$conf.low[i],
              coef_table$conf.high[i],
              coef_table$std.error[i],
              coef_table$statistic[i],
              coef_table$p.value.formatted[i]))
}

################################################################################
## MODEL ASSUMPTIONS TESTING
################################################################################

print_section("MODEL ASSUMPTIONS TESTING")

# Extract residuals and fitted values
resid_vals <- residuals(model1)
fitted_vals <- fitted(model1)

cat("Testing the four key assumptions of linear regression:\n\n")

# 1) Independence: Durbin-Watson test
cat("1. INDEPENDENCE OF RESIDUALS\n")
cat(rep("-", 70), "\n", sep = "")
cat("   Durbin-Watson test (H0: no autocorrelation)\n")
dw_test <- lmtest::dwtest(model1)
cat(sprintf("   DW statistic = %.3f, %s\n", 
            dw_test$statistic, 
            format_p_value(dw_test$p.value)))
if (dw_test$p.value > 0.05) {
  cat("   → Assumption met: No significant autocorrelation\n\n")
} else {
  cat("   → WARNING: Significant autocorrelation detected\n\n")
}

# 2) Homoscedasticity: Breusch-Pagan test
cat("2. HOMOSCEDASTICITY (Constant Variance)\n")
cat(rep("-", 70), "\n", sep = "")
cat("   Breusch-Pagan test (H0: homoscedastic)\n")
bp_test <- lmtest::bptest(model1)
cat(sprintf("   BP statistic = %.3f, %s\n", 
            bp_test$statistic, 
            format_p_value(bp_test$p.value)))
if (bp_test$p.value > 0.05) {
  cat("   → Assumption met: Variance is constant\n\n")
} else {
  cat("   → WARNING: Heteroscedasticity detected\n\n")
}

# 3) Normality of residuals: Kolmogorov-Smirnov test
cat("3. NORMALITY OF RESIDUALS\n")
cat(rep("-", 70), "\n", sep = "")
cat("   Kolmogorov-Smirnov test (H0: normally distributed)\n")

# Add small jitter to avoid ties warning
resid_jittered <- resid_vals + rnorm(length(resid_vals), mean = 0, sd = 1e-10)

# Perform KS test against normal distribution with mean=0 and SD=sd(residuals)
ks_test <- ks.test(resid_jittered, "pnorm", 
                   mean = mean(resid_vals), 
                   sd = sd(resid_vals))

cat(sprintf("   KS D = %.4f, %s\n", 
            ks_test$statistic, 
            format_p_value(ks_test$p.value)))

# Residual statistics
cat(sprintf("   Residuals: Mean = %.5f (should be ~0), SD = %.3f\n", 
            mean(resid_vals), sd(resid_vals)))

if (ks_test$p.value > 0.05) {
  cat("   → Assumption met: Residuals are normally distributed\n\n")
} else {
  cat("   → WARNING: Residuals deviate from normality\n\n")
}

# 4) Linearity: Visual inspection
cat("4. LINEARITY\n")
cat(rep("-", 70), "\n", sep = "")
cat("   Visual inspection via diagnostic plots (see below)\n\n")

################################################################################
## DIAGNOSTIC PLOTS
################################################################################

print_section("DIAGNOSTIC PLOTS")

cat("Generating diagnostic plots...\n\n")

# Create diagnostic plots
diag_df <- data.frame(
  fitted = fitted_vals,
  resid = resid_vals,
  std_resid = rstandard(model1)
)

# 1) Residuals vs Fitted (linearity & homoscedasticity)
plot1 <- ggplot(diag_df, aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(se = TRUE, color = "blue", alpha = 0.3) +
  theme_pubr() +
  xlab("Fitted Values") +
  ylab("Residuals") +
  ggtitle("Residuals vs Fitted Values",
          subtitle = "Check for linearity & homoscedasticity")

print(plot1)

# 2) QQ-plot (normality of residuals)
plot2 <- ggplot(diag_df, aes(sample = resid)) +
  stat_qq(alpha = 0.6, size = 2) +
  stat_qq_line(color = "red", linewidth = 1) +
  theme_pubr() +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  ggtitle("Normal Q-Q Plot",
          subtitle = "Check for normality of residuals")

print(plot2)

# 3) Scale-Location plot (homoscedasticity)
plot3 <- ggplot(diag_df, aes(x = fitted, y = sqrt(abs(std_resid)))) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(se = TRUE, color = "red", alpha = 0.3) +
  theme_pubr() +
  xlab("Fitted Values") +
  ylab("√|Standardized Residuals|") +
  ggtitle("Scale-Location Plot",
          subtitle = "Check for homoscedasticity")

print(plot3)

# 4) Residuals histogram
plot4 <- ggplot(diag_df, aes(x = resid)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  theme_pubr() +
  xlab("Residuals") +
  ylab("Frequency") +
  ggtitle("Distribution of Residuals",
          subtitle = "Check for normality")

print(plot4)

################################################################################
## CROSS-VALIDATION
################################################################################

print_section("10-FOLD CROSS-VALIDATION")

cat("Performing 10-fold cross-validation to assess model generalizability...\n\n")

# Set seed for reproducibility
set.seed(123)
k <- 10
n <- nrow(df_final)
folds <- sample(rep(1:k, length.out = n))

# Initialize results storage
cv_metrics <- data.frame(
  fold = integer(0), 
  RMSE = numeric(0), 
  R2 = numeric(0),
  MAE = numeric(0)
)

# Perform k-fold CV
for (fold in 1:k) {
  # Split data
  train_idx <- which(folds != fold)
  test_idx  <- which(folds == fold)
  train_df <- df_final[train_idx, , drop = FALSE]
  test_df  <- df_final[test_idx, , drop = FALSE]
  
  # Fit on training set
  fit_cv <- lm(
    zscore_SF ~ zscore_EF + Age + EducationLevel + VGexp + Sex, 
    data = train_df
  )
  
  # Predict on test set
  preds <- predict(fit_cv, newdata = test_df)
  obs <- test_df$zscore_SF
  
  # Calculate metrics (handling possible NA predictions)
  valid <- !is.na(preds) & !is.na(obs)
  
  if (sum(valid) > 0) {
    # RMSE
    RMSE_fold <- sqrt(mean((obs[valid] - preds[valid])^2))
    
    # R²
    SSE <- sum((obs[valid] - preds[valid])^2)
    SST <- sum((obs[valid] - mean(obs[valid]))^2)
    R2_fold <- ifelse(SST == 0, NA, 1 - SSE/SST)
    
    # MAE
    MAE_fold <- mean(abs(obs[valid] - preds[valid]))
  } else {
    RMSE_fold <- NA
    R2_fold <- NA
    MAE_fold <- NA
  }
  
  cv_metrics <- rbind(cv_metrics, data.frame(
    fold = fold, 
    RMSE = RMSE_fold, 
    R2 = R2_fold,
    MAE = MAE_fold
  ))
}

# Print fold-by-fold results
cat("Cross-validation results by fold:\n")
print(cv_metrics)

# Summary statistics
cat("\n", rep("-", 70), "\n", sep = "")
cat("CROSS-VALIDATION SUMMARY\n")
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("RMSE: Mean = %.4f, SD = %.4f\n", 
            mean(cv_metrics$RMSE, na.rm = TRUE), 
            sd(cv_metrics$RMSE, na.rm = TRUE)))
cat(sprintf("R²:   Mean = %.4f, SD = %.4f\n", 
            mean(cv_metrics$R2, na.rm = TRUE), 
            sd(cv_metrics$R2, na.rm = TRUE)))
cat(sprintf("MAE:  Mean = %.4f, SD = %.4f\n", 
            mean(cv_metrics$MAE, na.rm = TRUE), 
            sd(cv_metrics$MAE, na.rm = TRUE)))

# Interpretation
cat("\nInterpretation:\n")
if (abs(mean(cv_metrics$R2, na.rm = TRUE) - model1_summary$r.squared) < 0.05) {
  cat("  → Model generalizes well (CV R² ≈ training R²)\n")
} else {
  cat("  → WARNING: Possible overfitting (CV R² < training R²)\n")
}

################################################################################
## SUMMARY FOR MANUSCRIPT
################################################################################

print_section("SUMMARY FOR MANUSCRIPT")

cat("Model equation:\n")
cat("  SF = β₀ + β₁(EF) + β₂(Age) + β₃(Education) + β₄(VG Experience) + β₅(Sex) + ε\n\n")

cat("Results:\n")
cat(sprintf("  Multiple R² = %.3f, Adjusted R² = %.3f\n", 
            model1_summary$r.squared, 
            model1_summary$adj.r.squared))
cat(sprintf("  F(%d, %d) = %.3f, %s\n\n",
            as.integer(fstat[2]), as.integer(fstat[3]),
            as.numeric(fstat[1]),
            format_p_value(p_model)))

cat("Significant predictors (α = .05):\n")
sig_predictors <- coef_table[coef_table$p.value < 0.05, ]
if (nrow(sig_predictors) > 0) {
  for (i in 1:nrow(sig_predictors)) {
    cat(sprintf("  - %s: β = %.3f, SE = %.3f, t = %.3f, %s\n",
                sig_predictors$term[i],
                sig_predictors$estimate[i],
                sig_predictors$std.error[i],
                sig_predictors$statistic[i],
                format_p_value(sig_predictors$p.value[i])))
  }
} else {
  cat("  (None)\n")
}

cat("\nModel assumptions:\n")
cat(sprintf("  Independence: %s\n", 
            ifelse(dw_test$p.value > 0.05, "✓ Met", "✗ Violated")))
cat(sprintf("  Homoscedasticity: %s\n", 
            ifelse(bp_test$p.value > 0.05, "✓ Met", "✗ Violated")))
cat(sprintf("  Normality: %s\n", 
            ifelse(ks_test$p.value > 0.05, "✓ Met", "✗ Violated")))
cat("  Linearity: See diagnostic plots\n")

cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat("Analysis complete!\n")
cat(rep("=", 80), "\n\n", sep = "")