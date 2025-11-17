## 6_SF_RegressionModel.R
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
#     * Normality of residuals (KS test, QQ-plot)
#     * Homoscedasticity (Breusch-Pagan test)
#     * Independence (Durbin-Watson test)
#   - 10-fold cross-validation for model validation
#
# Output:
#   Returns a list with all regression results

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
required_packages <- c("ggplot2", "ggpubr", "dplyr", "broom", "lmtest")
load_packages(required_packages)

# Path management
project_dir <- dirname(dirname(getwd()))

# Load data
df_final <- load_data(project_dir, z_scored = TRUE)

# Create output directory
figure_path <- file.path(project_dir, "results", "figures")
dir.create(figure_path, recursive = TRUE, showWarnings = FALSE)

################################################################################
## DATA PREPARATION
################################################################################

# Ensure Sex is a factor
df_final$Sex <- as.factor(df_final$Sex)

# Check for missing data
model_vars <- c("zscore_SF", "zscore_EF", "Age", "EducationLevel", "VGexp", "Sex")
missing_count <- sum(!complete.cases(df_final[, model_vars]))

################################################################################
## FIT REGRESSION MODEL
################################################################################

model1 <- lm(
  formula = zscore_SF ~ zscore_EF + Age + EducationLevel + VGexp + Sex, 
  data = df_final
)

model1_summary <- summary(model1)

# Extract model statistics
if (!is.null(model1_summary$fstatistic)) {
  fstat <- model1_summary$fstatistic
  f_value <- as.numeric(fstat[1])
  f_df1 <- as.integer(fstat[2])
  f_df2 <- as.integer(fstat[3])
  f_pvalue <- pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
} else {
  f_value <- NA
  f_df1 <- NA
  f_df2 <- NA
  f_pvalue <- NA
}

# Get coefficients with confidence intervals
coef_table <- broom::tidy(model1, conf.int = TRUE, conf.level = 0.95)

################################################################################
## TEST MODEL ASSUMPTIONS
################################################################################

# Extract residuals and fitted values
resid_vals <- residuals(model1)
fitted_vals <- fitted(model1)

# 1) Durbin-Watson test (independence)
dw_test <- lmtest::dwtest(model1)

# 2) Breusch-Pagan test (homoscedasticity)
bp_test <- lmtest::bptest(model1)

# 3) Kolmogorov-Smirnov test (normality of residuals)
resid_jittered <- resid_vals + rnorm(length(resid_vals), mean = 0, sd = 1e-10)
ks_test <- ks.test(resid_jittered, "pnorm", 
                   mean = mean(resid_vals), 
                   sd = sd(resid_vals))

################################################################################
## CREATE DIAGNOSTIC PLOTS
################################################################################

diag_df <- data.frame(
  fitted = fitted_vals,
  resid = resid_vals,
  std_resid = rstandard(model1)
)

# 1) Residuals vs Fitted
plot1 <- ggplot(diag_df, aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(se = TRUE, color = "blue", alpha = 0.3) +
  theme_pubr() +
  xlab("Fitted Values") +
  ylab("Residuals") +
  ggtitle("Residuals vs Fitted")

# 2) QQ-plot
plot2 <- ggplot(diag_df, aes(sample = resid)) +
  stat_qq(alpha = 0.6, size = 2) +
  stat_qq_line(color = "red", linewidth = 1) +
  theme_pubr() +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  ggtitle("Normal Q-Q Plot")

# 3) Scale-Location
plot3 <- ggplot(diag_df, aes(x = fitted, y = sqrt(abs(std_resid)))) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(se = TRUE, color = "red", alpha = 0.3) +
  theme_pubr() +
  xlab("Fitted Values") +
  ylab("√|Standardized Residuals|") +
  ggtitle("Scale-Location")

# 4) Residuals histogram
plot4 <- ggplot(diag_df, aes(x = resid)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  theme_pubr() +
  xlab("Residuals") +
  ylab("Frequency") +
  ggtitle("Distribution of Residuals")

# Save diagnostic plots (optional - can be removed if not needed)
# ggsave(file.path(figure_path, "Fig6_diagnostics.pdf"), 
#        plot = gridExtra::grid.arrange(plot1, plot2, plot3, plot4, ncol = 2),
#        width = 10, height = 8)

################################################################################
## CROSS-VALIDATION
################################################################################

set.seed(123)
k <- 10
n <- nrow(df_final)
folds <- sample(rep(1:k, length.out = n))

cv_metrics <- data.frame(
  fold = integer(0), 
  RMSE = numeric(0), 
  R2 = numeric(0),
  MAE = numeric(0)
)

for (fold in 1:k) {
  train_idx <- which(folds != fold)
  test_idx  <- which(folds == fold)
  train_df <- df_final[train_idx, , drop = FALSE]
  test_df  <- df_final[test_idx, , drop = FALSE]
  
  fit_cv <- lm(
    zscore_SF ~ zscore_EF + Age + EducationLevel + VGexp + Sex, 
    data = train_df
  )
  
  preds <- predict(fit_cv, newdata = test_df)
  obs <- test_df$zscore_SF
  
  valid <- !is.na(preds) & !is.na(obs)
  
  if (sum(valid) > 0) {
    RMSE_fold <- sqrt(mean((obs[valid] - preds[valid])^2))
    SSE <- sum((obs[valid] - preds[valid])^2)
    SST <- sum((obs[valid] - mean(obs[valid]))^2)
    R2_fold <- ifelse(SST == 0, NA, 1 - SSE/SST)
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

################################################################################
## STORE RESULTS
################################################################################

regression_results <- list(
  # Sample info
  model_vars = model_vars,
  n_total = nrow(df_final),
  n_complete = nrow(df_final) - missing_count,
  n_missing = missing_count,
  
  # Model object and summary
  model = model1,
  model_summary = model1_summary,
  
  # Model statistics
  r_squared = model1_summary$r.squared,
  adj_r_squared = model1_summary$adj.r.squared,
  residual_se = model1_summary$sigma,
  df_residual = model1_summary$df[2],
  f_value = f_value,
  f_df1 = f_df1,
  f_df2 = f_df2,
  f_pvalue = f_pvalue,
  
  # Coefficients
  coef_table = coef_table,
  
  # Assumptions tests
  dw_statistic = dw_test$statistic,
  dw_pvalue = dw_test$p.value,
  dw_met = dw_test$p.value > 0.05,
  
  bp_statistic = bp_test$statistic,
  bp_pvalue = bp_test$p.value,
  bp_met = bp_test$p.value > 0.05,
  
  ks_statistic = ks_test$statistic,
  ks_pvalue = ks_test$p.value,
  ks_met = ks_test$p.value > 0.05,
  resid_mean = mean(resid_vals),
  resid_sd = sd(resid_vals),
  
  # Diagnostic plots
  plot_resid_fitted = plot1,
  plot_qq = plot2,
  plot_scale_location = plot3,
  plot_histogram = plot4,
  
  # Cross-validation
  cv_metrics = cv_metrics,
  cv_rmse_mean = mean(cv_metrics$RMSE, na.rm = TRUE),
  cv_rmse_sd = sd(cv_metrics$RMSE, na.rm = TRUE),
  cv_r2_mean = mean(cv_metrics$R2, na.rm = TRUE),
  cv_r2_sd = sd(cv_metrics$R2, na.rm = TRUE),
  cv_mae_mean = mean(cv_metrics$MAE, na.rm = TRUE),
  cv_mae_sd = sd(cv_metrics$MAE, na.rm = TRUE)
)

# Print confirmation
cat("Regression analysis completed.\n")
cat(sprintf("Model R² = %.3f, Adjusted R² = %.3f\n", 
            regression_results$r_squared, 
            regression_results$adj_r_squared))
cat(sprintf("F(%d, %d) = %.3f, %s\n", 
            regression_results$f_df1, 
            regression_results$f_df2,
            regression_results$f_value,
            format_p_value(regression_results$f_pvalue)))