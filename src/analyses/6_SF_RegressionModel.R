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
required_packages <- c("ggplot2", "ggpubr", "dplyr", "broom", "lmtest", "car")
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

model_formula <- zscore_SF ~ zscore_EF + Age + EducationLevel + VGexp + Sex

# Simplified model used for cross-validation (keep full model for reported regression)
model_formula_cv <- zscore_SF ~ zscore_EF + Sex

# Correlation matrix among numeric predictors for multicollinearity screening
numeric_predictors <- c("zscore_EF", "Age", "EducationLevel", "VGexp")
predictor_cor_matrix <- stats::cor(
  df_final[, numeric_predictors],
  use = "pairwise.complete.obs",
  method = "pearson"
)

################################################################################
## FIT REGRESSION MODEL
################################################################################

model1 <- lm(
  formula = model_formula,
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

# VIF/GVIF diagnostics for multicollinearity
vif_raw <- car::vif(model1)
if (is.matrix(vif_raw)) {
  adj_col <- "GVIF^(1/(2*Df))"
  if (!(adj_col %in% colnames(vif_raw))) {
    adj_col <- colnames(vif_raw)[ncol(vif_raw)]
  }

  vif_table <- data.frame(
    term = rownames(vif_raw),
    GVIF = as.numeric(vif_raw[, "GVIF"]),
    Df = as.numeric(vif_raw[, "Df"]),
    GVIF_adj = as.numeric(vif_raw[, adj_col]),
    row.names = NULL,
    check.names = FALSE
  )

  vif_metric <- vif_table$GVIF_adj
} else {
  vif_table <- data.frame(
    term = names(vif_raw),
    VIF = as.numeric(vif_raw),
    row.names = NULL,
    check.names = FALSE
  )

  vif_metric <- vif_table$VIF
}

max_abs_cor <- suppressWarnings(max(abs(predictor_cor_matrix[upper.tri(predictor_cor_matrix)]), na.rm = TRUE))
if (!is.finite(max_abs_cor)) {
  max_abs_cor <- NA_real_
}

high_cor_pairs <- which(abs(predictor_cor_matrix) > 0.80 & upper.tri(predictor_cor_matrix), arr.ind = TRUE)
high_cor_terms <- if (nrow(high_cor_pairs) > 0) {
  apply(high_cor_pairs, 1, function(idx) {
    paste0(rownames(predictor_cor_matrix)[idx[1]], "-", colnames(predictor_cor_matrix)[idx[2]])
  })
} else {
  character(0)
}

vif_over_10_terms <- vif_table$term[which(vif_metric > 10)]

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
## REPEATED K-FOLD CROSS-VALIDATION
## Performs k-fold CV repeated `repeats` times and records fold + repeat identifiers
################################################################################

set.seed(123)
k <- 10
repeats <- 50
n <- nrow(df_final)

cv_metrics <- data.frame(
  repeat_id = integer(0),
  fold = integer(0), 
  RMSE = numeric(0), 
  R2 = numeric(0),
  MAE = numeric(0)
)

for (rep_i in 1:repeats) {
  folds <- sample(rep(1:k, length.out = n))
  for (fold in 1:k) {
    train_idx <- which(folds != fold)
    test_idx  <- which(folds == fold)
    train_df <- df_final[train_idx, , drop = FALSE]
    test_df  <- df_final[test_idx, , drop = FALSE]
    
    fit_cv <- lm(
      zscore_SF ~ zscore_EF + Sex, 
      # zscore_SF ~ zscore_EF + Age + EducationLevel + VGexp + Sex,
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
      repeat_id = rep_i,
      fold = fold, 
      RMSE = RMSE_fold, 
      R2 = R2_fold,
      MAE = MAE_fold
    ))
  }
}

# Assumptions summary
assumptions <- list(
  independence = dw_test$p.value > 0.05,
  homoscedasticity = bp_test$p.value > 0.05,
  normality = ks_test$p.value > 0.05,
  no_multicollinearity = (length(high_cor_terms) == 0) && !any(vif_metric > 10, na.rm = TRUE)
)

################################################################################
# CV METRIC DISTRIBUTION PLOTS
# Histogram + density for RMSE and R2 with mean and median vertical lines
################################################################################

# RMSE plot
if (all(is.na(cv_metrics$RMSE))) {
  plot_cv_rmse <- NULL
} else {
  rmse_mean <- mean(cv_metrics$RMSE, na.rm = TRUE)
  rmse_med  <- median(cv_metrics$RMSE, na.rm = TRUE)
  plot_cv_rmse <- ggplot(cv_metrics, aes(x = RMSE)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
    geom_density(color = "darkblue", linewidth = 1, alpha = 0.3) +
    geom_vline(xintercept = rmse_mean, color = "red", linetype = "dashed", linewidth = 1) +
    geom_vline(xintercept = rmse_med, color = "darkgreen", linetype = "dotted", linewidth = 1) +
    theme_pubr() +
    xlab("RMSE") + ylab("Density") +
    ggtitle(bquote(atop(SF == beta[0] + beta[1]*EF + beta[2]*Sex + epsilon,
               .(paste0("Distribution of CV RMSE (k = ", k, ", repeats = ", repeats, ")")) )) ) +
    labs(subtitle = sprintf("mean = %.3f, median = %.3f", rmse_mean, rmse_med)) +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

  # save
  try(ggsave(file.path(figure_path, "SuppFig_CV_RMSE_density.pdf"), plot_cv_rmse, width = 7, height = 5))
}

# R2 plot
if (all(is.na(cv_metrics$R2))) {
  plot_cv_r2 <- NULL
} else {
  r2_mean <- mean(cv_metrics$R2, na.rm = TRUE)
  r2_med  <- median(cv_metrics$R2, na.rm = TRUE)
  plot_cv_r2 <- ggplot(cv_metrics, aes(x = R2)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
    geom_density(color = "darkblue", linewidth = 1, alpha = 0.3) +
    geom_vline(xintercept = r2_mean, color = "red", linetype = "dashed", linewidth = 1) +
    geom_vline(xintercept = r2_med, color = "darkgreen", linetype = "dotted", linewidth = 1) +
    theme_pubr() +
    xlab(expression(R^2)) + ylab("Density") +
    ggtitle(bquote(atop(SF == beta[0] + beta[1]*EF + beta[2]*Sex + epsilon,
               .(paste0("Distribution of CV R² (k = ", k, ", repeats = ", repeats, ")")) )) ) +
    labs(subtitle = sprintf("mean = %.3f, median = %.3f", r2_mean, r2_med)) +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

  # save
  try(ggsave(file.path(figure_path, "SuppFig_CV_R2_density.pdf"), plot_cv_r2, width = 7, height = 5))
}

################################################################################
## STORE RESULTS
################################################################################

regression_results <- list(
  # Sample info
  model_vars = model_vars,
  model_formula = deparse(model_formula),
  n_total = nrow(df_final),
  n_complete = nrow(df_final) - missing_count,
  n_missing = missing_count,

  # Multicollinearity diagnostics
  predictor_cor_matrix = predictor_cor_matrix,
  max_abs_predictor_correlation = max_abs_cor,
  high_correlation_pairs_over_0_80 = high_cor_terms,
  vif_table = vif_table,
  vif_over_10_terms = vif_over_10_terms,
  
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
  plot_cv_rmse = plot_cv_rmse,
  plot_cv_r2 = plot_cv_r2,
  
  # Cross-validation
  cv_metrics = cv_metrics,
  cv_repeats = repeats,
  cv_rmse_mean = mean(cv_metrics$RMSE, na.rm = TRUE),
  cv_rmse_sd = sd(cv_metrics$RMSE, na.rm = TRUE),
  cv_r2_mean = mean(cv_metrics$R2, na.rm = TRUE),
  cv_r2_sd = sd(cv_metrics$R2, na.rm = TRUE),
  cv_r2_median = median(cv_metrics$R2, na.rm = TRUE),
  cv_r2_min = ifelse(all(is.na(cv_metrics$R2)), NA, min(cv_metrics$R2, na.rm = TRUE)),
  cv_r2_max = ifelse(all(is.na(cv_metrics$R2)), NA, max(cv_metrics$R2, na.rm = TRUE)),
  cv_mae_mean = mean(cv_metrics$MAE, na.rm = TRUE),
  cv_mae_sd = sd(cv_metrics$MAE, na.rm = TRUE),

  # Combined assumptions decision
  assumptions = assumptions
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

cat("\nAssumption checks:\n")
cat(sprintf("- Independence (Durbin-Watson): %s\n", ifelse(regression_results$dw_met, "met", "not met")))
cat(sprintf("- Homoscedasticity (Breusch-Pagan): %s\n", ifelse(regression_results$bp_met, "met", "not met")))
cat(sprintf("- Residual normality (KS): %s\n", ifelse(regression_results$ks_met, "met", "not met")))
cat(sprintf("- No multicollinearity (|r| < .80 and VIF < 10): %s\n", ifelse(regression_results$assumptions$no_multicollinearity, "met", "not met")))

if (length(regression_results$high_correlation_pairs_over_0_80) > 0) {
  cat("  High-correlation pairs:", paste(regression_results$high_correlation_pairs_over_0_80, collapse = ", "), "\n")
}

if (length(regression_results$vif_over_10_terms) > 0) {
  cat("  Terms with VIF/GVIF-adjusted > 10:", paste(regression_results$vif_over_10_terms, collapse = ", "), "\n")
}

# Cross-validation summary (include median and range for R2 per reviewer request)
cat(sprintf("\nCross-validation (%d-fold, %d repeats) summary:\n", k, repeats))
cat(sprintf("- RMSE mean (sd): %.3f (%.3f)\n", regression_results$cv_rmse_mean, regression_results$cv_rmse_sd))
cat(sprintf("- MAE mean (sd): %.3f (%.3f)\n", regression_results$cv_mae_mean, regression_results$cv_mae_sd))
cat(sprintf("- R² mean (sd): %.3f (%.3f)\n", regression_results$cv_r2_mean, regression_results$cv_r2_sd))
cat(sprintf("- R² median: %.3f\n", regression_results$cv_r2_median))
cat(sprintf("- R² range: [%.3f, %.3f]\n", regression_results$cv_r2_min, regression_results$cv_r2_max))