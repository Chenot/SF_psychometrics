## P08_SF_RegressionModel.R
# Author: Quentin Chenot
# Date: 2025-10-29
# Description: This script performs the regression model to establish the relationships between
#              Space Fortress performance, EF and co-variates (age, education level, video game experience, sex)
# Dependencies: ggplot2, ggpubr, ggExtra, dplyr, cowplot, broom, xtable, reshape2, rstudioapi
# Inputs: 
#   - Processed dataframe with z-scores: 'results/combined_data/data_zscored.csv'
# Outputs:
#   - Correlation plots saved as:
#   - Printed regression model results

## LOAD LIBRARIES
# Function to check if each required package is installed, and install it if not
required_packages <- c("ggplot2", "ggpubr", "ggExtra", "dplyr", "cowplot", 
                      "broom", "xtable", "reshape2", "rstudioapi", "viridis")
install_if_not_present <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
  }
}
lapply(required_packages, install_if_not_present) # Apply the function to each required package

# Load the libraries
library(ggplot2)
library(ggpubr)
library(ggExtra)
library(dplyr)
library(cowplot)
library(broom)
library(xtable)
library(reshape2)
library(rstudioapi)
library(lmtest)

## PATH MANAGEMENT
# Get the directory and path to this file
this_file <- rstudioapi::getSourceEditorContext()$path  # if using RStudio
this_dir <- dirname(this_file)
setwd(this_dir)
project_dir <- dirname(dirname(this_dir)) # Get the project directory

# Data & Figure paths
data_path <- file.path(project_dir, "data") # Define the relative path to the data files
df_final <- read.csv(file.path(project_dir,"results", "combined_data", "data_zscored.csv"))
figure_path <- file.path(project_dir, "results" , "figures") # Define the relative path to the data and results


# GLM SF, EF & covariates
# ensure factor for categorical predictors
df_final$Sex <- as.factor(df_final$Sex)
model1 <- lm(formula = zscore_SF ~ zscore_EF + Age + EducationLevel + VGexp + Sex, data = df_final)

# summary + explicit model F-test
model1_summary <- summary(model1)
print(model1_summary)

# extract and print F-statistic and p-value explicitly (model significance)
if (!is.null(model1_summary$fstatistic)) {
  fstat <- model1_summary$fstatistic
  Fval <- fstat[1]; df1 <- fstat[2]; df2 <- fstat[3]
  p_model <- pf(Fval, df1, df2, lower.tail = FALSE)
  cat(sprintf("Model F-test: F(%d, %d) = %.3f, p = %.3g\n", as.integer(df1), as.integer(df2), as.numeric(Fval), p_model))
} else {
  cat("No F-statistic available for this model.\n")
}

# --- Assumption checks ---
resid_vals <- residuals(model1)
fitted_vals <- fitted(model1)

# 1) Durbin-Watson (autocorrelation of residuals)
cat("\nDurbin-Watson test (autocorrelation of residuals):\n")
print(lmtest::dwtest(model1))

# 2) Breusch-Pagan test (heteroscedasticity)
cat("\nBreusch-Pagan test (heteroscedasticity):\n")
print(lmtest::bptest(model1))

# 3) Normality of residuals (Shapiro-Wilk) + Anderson-Darling if nortest available
cat("\nShapiro-Wilk test (normality of residuals):\n")
shp <- shapiro.test(resid_vals)
print(shp)

# Quick numeric summaries of residuals
cat("\nResiduals: mean (should be ~0), sd:\n")
cat(sprintf(" mean = %.5f, sd = %.5f\n", mean(resid_vals), sd(resid_vals)))

# Optional diagnostic plots (display; do not save here)
# Residuals vs Fitted and QQ plot using ggplot2
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  p1 <- ggplot(data = data.frame(fitted = fitted_vals, resid = resid_vals),
               aes(x = fitted, y = resid)) +
    geom_point(alpha = 0.6) + geom_hline(yintercept = 0, linetype = "dashed") +
    xlab("Fitted values") + ylab("Residuals") + ggtitle("Residuals vs Fitted")
  p2 <- ggplot(data = data.frame(sample = resid_vals), aes(sample = sample)) +
    stat_qq(alpha = 0.6) + stat_qq_line(color = "red") + ggtitle("QQ-plot of residuals")
  print(p1); print(p2)
}

# --- Cross-validation (10-fold) ---
set.seed(123)
k <- 10
n <- nrow(df_final)
folds <- sample(rep(1:k, length.out = n))

cv_metrics <- data.frame(fold = integer(0), RMSE = numeric(0), R2 = numeric(0))
for (fold in 1:k) {
  train_idx <- which(folds != fold)
  test_idx  <- which(folds == fold)
  train_df <- df_final[train_idx, , drop = FALSE]
  test_df  <- df_final[test_idx, , drop = FALSE]

  # fit on train
  fit_cv <- lm(zscore_SF ~ zscore_EF + Age + EducationLevel + VGexp + Sex, data = train_df)
  preds <- predict(fit_cv, newdata = test_df)
  obs <- test_df$zscore_SF

  # handle possible NA predictions
  valid <- !is.na(preds) & !is.na(obs)
  if (sum(valid) > 0) {
    SSE <- sum((obs[valid] - preds[valid])^2)
    SST <- sum((obs[valid] - mean(obs[valid]))^2)
    R2_fold <- ifelse(SST == 0, NA, 1 - SSE/SST)
    RMSE_fold <- sqrt(mean((obs[valid] - preds[valid])^2))
  } else {
    R2_fold <- NA; RMSE_fold <- NA
  }
  cv_metrics <- rbind(cv_metrics, data.frame(fold = fold, RMSE = RMSE_fold, R2 = R2_fold))
}

cat("\nCross-validation (", k, "-fold) results (per-fold):\n", sep = "")
print(cv_metrics)
cat("\nCross-validation summary:\n")
cat(sprintf(" RMSE: mean = %.4f ; sd = %.4f\n", mean(cv_metrics$RMSE, na.rm = TRUE), sd(cv_metrics$RMSE, na.rm = TRUE)))
cat(sprintf(" R2 : mean = %.4f ; sd = %.4f\n", mean(cv_metrics$R2, na.rm = TRUE), sd(cv_metrics$R2, na.rm = TRUE)))

# Optionally, tidy the model with broom if user wants a neat table
if (requireNamespace("broom", quietly = TRUE)) {
  library(broom)
  cat("\nModel coefficients (tidy):\n")
  print(broom::tidy(model1))
}