## utils.R
# Author: Quentin Chenot
# Date: 2025-11-06
# Description: Utility functions for Space Fortress analysis scripts
# This file contains reusable functions for statistical analysis and data processing

################################################################################
## PACKAGE MANAGEMENT
################################################################################

#' Load required packages
#' Automatically installs missing packages
#'
#' @param packages Character vector of package names
load_packages <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE, quietly = TRUE)) {
      message(paste("Installing package:", package))
      install.packages(package)
      library(package, character.only = TRUE)
    }
  }
  message(paste("Successfully loaded", length(packages), "packages"))
}

################################################################################
## DATA LOADING
################################################################################

#' Load final dataset
#'
#' @param project_dir Path to project root directory
#' @param z_scored Logical, load z-scored data? (default: TRUE)
#' @return Data frame with loaded data
load_data <- function(project_dir, z_scored = TRUE) {
  if (z_scored) {
    file_path <- file.path(project_dir, "results", "combined_data", "data_zscored.csv")
  } else {
    file_path <- file.path(project_dir, "results", "combined_data", "data.csv")
  }
  
  if (!file.exists(file_path)) {
    stop(paste("Data file not found:", file_path))
  }
  
  df <- read.csv(file_path)
  message(paste("Loaded data from:", basename(file_path)))
  message(paste("Sample size:", nrow(df), "participants"))
  
  return(df)
}

################################################################################
## STATISTICAL FORMATTING
################################################################################

#' Format p-values for reporting
#'
#' @param p_value Numeric p-value
#' @return Character string with formatted p-value
format_p_value <- function(p_value) {
  if (is.na(p_value)) return("p = NA")
  
  if (p_value < .001) {
    return("p < .001")
  } else {
    formatted_p <- sprintf("%.3f", p_value)
    formatted_p <- sub("^0\\.", ".", formatted_p)  # Remove leading zero
    return(paste("p =", formatted_p))
  }
}

#' Format correlation coefficient
#'
#' @param r Correlation coefficient
#' @return Character string with formatted r-value
format_r <- function(r) {
  r_formatted <- sprintf("%.2f", r)
  r_formatted <- sub("^0\\.", ".", r_formatted)
  r_formatted <- sub("^-0\\.", "-.", r_formatted)
  return(r_formatted)
}

#' Format 95% confidence interval
#'
#' @param cor_result Result from cor.test()
#' @return Character string with formatted CI
format_ci <- function(cor_result) {
  if (is.null(cor_result$conf.int)) {
    return("[CI not available]")
  }
  
  lower <- cor_result$conf.int[1]
  upper <- cor_result$conf.int[2]
  
  sprintf("[%.2f, %.2f]", lower, upper)
}

#' Create detailed correlation text for reporting
#'
#' @param cor_result Result from correlation_test()
#' @param variable_name Name of the variable being correlated
#' @return Character string with formatted correlation report
format_correlation_text <- function(cor_result, variable_name) {
  r_value <- cor_result$cor_result$estimate
  r_formatted <- format_r(as.numeric(r_value))
  r_squared <- r_value^2
  r_squared_formatted <- format_r(as.numeric(r_squared))
  ci <- format_ci(cor_result$cor_result)
  
  if (cor_result$method == "pearson") {
    text <- paste0("r = ", r_formatted,
                   " (95% CI ", ci, 
                   ", ", format_p_value(cor_result$cor_result$p.value),
                   ", R² = ", r_squared_formatted,
                   ") for ", variable_name)
  } else {
    text <- paste0("ρ = ", r_formatted,
                   " (95% CI ", ci, 
                   ", ", format_p_value(cor_result$cor_result$p.value),
                   ", ρ² = ", r_squared_formatted,
                   ") for ", variable_name)
  }
  
  return(text)
}

################################################################################
## NORMALITY TESTING
################################################################################

#' Perform Kolmogorov-Smirnov normality test
#'
#' @param data Numeric vector to test
#' @param var_name Name of variable (for printing)
#' @return List with test results (statistic, p.value, is_normal)
ks_normality_test <- function(data, var_name) {
  # Remove NAs
  data_clean <- data[!is.na(data)]
  n <- length(data_clean)
  
  if (n < 3) {
    warning(paste("Insufficient data for", var_name))
    return(list(statistic = NA, p.value = NA, is_normal = FALSE))
  }
  
  # Add small jitter to avoid ties warning
  data_jittered <- data_clean + rnorm(n, mean = 0, sd = 1e-10)
  
  # Perform KS test
  ks_result <- ks.test(data_jittered, "pnorm", 
                       mean = mean(data_clean), 
                       sd = sd(data_clean))
  
  # Print results
  cat(sprintf("\n%s (n = %d):\n", var_name, n))
  cat(sprintf("  Kolmogorov-Smirnov D = %.4f, p = %.4f\n", 
              ks_result$statistic, ks_result$p.value))
  cat(sprintf("  Mean = %.3f, SD = %.3f\n", mean(data_clean), sd(data_clean)))
  
  is_normal <- ks_result$p.value > 0.05
  if (is_normal) {
    cat("  → Normally distributed (p > .05)\n")
  } else {
    cat("  → Not normally distributed (p ≤ .05)\n")
  }
  
  return(list(
    statistic = ks_result$statistic,
    p.value = ks_result$p.value,
    is_normal = is_normal
  ))
}

################################################################################
## CORRELATION ANALYSIS
################################################################################

#' Test normality and perform appropriate correlation
#'
#' @param x First variable (numeric vector)
#' @param y Second variable (numeric vector)
#' @param var_x_name Name of first variable
#' @param var_y_name Name of second variable
#' @param alpha Significance level for normality test (default: 0.05)
#' @param print_results Logical, whether to print results (default: TRUE)
#' @return List with correlation test results and method used
test_normality_and_correlate <- function(x, y, var_x_name, var_y_name, 
                                         alpha = 0.05, print_results = TRUE) {
  # Remove NAs
  complete_cases <- complete.cases(x, y)
  x_clean <- x[complete_cases]
  y_clean <- y[complete_cases]
  n <- length(x_clean)
  
  if (print_results) {
    cat("\n", rep("=", 70), "\n", sep = "")
    cat("CORRELATION ANALYSIS:", var_x_name, "vs", var_y_name, "\n")
    cat(rep("=", 70), "\n", sep = "")
    cat("Sample size:", n, "\n")
  }
  
  # Test normality
  if (print_results) cat("\nNormality Tests:\n")
  norm_x <- ks_normality_test(x_clean, var_x_name)
  norm_y <- ks_normality_test(y_clean, var_y_name)
  both_normal <- norm_x$is_normal && norm_y$is_normal
  
  # Choose correlation method
  if (both_normal) {
    method <- "pearson"
    if (print_results) {
      cat("\n→ Both variables normally distributed\n")
      cat("→ Using Pearson correlation\n")
    }
  } else {
    method <- "spearman"
    if (print_results) {
      cat("\n→ At least one variable not normally distributed\n")
      cat("→ Using Spearman correlation\n")
    }
  }
  
  # Perform correlation
  cor_result <- cor.test(x_clean, y_clean, method = method)
  
  # For Spearman, add bootstrap CI if not available
  if (method == "spearman" && is.null(cor_result$conf.int)) {
    if (requireNamespace("boot", quietly = TRUE)) {
      # Bootstrap function
      spearman_boot <- function(data, indices) {
        cor(data[indices, 1], data[indices, 2], method = "spearman")
      }
      
      # Perform bootstrap
      boot_data <- data.frame(x = x_clean, y = y_clean)
      boot_result <- boot::boot(boot_data, spearman_boot, R = 1000)
      boot_ci <- boot::boot.ci(boot_result, type = "perc")
      
      # Add CI to result
      cor_result$conf.int <- boot_ci$percent[4:5]
      attr(cor_result$conf.int, "conf.level") <- 0.95
      
      if (print_results) {
        cat("  (Bootstrap CI computed with 1000 replicates)\n")
      }
    } else {
      if (print_results) {
        cat("  (Install 'boot' package for confidence intervals)\n")
      }
    }
  }
  
  # Print correlation results
  if (print_results) {
    cat("\nCorrelation Results:\n")
    if (method == "pearson") {
      cat(sprintf("  Pearson r = %s\n", format_r(as.numeric(cor_result$estimate))))
      cat(sprintf("  R² = %s\n", format_r(as.numeric(cor_result$estimate^2))))
    } else {
      cat(sprintf("  Spearman ρ = %s\n", format_r(as.numeric(cor_result$estimate))))
      cat(sprintf("  ρ² = %s\n", format_r(as.numeric(cor_result$estimate^2))))
    }
    cat(sprintf("  %s\n", format_p_value(cor_result$p.value)))
    
    if (!is.null(cor_result$conf.int)) {
      cat(sprintf("  95%% CI: [%.3f, %.3f]\n", 
                  cor_result$conf.int[1], cor_result$conf.int[2]))
    }
    cat(rep("=", 70), "\n\n", sep = "")
  }
  
  return(list(
    cor_result = cor_result,
    method = method,
    n = n
  ))
}

#' Create correlation label for plots
#'
#' @param cor_result Result from correlation_test()
#' @return Character string with formatted label
create_cor_label <- function(cor_result) {
  r_value <- cor_result$cor_result$estimate
  r_formatted <- format_r(as.numeric(r_value))  # Ensure numeric conversion
  method <- cor_result$method
  
  if (method == "pearson") {
    r_squared <- r_value^2
    r_squared_formatted <- format_r(as.numeric(r_squared))
    label <- paste0("r = ", r_formatted, 
                    ", R² = ", r_squared_formatted, 
                    ", ", format_p_value(cor_result$cor_result$p.value))
  } else {
    # For Spearman, report rho² as effect size (proportion of variance explained)
    rho_squared <- r_value^2
    rho_squared_formatted <- format_r(as.numeric(rho_squared))
    label <- paste0("ρ = ", r_formatted, 
                    ", ρ² = ", rho_squared_formatted,
                    ", ", format_p_value(cor_result$cor_result$p.value))
  }
  
  return(label)
}

################################################################################
## T-TEST ANALYSIS
################################################################################

#' Create t-test label for plots
#'
#' @param t_test_result Result from t.test()
#' @param cohens_d Cohen's d effect size
#' @return Character string with formatted label
create_ttest_label <- function(t_test_result, cohens_d) {
  t_stat <- t_test_result$statistic
  
  label <- paste0(
    "t = ", sprintf("%.2f", t_stat),
    ", d = ", sprintf("%.2f", abs(cohens_d)),
    ", ", format_p_value(t_test_result$p.value)
  )
  
  return(label)
}

#' Calculate Cohen's d for independent samples
#'
#' @param mean1 Mean of group 1
#' @param mean2 Mean of group 2
#' @param sd1 Standard deviation of group 1
#' @param sd2 Standard deviation of group 2
#' @return Numeric value of Cohen's d
calculate_cohens_d <- function(mean1, mean2, sd1, sd2) {
  # Pooled standard deviation
  sd_pooled <- sqrt((sd1^2 + sd2^2) / 2)
  d <- (mean1 - mean2) / sd_pooled
  return(d)
}

################################################################################
## PRINT UTILITIES
################################################################################

#' Print section header
#'
#' @param title Section title
print_section <- function(title) {
  cat("\n")
  cat(rep("#", 80), "\n", sep = "")
  cat("##", title, "\n")
  cat(rep("#", 80), "\n\n", sep = "")
}

################################################################################
## END OF UTILS.R
################################################################################

message("\n=== utils.R loaded successfully ===")
message("Available functions:")
message("  - load_packages()")
message("  - load_data()")
message("  - format_p_value()")
message("  - format_r()")
message("  - ks_normality_test()")
message("  - correlation_test()")
message("  - create_cor_label()")
message("  - create_ttest_label()")
message("  - calculate_cohens_d()")
message("  - print_section()")