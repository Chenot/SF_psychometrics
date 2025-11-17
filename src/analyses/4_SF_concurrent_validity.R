## 4_SF_concurrent_validity.R
# Author: Quentin Chenot
# Date: 2025-11-06
# Description: This script tests hypotheses about relationships between Space Fortress performance
#              and executive functions.
#
# Research Questions:
#   1. Is SF performance correlated with overall Executive Functions? (expected: positive)
#   2. Is SF performance correlated with EF sub-components?
#      - Inhibition (expected: positive)
#      - Updating (expected: positive)
#      - Shifting (expected: positive)
#
# Methodology:
#   - Normality testing using Kolmogorov-Smirnov test (appropriate for n=145)
#   - Pearson correlation (if both variables normal) or Spearman (if not)
#
# Outputs:
#   Returns a list with correlation results and generates figures

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
required_packages <- c("ggplot2", "ggpubr", "dplyr", "cowplot")
load_packages(required_packages)

# Path management
project_dir <- dirname(dirname(getwd()))

# Load data
df_final <- load_data(project_dir, z_scored = TRUE)

# Create output directory
figure_path <- file.path(project_dir, "results", "figures")
dir.create(figure_path, recursive = TRUE, showWarnings = FALSE)

################################################################################
## ANALYSIS 1: SF & EXECUTIVE FUNCTIONS (COMPOSITE)
################################################################################

# Test normality and perform correlation
result_EF <- test_normality_and_correlate(
  df_final$zscore_SF, 
  df_final$zscore_EF,
  "Space Fortress (z-score)", 
  "Executive Functions (z-score)",
  print_results = FALSE
)

# Create correlation label and plot
cor_label_EF <- create_cor_label(result_EF)

plot_EF <- ggplot(df_final, aes(x = zscore_EF, y = zscore_SF)) +
  geom_smooth(method = lm, color = "black", fill = "lightgray", se = TRUE, alpha = 0.3) +
  geom_point(alpha = 0.8, size = 2, color = "black", shape = 16) +
  annotate("text", x = -1, y = 2.5, label = cor_label_EF, hjust = 0, size = 3.5) +
  theme_pubr() +
  xlab("Executive Functions (z-score)") +
  ylab("Space Fortress (z-score)") +
  theme(plot.title = element_text(hjust = 0.1))

# Save plot
print(plot_EF)
output_file_EF <- file.path(figure_path, "Fig3_SF_EF.pdf")
ggsave(output_file_EF, plot = plot_EF, width = 5, height = 4, units = "in",
       device = cairo_pdf)

################################################################################
## ANALYSIS 2: SF & INHIBITION
################################################################################

result_inhibition <- test_normality_and_correlate(
  df_final$zscore_SF, 
  df_final$zscore_inhibition,
  "Space Fortress (z-score)", 
  "Inhibition (z-score)",
  print_results = FALSE
)

cor_label_inhibition <- create_cor_label(result_inhibition)

plot_inhibition <- ggplot(df_final, aes(x = zscore_inhibition, y = zscore_SF)) +
  geom_smooth(method = lm, color = "black", fill = "lightgray", se = TRUE, alpha = 0.3) +
  geom_point(alpha = 0.8, size = 2, color = "black", shape = 16) +
  annotate("text", x = -1.5, y = 2.5, label = cor_label_inhibition, hjust = 0, size = 3.5) +
  theme_pubr() +
  xlab("Inhibition (z-score)") +
  ylab("Space Fortress (z-score)") +
  theme(plot.title = element_text(hjust = 0.1))

################################################################################
## ANALYSIS 3: SF & UPDATING
################################################################################

result_updating <- test_normality_and_correlate(
  df_final$zscore_SF, 
  df_final$zscore_WM,
  "Space Fortress (z-score)", 
  "Updating (z-score)",
  print_results = FALSE
)

cor_label_updating <- create_cor_label(result_updating)

plot_updating <- ggplot(df_final, aes(x = zscore_WM, y = zscore_SF)) +
  geom_smooth(method = lm, color = "black", fill = "lightgray", se = TRUE, alpha = 0.3) +
  geom_point(alpha = 0.8, size = 2, color = "black", shape = 16) +
  annotate("text", x = -1.5, y = 2.5, label = cor_label_updating, hjust = 0, size = 3.5) +
  theme_pubr() +
  xlab("Updating (z-score)") +
  ylab("Space Fortress (z-score)") +
  theme(plot.title = element_text(hjust = 0.1))

################################################################################
## ANALYSIS 4: SF & SHIFTING 
################################################################################

result_shifting <- test_normality_and_correlate(
  df_final$zscore_SF, 
  df_final$zscore_shifting,
  "Space Fortress (z-score)", 
  "Shifting (z-score)",
  print_results = FALSE
)

cor_label_shifting <- create_cor_label(result_shifting)

plot_shifting <- ggplot(df_final, aes(x = zscore_shifting, y = zscore_SF)) +
  geom_smooth(method = lm, color = "black", fill = "lightgray", se = TRUE, alpha = 0.3) +
  geom_point(alpha = 0.8, size = 2, color = "black", shape = 16) +
  annotate("text", x = -1.5, y = 2.5, label = cor_label_shifting, hjust = 0, size = 3.5) +
  theme_pubr() +
  xlab("Shifting (z-score)") +
  ylab("Space Fortress (z-score)") +
  theme(plot.title = element_text(hjust = 0.1))

################################################################################
## CREATE COMBINED SUBSCORES FIGURE
################################################################################

# Combine all EF subscore plots into a 1x3 grid
combined_plot_subscores <- plot_grid(
  plot_inhibition, plot_updating, plot_shifting,
  ncol = 3, nrow = 1,
  labels = c("A", "B", "C"),
  label_size = 14
)

# Save combined plot
output_file_subscores <- file.path(figure_path, "Fig4_SF_EFsubscores.pdf")
ggsave(output_file_subscores, plot = combined_plot_subscores, 
       width = 12, height = 4, units = "in",
       device = cairo_pdf)

################################################################################
## STORE RESULTS
################################################################################

# Create results list
concurrent_validity_results <- list(
  # EF composite
  EF_method = result_EF$method,
  EF_r = result_EF$cor_result$estimate,
  EF_ci_lower = result_EF$cor_result$conf.int[1],
  EF_ci_upper = result_EF$cor_result$conf.int[2],
  EF_pvalue = result_EF$cor_result$p.value,
  EF_n = result_EF$n,
  EF_plot_path = output_file_EF,
  
  # Inhibition
  inhibition_method = result_inhibition$method,
  inhibition_r = result_inhibition$cor_result$estimate,
  inhibition_ci_lower = result_inhibition$cor_result$conf.int[1],
  inhibition_ci_upper = result_inhibition$cor_result$conf.int[2],
  inhibition_pvalue = result_inhibition$cor_result$p.value,
  inhibition_n = result_inhibition$n,
  
  # Updating
  updating_method = result_updating$method,
  updating_r = result_updating$cor_result$estimate,
  updating_ci_lower = result_updating$cor_result$conf.int[1],
  updating_ci_upper = result_updating$cor_result$conf.int[2],
  updating_pvalue = result_updating$cor_result$p.value,
  updating_n = result_updating$n,
  
  # Shifting
  shifting_method = result_shifting$method,
  shifting_r = result_shifting$cor_result$estimate,
  shifting_ci_lower = result_shifting$cor_result$conf.int[1],
  shifting_ci_upper = result_shifting$cor_result$conf.int[2],
  shifting_pvalue = result_shifting$cor_result$p.value,
  shifting_n = result_shifting$n,
  
  subscores_plot_path = output_file_subscores
)

# Print confirmation
print(concurrent_validity_results)