## P06_SF_concurrent_validity.R
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
#   - Statistical results printed to console
#   - SF vs EF composite plot: 'results/figures/Fig3_SF_EF.pdf'
#   - SF vs EF subscores plot: 'results/figures/Fig4_SF_EFsubscores.pdf'

################################################################################
## SETUP
################################################################################

# Source utility functions
source("utils.R")

# Load required packages
required_packages <- c("ggplot2", "ggpubr", "dplyr", "cowplot", "rstudioapi")
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
## ANALYSIS 1: SF & EXECUTIVE FUNCTIONS (COMPOSITE SCORE)
################################################################################

print_section("ANALYSIS 1: Space Fortress & Executive Functions (Composite)")

# 1) Test normality and perform correlation
result_EF <- correlation_test(
  df_final$zscore_SF, 
  df_final$zscore_EF,
  "Space Fortress (z-score)", 
  "Executive Functions (z-score)"
)

# 2) Create correlation label
cor_label_EF <- create_cor_label(result_EF)

# 3) Create plot
plot_EF <- ggplot(df_final, aes(x = zscore_EF, y = zscore_SF)) +
  geom_smooth(method = lm, color = "black", fill = "lightgray", se = TRUE, alpha = 0.3) +
  geom_point(alpha = 0.8, size = 2, color = "black", shape = 16) +
  annotate("text", x = -1, y = 2.5, label = cor_label_EF, hjust = 0, size = 3.5) +
  theme_pubr() +
  xlab("Executive Functions (z-score)") +
  ylab("Space Fortress (z-score)") +
  theme(plot.title = element_text(hjust = 0.1))

print(plot_EF)

# Save plot
output_file_EF <- file.path(figure_path, "Fig3_SF_EF.pdf")
ggsave(output_file_EF, plot = plot_EF, width = 5, height = 4, units = "in",
       device = cairo_pdf)
cat(sprintf("Plot saved to: %s\n\n", output_file_EF))

################################################################################
## ANALYSIS 2: SF & INHIBITION
################################################################################

print_section("ANALYSIS 2: Space Fortress & Inhibition")

# 1) Test normality and perform correlation
result_inhibition <- correlation_test(
  df_final$zscore_SF, 
  df_final$zscore_inhibition,
  "Space Fortress (z-score)", 
  "Inhibition (z-score)"
)

# 2) Create correlation label
cor_label_inhibition <- create_cor_label(result_inhibition)

# 3) Create plot
plot_inhibition <- ggplot(df_final, aes(x = zscore_inhibition, y = zscore_SF)) +
  geom_smooth(method = lm, color = "black", fill = "lightgray", se = TRUE, alpha = 0.3) +
  geom_point(alpha = 0.8, size = 2, color = "black", shape = 16) +
  annotate("text", x = -1.5, y = 2.5, label = cor_label_inhibition, hjust = 0, size = 3.5) +
  theme_pubr() +
  xlab("Inhibition (z-score)") +
  ylab("Space Fortress (z-score)") +
  theme(plot.title = element_text(hjust = 0.1))

print(plot_inhibition)

################################################################################
## ANALYSIS 3: SF & UPDATING
################################################################################

print_section("ANALYSIS 3: Space Fortress & Updating")

# 1) Test normality and perform correlation
result_updating <- correlation_test(
  df_final$zscore_SF, 
  df_final$zscore_WM,
  "Space Fortress (z-score)", 
  "Updating (z-score)"
)

# 2) Create correlation label
cor_label_updating <- create_cor_label(result_updating)

# 3) Create plot
plot_updating <- ggplot(df_final, aes(x = zscore_WM, y = zscore_SF)) +
  geom_smooth(method = lm, color = "black", fill = "lightgray", se = TRUE, alpha = 0.3) +
  geom_point(alpha = 0.8, size = 2, color = "black", shape = 16) +
  annotate("text", x = -1.5, y = 2.5, label = cor_label_updating, hjust = 0, size = 3.5) +
  theme_pubr() +
  xlab("Updating (z-score)") +
  ylab("Space Fortress (z-score)") +
  theme(plot.title = element_text(hjust = 0.1))

print(plot_updating)

################################################################################
## ANALYSIS 4: SF & SHIFTING 
################################################################################

print_section("ANALYSIS 4: Space Fortress & Shifting")

# 1) Test normality and perform correlation
result_shifting <- correlation_test(
  df_final$zscore_SF, 
  df_final$zscore_shifting,
  "Space Fortress (z-score)", 
  "Shifting (z-score)"
)

# 2) Create correlation label
cor_label_shifting <- create_cor_label(result_shifting)

# 3) Create plot
plot_shifting <- ggplot(df_final, aes(x = zscore_shifting, y = zscore_SF)) +
  geom_smooth(method = lm, color = "black", fill = "lightgray", se = TRUE, alpha = 0.3) +
  geom_point(alpha = 0.8, size = 2, color = "black", shape = 16) +
  annotate("text", x = -1.5, y = 2.5, label = cor_label_shifting, hjust = 0, size = 3.5) +
  theme_pubr() +
  xlab("Shifting (z-score)") +
  ylab("Space Fortress (z-score)") +
  theme(plot.title = element_text(hjust = 0.1))

print(plot_shifting)

################################################################################
## CREATE AND SAVE COMBINED EF SUBSCORES FIGURE
################################################################################

print_section("Creating Combined EF Subscores Figure")

# Combine all EF subscore plots into a 1x3 grid
combined_plot_subscores <- plot_grid(
  plot_inhibition, plot_updating, plot_shifting,
  ncol = 3, nrow = 1,
  labels = c("A", "B", "C"),
  label_size = 14
)

print(combined_plot_subscores)

# Save combined plot
output_file_subscores <- file.path(figure_path, "Fig4_SF_EFsubscores.pdf")
ggsave(output_file_subscores, plot = combined_plot_subscores, 
       width = 12, height = 4, units = "in",
       device = cairo_pdf)

cat(sprintf("Combined subscores plot saved to: %s\n\n", output_file_subscores))

################################################################################
## GENERATE FORMATTED TEXT FOR MANUSCRIPT
################################################################################

print_section("Formatted Results for Manuscript")

cat("Copy-paste the following text into your manuscript:\n\n")

# Create formatted correlation text for each EF component
text_inhibition <- format_correlation_text(result_inhibition, "inhibition")
text_updating <- format_correlation_text(result_updating, "updating")
text_shifting <- format_correlation_text(result_shifting, "shifting")

# Create manuscript paragraph
manuscript_text <- paste0(
  "The correlations between the EF sub-scores and SF score were ",
  text_updating, "; ",
  text_shifting, "; ",
  text_inhibition, ". ",
  "These results are presented in Figure 4."
)

cat(manuscript_text, "\n\n")
