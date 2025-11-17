## 5_SF_covariates.R
# Author: Quentin Chenot
# Date: 2025-11-06
# Description: This script analyzes relationships between Space Fortress performance and covariates
#
# Research Questions:
#   1. Is SF performance correlated with Education Level? (expected: positive)
#   2. Is SF performance correlated with Age? (expected: negative)
#   3. Is SF performance correlated with Video Game Experience? (expected: positive)
#   4. Do men have higher SF scores than women? (tested with t-test)
#
# Methodology:
#   - Normality testing using Kolmogorov-Smirnov test
#   - Pearson correlation (if both variables normal) or Spearman (if not)
#   - Independent samples t-test for sex differences (with Cohen's d effect size)
#
# Output:
#   Returns a list with all covariate analysis results

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
required_packages <- c("ggplot2", "ggpubr", "ggdist", "dplyr", "cowplot")
load_packages(required_packages)

# Path management
project_dir <- dirname(dirname(getwd()))

# Load data
df_final <- load_data(project_dir, z_scored = TRUE)

# Create output directory
figure_path <- file.path(project_dir, "results", "figures")
dir.create(figure_path, recursive = TRUE, showWarnings = FALSE)

################################################################################
## ANALYSIS 1: SF & VIDEO GAME EXPERIENCE
################################################################################

result_VGexp <- test_normality_and_correlate(
  df_final$zscore_SF, 
  df_final$VGexp,
  "Space Fortress (z-score)", 
  "Video Game Experience",
  print_results = FALSE
)

cor_label_VGexp <- create_cor_label(result_VGexp)

plot_VGexp <- ggplot(df_final, aes(x = VGexp, y = zscore_SF)) +
  geom_smooth(method = lm, color = "black", fill = "lightgray", se = TRUE, alpha = 0.3) +
  geom_point(alpha = 0.6, size = 2, color = "black", shape = 16) +
  annotate("text", x = 2, y = 2.5, label = cor_label_VGexp, hjust = 0, size = 3.5) +
  theme_pubr() +
  xlab("Video Game Experience (years)") +
  ylab("Space Fortress (z-score)") +
  theme(plot.title = element_text(hjust = 0.1))

################################################################################
## ANALYSIS 2: SF & EDUCATION LEVEL
################################################################################

result_EL <- test_normality_and_correlate(
  df_final$zscore_SF, 
  df_final$EducationLevel,
  "Space Fortress (z-score)", 
  "Education Level",
  print_results = FALSE
)

cor_label_EL <- create_cor_label(result_EL)

plot_EL <- ggplot(df_final, aes(x = EducationLevel, y = zscore_SF)) +
  geom_smooth(method = lm, color = "black", fill = "lightgray", se = TRUE, alpha = 0.3) +
  geom_point(alpha = 0.6, size = 2, color = "black", shape = 16) +
  annotate("text", x = 12, y = 2.5, label = cor_label_EL, hjust = 0, size = 3.5) +
  theme_pubr() +
  xlab("Education Level (years)") +
  ylab("Space Fortress (z-score)") +
  theme(plot.title = element_text(hjust = 0.1))

################################################################################
## ANALYSIS 3: SF & AGE
################################################################################

result_Age <- test_normality_and_correlate(
  df_final$zscore_SF, 
  df_final$Age,
  "Space Fortress (z-score)", 
  "Age",
  print_results = FALSE
)

cor_label_Age <- create_cor_label(result_Age)

plot_Age <- ggplot(df_final, aes(x = Age, y = zscore_SF)) +
  geom_smooth(method = lm, color = "black", fill = "lightgray", se = TRUE, alpha = 0.3) +
  geom_point(alpha = 0.6, size = 2, color = "black", shape = 16) +
  annotate("text", x = 22, y = 2.5, label = cor_label_Age, hjust = 0, size = 3.5) +
  theme_pubr() +
  xlab("Age (years)") +
  ylab("Space Fortress (z-score)") +
  theme(plot.title = element_text(hjust = 0.1))

################################################################################
## ANALYSIS 4: SF & SEX
################################################################################

# Perform t-test
t_test_result <- t.test(zscore_SF ~ Sex, data = df_final)

# Calculate descriptive statistics
mean_sd <- df_final %>%
  group_by(Sex) %>%
  summarise(
    n = n(),
    mean_zscore_SF = mean(zscore_SF, na.rm = TRUE),
    sd_zscore_SF = sd(zscore_SF, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate Cohen's d
cohens_d <- calculate_cohens_d(
  mean_sd$mean_zscore_SF[1], 
  mean_sd$mean_zscore_SF[2],
  mean_sd$sd_zscore_SF[1], 
  mean_sd$sd_zscore_SF[2]
)

test_label <- create_ttest_label(t_test_result, cohens_d)

# Create raincloud plot
df_final$Sex_numeric <- as.numeric(as.factor(df_final$Sex))

plot_Sex <- ggplot(df_final, aes(x = Sex, y = zscore_SF, fill = Sex, color = Sex)) +
  ggdist::stat_halfeye(
    width = 0.5,
    .width = 0,
    justification = -0.2,
    point_colour = NA,
    alpha = 0.5
  ) +
  geom_boxplot(
    width = 0.15,
    outlier.shape = NA,
    alpha = 0.5
  ) +
  geom_point(
    aes(x = Sex_numeric - 0.2),
    size = 1.5,
    alpha = 0.6,
    shape = 16,
    position = position_jitter(width = 0.1, height = 0, seed = 123)
  ) +
  annotate("text", x = Inf, y = Inf, label = test_label, 
           hjust = 1.1, vjust = 1.5, size = 3.5) +
  theme_pubr() +
  scale_x_discrete(labels = c("man" = "Men", "woman" = "Women")) +
  xlab("Sex") +
  ylab("Space Fortress (z-score)") +
  scale_color_manual(
    name = "Sex",
    labels = c("Men", "Women"),
    values = c("man" = "#003f5c", "woman" = "#ff8531")
  ) +
  scale_fill_manual(
    name = "Sex",
    labels = c("Men", "Women"),
    values = c("man" = "#003f5c", "woman" = "#ff8531")
  ) +
  theme(legend.position = "none")

################################################################################
## CREATE COMBINED FIGURE
################################################################################

combined_plot <- plot_grid(
  plot_VGexp, plot_EL, plot_Age, plot_Sex,
  ncol = 2, nrow = 2,
  labels = c("A", "B", "C", "D"),
  label_size = 14
)

# Save combined plot
print(combined_plot)
output_file <- file.path(figure_path, "Fig5_SF_covariates.pdf")
ggsave(output_file, plot = combined_plot, width = 10, height = 8, units = "in",
       device = cairo_pdf)

################################################################################
## STORE RESULTS
################################################################################

covariates_results <- list(
  # Video Game Experience
  VGexp_method = result_VGexp$method,
  VGexp_r = result_VGexp$cor_result$estimate,
  VGexp_ci_lower = result_VGexp$cor_result$conf.int[1],
  VGexp_ci_upper = result_VGexp$cor_result$conf.int[2],
  VGexp_pvalue = result_VGexp$cor_result$p.value,
  VGexp_n = result_VGexp$n,
  
  # Education Level
  EL_method = result_EL$method,
  EL_r = result_EL$cor_result$estimate,
  EL_ci_lower = result_EL$cor_result$conf.int[1],
  EL_ci_upper = result_EL$cor_result$conf.int[2],
  EL_pvalue = result_EL$cor_result$p.value,
  EL_n = result_EL$n,
  
  # Age
  Age_method = result_Age$method,
  Age_r = result_Age$cor_result$estimate,
  Age_ci_lower = result_Age$cor_result$conf.int[1],
  Age_ci_upper = result_Age$cor_result$conf.int[2],
  Age_pvalue = result_Age$cor_result$p.value,
  Age_n = result_Age$n,
  
  # Sex
  sex_ttest = t_test_result,
  sex_mean_sd = mean_sd,
  sex_cohens_d = cohens_d,
  sex_t = t_test_result$statistic,
  sex_df = t_test_result$parameter,
  sex_pvalue = t_test_result$p.value,
  sex_ci_lower = t_test_result$conf.int[1],
  sex_ci_upper = t_test_result$conf.int[2],
  
  # Figure path
  combined_plot_path = output_file
)

# Print confirmation
cat("Covariates analysis completed.\n")
cat(sprintf("Combined plot saved to: %s\n", output_file))