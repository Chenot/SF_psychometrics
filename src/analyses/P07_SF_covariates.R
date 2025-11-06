## P07_SF_covariates.R
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
#   - Normality testing using Kolmogorov-Smirnov test (appropriate for n=145)
#   - Pearson correlation (if both variables normal) or Spearman (if not)
#   - Independent samples t-test for sex differences (with Cohen's d effect size)
#
# Outputs:
#   - Statistical results printed to console
#   - Individual plots for each covariate
#   - Combined grid plot saved as 'results/figures/Fig5_SF_covariates.pdf'

################################################################################
## SETUP
################################################################################

# Source utility functions
source("utils.R")

# Load required packages
required_packages <- c("ggplot2", "ggpubr", "ggdist", "dplyr", "cowplot", "rstudioapi")
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
## ANALYSIS 1: SF & VIDEO GAME EXPERIENCE
################################################################################

print_section("ANALYSIS 1: Space Fortress & Video Game Experience")

# 1) Test normality and perform correlation
result_VGexp <- correlation_test(
  df_final$zscore_SF, 
  df_final$VGexp,
  "Space Fortress (z-score)", 
  "Video Game Experience"
)

# 2) Create correlation label
cor_label_VGexp <- create_cor_label(result_VGexp)

# 3) Create plot
plot_VGexp <- ggplot(df_final, aes(x = VGexp, y = zscore_SF)) +
  geom_smooth(method = lm, color = "black", fill = "lightgray", se = TRUE, alpha = 0.3) +
  geom_point(alpha = 0.6, size = 2, color = "black", shape = 16) +
  annotate("text", x = 2, y = 2.5, label = cor_label_VGexp, hjust = 0, size = 3.5) +
  theme_pubr() +
  xlab("Video Game Experience (years)") +
  ylab("Space Fortress (z-score)") +
  theme(plot.title = element_text(hjust = 0.1))

print(plot_VGexp)

################################################################################
## ANALYSIS 2: SF & EDUCATION LEVEL
################################################################################

print_section("ANALYSIS 2: Space Fortress & Education Level")

# 1) Test normality and perform correlation
result_EL <- correlation_test(
  df_final$zscore_SF, 
  df_final$EducationLevel,
  "Space Fortress (z-score)", 
  "Education Level"
)

# 2) Create correlation label
cor_label_EL <- create_cor_label(result_EL)

# 3) Create plot
plot_EL <- ggplot(df_final, aes(x = EducationLevel, y = zscore_SF)) +
  geom_smooth(method = lm, color = "black", fill = "lightgray", se = TRUE, alpha = 0.3) +
  geom_point(alpha = 0.6, size = 2, color = "black", shape = 16) +
  annotate("text", x = 12, y = 2.5, label = cor_label_EL, hjust = 0, size = 3.5) +
  theme_pubr() +
  xlab("Education Level (years)") +
  ylab("Space Fortress (z-score)") +
  theme(plot.title = element_text(hjust = 0.1))

print(plot_EL)

################################################################################
## ANALYSIS 3: SF & AGE
################################################################################

print_section("ANALYSIS 3: Space Fortress & Age")

# 1) Test normality and perform correlation
result_Age <- correlation_test(
  df_final$zscore_SF, 
  df_final$Age,
  "Space Fortress (z-score)", 
  "Age"
)

# 2) Create correlation label
cor_label_Age <- create_cor_label(result_Age)

# 3) Create plot
plot_Age <- ggplot(df_final, aes(x = Age, y = zscore_SF)) +
  geom_smooth(method = lm, color = "black", fill = "lightgray", se = TRUE, alpha = 0.3) +
  geom_point(alpha = 0.6, size = 2, color = "black", shape = 16) +
  annotate("text", x = 22, y = 2.5, label = cor_label_Age, hjust = 0, size = 3.5) +
  theme_pubr() +
  xlab("Age (years)") +
  ylab("Space Fortress (z-score)") +
  theme(plot.title = element_text(hjust = 0.1))

print(plot_Age)

################################################################################
## ANALYSIS 4: SF & SEX
################################################################################

print_section("ANALYSIS 4: Space Fortress & Sex")

# 1) Perform t-test
cat("\nIndependent Samples t-test:\n")
cat(rep("-", 70), "\n", sep = "")

t_test_result <- t.test(zscore_SF ~ Sex, data = df_final)

# Calculate descriptive statistics
mean_sd <- df_final %>%
  group_by(Sex) %>%
  summarise(
    n = n(),
    mean_zscore_SF = mean(zscore_SF, na.rm = TRUE),
    sd_zscore_SF = sd(zscore_SF, na.rm = TRUE)
  )

# Print descriptive statistics
cat("\nDescriptive Statistics:\n")
print(mean_sd)

# Calculate Cohen's d
cohens_d <- calculate_cohens_d(
  mean_sd$mean_zscore_SF[1], 
  mean_sd$mean_zscore_SF[2],
  mean_sd$sd_zscore_SF[1], 
  mean_sd$sd_zscore_SF[2]
)

# Print t-test results
cat("\nt-test Results:\n")
cat(sprintf("  t(%d) = %.3f\n", t_test_result$parameter, t_test_result$statistic))
cat(sprintf("  %s\n", format_p_value(t_test_result$p.value)))
cat(sprintf("  Cohen's d = %.3f\n", abs(cohens_d)))
cat(sprintf("  95%% CI: [%.3f, %.3f]\n", 
            t_test_result$conf.int[1], t_test_result$conf.int[2]))

# Interpret effect size
if (abs(cohens_d) < 0.2) {
  cat("  → Small effect size\n")
} else if (abs(cohens_d) < 0.5) {
  cat("  → Small to medium effect size\n")
} else if (abs(cohens_d) < 0.8) {
  cat("  → Medium to large effect size\n")
} else {
  cat("  → Large effect size\n")
}

cat(rep("-", 70), "\n\n", sep = "")

# 2) Create test label
test_label <- create_ttest_label(t_test_result, cohens_d)

# 3) Create raincloud plot
# Prepare numeric position for jittered points
df_final$Sex_numeric <- as.numeric(as.factor(df_final$Sex))

plot_Sex <- ggplot(df_final, aes(x = Sex, y = zscore_SF, fill = Sex, color = Sex)) +
  # Half-violin (distribution cloud)
  ggdist::stat_halfeye(
    width = 0.5,
    .width = 0,
    justification = -0.2,
    point_colour = NA,
    alpha = 0.5
  ) +
  # Boxplot
  geom_boxplot(
    width = 0.15,
    outlier.shape = NA,
    alpha = 0.5
  ) +
  # Jittered points (rain)
  geom_point(
    aes(x = Sex_numeric - 0.2),
    size = 1.5,
    alpha = 0.6,
    shape = 16,
    position = position_jitter(width = 0.1, height = 0, seed = 123)
  ) +
  # Add test results
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

print(plot_Sex)

################################################################################
## CREATE AND SAVE COMBINED FIGURE
################################################################################

print_section("Creating Combined Figure")

# Combine all plots into a 2x2 grid
combined_plot <- plot_grid(
  plot_VGexp, plot_EL, plot_Age, plot_Sex,
  ncol = 2, nrow = 2,
  labels = c("A", "B", "C", "D"),
  label_size = 14
)

print(combined_plot)

# Save combined plot with proper encoding for special characters
output_file <- file.path(figure_path, "Fig5_SF_covariates.pdf")
ggsave(output_file, plot = combined_plot, width = 10, height = 8, units = "in",
       device = cairo_pdf) 

cat(sprintf("Combined plot saved to: %s\n", output_file))


