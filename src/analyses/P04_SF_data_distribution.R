## P04_SF_data_distribution.R
# Author: Quentin Chenot
# Date: 2025-11-06
# Description: This script analyzes the distribution of Space Fortress performance data
#              to assess normality and data quality.
#
# Research Question:
#   Is Space Fortress performance normally distributed in the sample?
#
# Methodology:
#   - Descriptive statistics (mean, SD, range)
#   - Skewness and kurtosis analysis
#   - Kolmogorov-Smirnov normality test (appropriate for n=145)
#   - Visual inspection: density plot and Q-Q plot
#
# Interpretation:
#   - Skewness: |skew| < 2 indicates acceptable symmetry
#   - Kurtosis: |kurt| < 7 indicates acceptable tailedness
#   - KS test: p > .05 indicates normal distribution
#
# Outputs:
#   - Statistical results printed to console
#   - Combined distribution plot: 'results/figures/Fig2_SF_data_distribution.pdf'

################################################################################
## SETUP
################################################################################

# Source utility functions
source("utils.R")

# Load required packages
required_packages <- c("ggplot2", "dplyr", "ggpubr", "e1071", "rstudioapi")
load_packages(required_packages)

# Path management
this_file <- rstudioapi::getSourceEditorContext()$path
this_dir <- dirname(this_file)
setwd(this_dir)
project_dir <- dirname(dirname(this_dir))

# Load data (non-z-scored for distribution analysis)
df_final <- load_data(project_dir, z_scored = FALSE)

# Create output directory
figure_path <- file.path(project_dir, "results", "figures")
dir.create(figure_path, recursive = TRUE, showWarnings = FALSE)

################################################################################
## DESCRIPTIVE STATISTICS
################################################################################

print_section("DESCRIPTIVE STATISTICS")

# Calculate basic statistics
n <- nrow(df_final)
mean_sf <- mean(df_final$SF, na.rm = TRUE)
sd_sf <- sd(df_final$SF, na.rm = TRUE)
min_sf <- min(df_final$SF, na.rm = TRUE)
max_sf <- max(df_final$SF, na.rm = TRUE)
median_sf <- median(df_final$SF, na.rm = TRUE)
q1_sf <- quantile(df_final$SF, 0.25, na.rm = TRUE)
q3_sf <- quantile(df_final$SF, 0.75, na.rm = TRUE)
iqr_sf <- IQR(df_final$SF, na.rm = TRUE)

cat("Space Fortress Performance Distribution:\n")
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("Sample size:       %d\n", n))
cat(sprintf("Mean:              %.2f\n", mean_sf))
cat(sprintf("Standard deviation: %.2f\n", sd_sf))
cat(sprintf("Median:            %.2f\n", median_sf))
cat(sprintf("Range:             [%.2f, %.2f]\n", min_sf, max_sf))
cat(sprintf("Q1 - Q3:           [%.2f, %.2f]\n", q1_sf, q3_sf))
cat(sprintf("IQR:               %.2f\n", iqr_sf))

################################################################################
## DISTRIBUTION SHAPE ANALYSIS
################################################################################

print_section("DISTRIBUTION SHAPE ANALYSIS")

# Calculate skewness and kurtosis (Type 2 for compatibility with SAS/SPSS)
skewness_val <- e1071::skewness(df_final$SF, type = 2)
kurtosis_val <- e1071::kurtosis(df_final$SF, type = 2)

cat("Measures of distribution shape:\n")
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("Skewness:          %.3f\n", skewness_val))
cat("  Interpretation:  ")
if (abs(skewness_val) < 0.5) {
  cat("Approximately symmetric\n")
} else if (abs(skewness_val) < 1) {
  cat("Moderately skewed\n")
} else if (abs(skewness_val) < 2) {
  cat("Highly skewed (but acceptable)\n")
} else {
  cat("Severely skewed\n")
}

if (skewness_val > 0) {
  cat("  Direction:       Positive skew (right tail longer)\n")
} else if (skewness_val < 0) {
  cat("  Direction:       Negative skew (left tail longer)\n")
} else {
  cat("  Direction:       Perfectly symmetric\n")
}

cat(sprintf("\nKurtosis:          %.3f\n", kurtosis_val))
cat("  Interpretation:  ")
if (abs(kurtosis_val) < 0.5) {
  cat("Approximately mesokurtic (normal)\n")
} else if (abs(kurtosis_val) < 2) {
  cat("Moderately non-normal tails\n")
} else if (abs(kurtosis_val) < 7) {
  cat("Highly non-normal tails (but acceptable)\n")
} else {
  cat("Severely non-normal tails\n")
}

if (kurtosis_val > 0) {
  cat("  Direction:       Leptokurtic (heavy tails, peaked)\n")
} else if (kurtosis_val < 0) {
  cat("  Direction:       Platykurtic (light tails, flat)\n")
} else {
  cat("  Direction:       Mesokurtic (normal tails)\n")
}

################################################################################
## NORMALITY TESTING
################################################################################

print_section("NORMALITY TESTING")

cat("Kolmogorov-Smirnov test for normality:\n")
cat(rep("-", 70), "\n", sep = "")

# Add small jitter to avoid ties warning
df_final$SF_jittered <- df_final$SF + rnorm(length(df_final$SF), mean = 0, sd = 1e-10)

# Perform KS test
ks_test <- ks.test(df_final$SF_jittered, "pnorm", 
                   mean = mean(df_final$SF), 
                   sd = sd(df_final$SF))

cat(sprintf("Test statistic D:  %.4f\n", ks_test$statistic))
cat(sprintf("p-value:           %s\n", format_p_value(ks_test$p.value)))
cat(sprintf("Sample size:       %d\n", n))

cat("\nInterpretation:\n")
if (ks_test$p.value > 0.05) {
  cat("  → Data are consistent with a normal distribution (p > .05)\n")
  cat("  → Parametric tests are appropriate\n")
} else {
  cat("  → Data significantly deviate from normality (p ≤ .05)\n")
  cat("  → Consider non-parametric tests or data transformation\n")
}

################################################################################
## VISUALIZATION
################################################################################

print_section("CREATING DISTRIBUTION PLOTS")

cat("Generating density plot and Q-Q plot...\n\n")

# 1) Density plot with rug
density_plot <- ggplot(data = df_final, aes(x = SF)) +
  geom_density(color = "black", fill = "lightgray", alpha = 0.7, linewidth = 1) +
  geom_rug(color = "black", alpha = 0.3, linewidth = 0.5) +
  geom_vline(xintercept = mean_sf, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = mean_sf + 50, y = Inf, 
           label = sprintf("Mean = %.1f", mean_sf), 
           hjust = 0, vjust = 1.5, color = "red", size = 3.5) +
  theme_pubr() +
  xlab("Space Fortress Score") +
  ylab("Density")

# 2) Q-Q plot
qq_plot <- ggqqplot(
  data = df_final, 
  x = "SF",
  color = "black",
  size = 1,
  alpha = 0.6,
  shape = 16,
  xlab = "Theoretical Quantiles", 
  ylab = "Sample Quantiles")

# Modify Q-Q plot appearance
qq_plot$layers[[1]]$aes_params$colour <- "black"
qq_plot$layers[[1]]$aes_params$size <- 1.5
qq_plot$layers[[1]]$aes_params$alpha <- 0.6

# 3) Combine plots
combined_plot <- ggarrange(
  density_plot, qq_plot, 
  ncol = 2, nrow = 1,
  labels = c("A", "B"),
  label.x = 0.1,
  label.y = 0.98
)

# Print combined plot
print(combined_plot)

# Save plot
output_file <- file.path(figure_path, "Fig2_SF_data_distribution.pdf")
ggsave(output_file, plot = combined_plot, width = 10, height = 4, units = "in",
       device = cairo_pdf)

cat(sprintf("Plot saved to: %s\n\n", output_file))

################################################################################
## FORMATTED OUTPUT FOR MANUSCRIPT
################################################################################

print_section("FORMATTED TEXT FOR MANUSCRIPT")

cat("Copy-paste the following into your manuscript:\n\n")
cat(rep("-", 70), "\n", sep = "")

manuscript_text <- sprintf(
  "Space Fortress scores ranged from %.0f to %.0f (M = %.1f, SD = %.1f, n = %d). The distribution showed %s (skewness = %.2f) and %s (kurtosis = %.2f). A Kolmogorov-Smirnov test indicated that the data %s (D = %.3f, %s), %s for parametric statistical analyses (see Figure 2).",
  min_sf, max_sf, mean_sf, sd_sf, n,
  ifelse(abs(skewness_val) < 0.5, "minimal skewness", 
         ifelse(abs(skewness_val) < 1, "moderate skewness", "high skewness")),
  skewness_val,
  ifelse(abs(kurtosis_val) < 0.5, "normal kurtosis", 
         ifelse(abs(kurtosis_val) < 2, "moderate kurtosis", "high kurtosis")),
  kurtosis_val,
  ifelse(ks_test$p.value > 0.05, "were normally distributed", "deviated from normality"),
  ks_test$statistic,
  format_p_value(ks_test$p.value),
  ifelse(ks_test$p.value > 0.05, "supporting the use of parametric tests", 
         "suggesting consideration of non-parametric approaches")
)

cat(strwrap(manuscript_text, width = 70), sep = "\n")
cat("\n", rep("-", 70), "\n\n", sep = "")
