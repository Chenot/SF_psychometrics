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

# Set working directory to script location
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  this_file <- rstudioapi::getSourceEditorContext()$path
  this_dir <- dirname(this_file)
  setwd(this_dir)
}

# Source utility functions
source("utils.R")

# Load required packages
required_packages <- c("ggplot2", "dplyr", "ggpubr", "e1071")
load_packages(required_packages)

# Path management
project_dir <- dirname(dirname(getwd()))

# Load data (non-z-scored for distribution analysis)
df_final <- load_data(project_dir, z_scored = FALSE)

# Create output directory
figure_path <- file.path(project_dir, "results", "figures")
dir.create(figure_path, recursive = TRUE, showWarnings = FALSE)

################################################################################
## CALCULATE DISTRIBUTION STATISTICS
################################################################################

# Descriptive statistics
n <- nrow(df_final)
mean_sf <- mean(df_final$SF, na.rm = TRUE)
sd_sf <- sd(df_final$SF, na.rm = TRUE)
min_sf <- min(df_final$SF, na.rm = TRUE)
max_sf <- max(df_final$SF, na.rm = TRUE)
median_sf <- median(df_final$SF, na.rm = TRUE)
q1_sf <- quantile(df_final$SF, 0.25, na.rm = TRUE)
q3_sf <- quantile(df_final$SF, 0.75, na.rm = TRUE)
iqr_sf <- IQR(df_final$SF, na.rm = TRUE)

# Distribution shape
skewness_val <- e1071::skewness(df_final$SF, type = 2)
kurtosis_val <- e1071::kurtosis(df_final$SF, type = 2)

# Normality test
df_final$SF_jittered <- df_final$SF + rnorm(length(df_final$SF), mean = 0, sd = 1e-10)
ks_test <- ks.test(df_final$SF_jittered, "pnorm", 
                   mean = mean(df_final$SF), 
                   sd = sd(df_final$SF))

################################################################################
## CREATE VISUALIZATION
################################################################################

# Density plot
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

# Q-Q plot
qq_plot <- ggqqplot(
  data = df_final, 
  x = "SF",
  color = "black",
  size = 1,
  alpha = 0.6,
  shape = 16,
  xlab = "Theoretical Quantiles", 
  ylab = "Sample Quantiles")

qq_plot$layers[[1]]$aes_params$colour <- "black"
qq_plot$layers[[1]]$aes_params$size <- 1.5
qq_plot$layers[[1]]$aes_params$alpha <- 0.6

# Combine plots
combined_plot <- ggarrange(
  density_plot, qq_plot, 
  ncol = 2, nrow = 1,
  labels = c("A", "B"),
  label.x = 0.1,
  label.y = 0.98
)

# Show plot
print(combined_plot)

# Save plot
output_file <- file.path(figure_path, "Fig2_SF_data_distribution.pdf")
ggsave(output_file, plot = combined_plot, width = 10, height = 4, units = "in",
       device = cairo_pdf)

################################################################################
## STORE RESULTS
################################################################################

# Create results list
distribution_results <- list(
  n = n,
  mean = mean_sf,
  sd = sd_sf,
  median = median_sf,
  min = min_sf,
  max = max_sf,
  q1 = q1_sf,
  q3 = q3_sf,
  iqr = iqr_sf,
  skewness = skewness_val,
  kurtosis = kurtosis_val,
  ks_statistic = ks_test$statistic,
  ks_pvalue = ks_test$p.value,
  plot_path = output_file
)

print(distribution_results)