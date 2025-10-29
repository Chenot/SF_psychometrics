## P07_SF_covariates.R
# Author: Quentin Chenot
# Date: 2025-10-29
# Description: This script tests hypotheses about relationships between Space Fortress performance
#              and covariates
# Dependencies: ggplot2, ggpubr, ggExtra, dplyr, cowplot, broom, xtable, reshape2, rstudioapi
# Inputs: 
#   - Processed dataframe with z-scores: 'results/combined_data/data_zscored.csv'
# Outputs:
#   - Correlation plots saved as:
#     * 'results/figures/SF_demographics.pdf'
#     * 'results/figures/SF_sex.pdf'
#   - Printed correlation statistics and regression model results

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

# p value formating
format_p_value <- function(p_value) {
  if (p_value < .001) {
    "p < .001"
  } else {
    formatted_p = sprintf("%.3f", p_value)  # Format with three decimal places
    formatted_p = sub("^0\\.", ".", formatted_p)  # Remove leading zero for numbers less than 1
    paste("p =", formatted_p)
  }
}

################################################################################
## SF and demographics:
## positively with Education level;
## negatively with age
## positively with video game experience
## Men will have a higher score than women. We tested this with a mixed-effect model.

# SF & EL (education level)
cor_test_EL <- cor.test(df_final$zscore_SF, df_final$EducationLevel)
r_value <- round(cor_test_EL$estimate, 2)
r_value_formatted <- sub("^0\\.", ".", r_value)
r_squared <- round(r_value^2, 2)
r_squared_formatted <- sub("^0\\.", ".", r_squared)
cor_label <- paste("r =", r_value_formatted, ", R² =", r_squared_formatted, ",", format_p_value(cor_test_EL$p.value))

# Plot
EL_SF <- ggplot(df_final, aes(x=EducationLevel, y= zscore_SF))+
  geom_point()+
  geom_smooth(method=lm , color="black", fill="lightgray", se=TRUE) +
  annotate("text", x = 12, y = 2.5, label = cor_label, hjust = 0) +  # Add custom label
  theme_pubr()+
  xlab("Education Level")+
  ylab("Space Fortress (z-score)")+
  theme(plot.title = element_text(hjust = 0.1))

# SF & Age
cor_test_Age <- cor.test(df_final$zscore_SF, df_final$Age)
r_value <- round(cor_test_Age$estimate, 2)
r_value_formatted <- sub("^0\\.", ".", r_value)
r_squared <- round(r_value^2, 2)
r_squared_formatted <- sub("^0\\.", ".", r_squared)
cor_label <- paste("r =", r_value_formatted, ", R² =", r_squared_formatted, ",", format_p_value(cor_test_Age$p.value))

# Plot
age_SF <- ggplot(df_final, aes(x=Age, y=zscore_SF))+
  geom_point()+
  geom_smooth(method=lm , color="black", fill="lightgray", se=TRUE) +
  annotate("text", x = 22, y = 2.5, label = cor_label, hjust = 0) +  # Add custom label
  theme_pubr()+
  xlab("Age")+
  ylab("Space Fortress (z-score)")+
  theme(plot.title = element_text(hjust = 0.1))

# SF & VGexp
cor_test_VGexp <- cor.test(df_final$zscore_SF, df_final$VGexp)
r_value <- round(cor_test_VGexp$estimate, 2)
r_value_formatted <- sub("^0\\.", ".", r_value)
r_squared <- round(r_value^2, 2)
r_squared_formatted <- sub("^0\\.", ".", r_squared)
cor_label <- paste("r =", r_value_formatted, ", R² =", r_squared_formatted, ",", format_p_value(cor_test_VGexp$p.value))

# Plot
VG_SF <- ggplot(df_final, aes(x=VGexp, y=zscore_SF))+
  geom_point()+
  geom_smooth(method=lm , color="black", fill="lightgray", se=TRUE) +
  annotate("text", x = 2, y = 2.5, label = cor_label, hjust = 0) +  # Add custom label
  theme_pubr()+
  xlab("Video Game Experience")+
  ylab("Space Fortress (z-score)")+
  theme(plot.title = element_text(hjust = 0.1))

# Plot GRID with demographics
Demographics_SF <- plot_grid(VG_SF, EL_SF, age_SF, ncol = 3, nrow = 1)

# Save plot
filename <- paste0(figure_path, "/SF_demographics.pdf")
ggsave(filename, plot = Demographics_SF, width = 12, height = 4, units = "in")

# SF & Sex
t_test_result <- t.test(zscore_SF ~ Sex, data = df_final) # Perform the Student's t-test
mean_sd <- df_final %>%       # Calculate means and standard deviations for each group
  group_by(Sex) %>%
  summarise(
    mean_zscore_SF = mean(zscore_SF, na.rm = TRUE),
    sd_zscore_SF = sd(zscore_SF, na.rm = TRUE)
  )

# Print the results
print(t_test_result)
print(mean_sd)

# Creating the plot
SF_sex <- ggplot(df_final, aes(x=zscore_EF, y=zscore_SF, color=Sex, fill=Sex)) +  # Add fill aesthetic
  geom_point() +
  geom_smooth(method="lm", se=TRUE, alpha=0.2) +  # Add transparency to fill
  theme_pubr() +
  xlab("Executive Functions (z-score)") +
  ylab("Space Fortress (z-score)") +
  theme(plot.title = element_text(hjust = 0.1)) +
  scale_color_manual(name = "Sex", 
                     labels = c("Men", "Women"),
                     values = c("man" = "#003f5c", "woman" = "#ff8531")) +
  scale_fill_manual(name = "Sex",   # Add fill scale
                    labels = c("Men", "Women"),
                    values = c("man" = "#003f5c", "woman" = "#ff8531"))

# Adding marginal histograms
SF_sex <- ggMarginal(SF_sex, groupColour = TRUE, groupFill = TRUE)
SF_sex

# Save plot
filename <- paste0(figure_path, "/SF_sex.pdf")
ggsave(filename, plot = SF_sex, width = 6, height = 6, units = "in")

# Function to calculate 95% CI for correlation coefficient
correlation_ci <- function(r, n, conf_level = 0.95) {
  z <- atanh(r)  # Fisher transformation
  se <- 1 / sqrt(n - 3)
  alpha <- 1 - conf_level
  z_crit <- qnorm(1 - alpha / 2)
  ci_lower <- tanh(z - z_crit * se)
  ci_upper <- tanh(z + z_crit * se)
  return(c(ci_lower, ci_upper))
}

# Calculate correlation results with correct CI for correlation coefficient
results <- df_final %>%
  group_by(Sex) %>%
  do({
    cor_test <- cor.test(.$zscore_SF, .$zscore_EF)
    lm_test <- lm(zscore_EF ~ zscore_SF, data = .)
    r_squared <- summary(lm_test)$r.squared
    
    # Calculate 95% CI for correlation coefficient
    ci <- correlation_ci(cor_test$estimate, nrow(.))
    
    # Returning a combined data frame
    data.frame(
      Sex = unique(.$Sex),
      r_value = cor_test$estimate,
      p_value = cor_test$p.value,
      r_squared = r_squared,
      CI_lower = ci[1],
      CI_upper = ci[2]
    )
  }) %>%
  ungroup()

# View results
print(results)
