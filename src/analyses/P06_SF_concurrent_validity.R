## P06_SF_concurrent_validity.R
# Author: Quentin Chenot
# Date: 2025-10-29
# Description: This script tests hypotheses about relationships between Space Fortress performance and
#              executive functions.
# Dependencies: ggplot2, ggpubr, ggExtra, dplyr, cowplot, broom, xtable, reshape2, rstudioapi
# Inputs: 
#   - Processed dataframe with z-scores: 'results/combined_data/data_zscored.csv'
# Outputs:
#   - Correlation plots saved as:
#     * 'results/figures/SF_EF.pdf'
#     * 'results/figures/SF_EFsubscores.pdf'
#   - Printed correlation statistics

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

# Function for p value formating
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
############## Correlation between SF and EF composite score ###################
################################################################################
# Calculate correlation and format the p-value
cor_test <- cor.test(df_final$zscore_SF, df_final$zscore_EF)
r_value <- round(cor_test$estimate, 2)
r_value_formatted <- sub("^0\\.", ".", r_value)
r_squared <- round(r_value^2, 2)
r_squared_formatted <- sub("^0\\.", ".", r_squared)
cor_label <- paste("r =", r_value_formatted, ", R² =", r_squared_formatted, ",", format_p_value(cor_test$p.value))

# Plot
EF_SF <- ggplot(df_final, aes(y = zscore_SF, x = zscore_EF)) +
  geom_point() +
  geom_smooth(method = lm, color = "black", fill = "lightgray", se = TRUE) +
  annotate("text", x = -1, y = 2.5, label = cor_label, hjust = 0) +  # Add custom label
  theme_pubr() +
  xlab("Executive Functions (z-score)") +
  ylab("Space Fortress (z-score)") +
  theme(plot.title = element_text(hjust = 0.1))

# Save plot
filename <- paste0(figure_path, "/SF_EF.pdf")
ggsave(filename, plot = EF_SF, width = 5, height = 4, units = "in")


################################################################################
## Correlation between SF and EF sub-scores (inhibition, updating, switching) ##
################################################################################

# Updating
cor_test_updating <- cor.test(df_final$zscore_SF, df_final$zscore_WM)
r_value <- round(cor_test_updating$estimate, 2)
r_value_formatted <- sub("^0\\.", ".", r_value)
r_squared <- round(r_value^2, 2)
r_squared_formatted <- sub("^0\\.", ".", r_squared)
cor_label <- paste("r =", r_value_formatted, ", R² =", r_squared_formatted, ",", format_p_value(cor_test_updating$p.value))

#Plot
SF_updating <- ggplot(df_final, aes(y=zscore_SF, x=zscore_WM))+
  geom_point()+
  geom_smooth(method=lm , color="black", fill="lightgray", se=TRUE) +
  annotate("text", x = -1.5, y = 2.5, label = cor_label, hjust = 0) +  # Add custom label
  theme_pubr()+
  xlab("Updating (z-score)")+
  ylab("Space Fortress (z-score)")+
  theme(plot.title = element_text(hjust = 0.1))

# Switching
cor_test_shifting <- cor.test(df_final$zscore_SF, df_final$zscore_shifting)
r_value <- round(cor_test_shifting$estimate, 2)
r_value_formatted <- sub("^0\\.", ".", r_value)
r_squared <- round(r_value^2, 2)
r_squared_formatted <- sub("^0\\.", ".", r_squared)
cor_label <- paste("r =", r_value_formatted, ", R² =", r_squared_formatted, ",", format_p_value(cor_test_shifting$p.value))

# Plot
SF_shifting <- ggplot(df_final, aes(y=zscore_SF, x=zscore_shifting))+
  geom_point()+
  geom_smooth(method=lm , color="black", fill="lightgray", se=TRUE) +
  annotate("text", x = -1.5, y = 2.5, label = cor_label, hjust = 0) +  # Add custom label
  theme_pubr()+
  xlab("Shifting (z-score)")+
  ylab("Space Fortress (z-score)")+
  theme(plot.title = element_text(hjust = 0.1))

# Inhibition
cor_test_inhibition <- cor.test(df_final$zscore_SF, df_final$zscore_inhibition)
r_value <- round(cor_test_inhibition$estimate, 2)
r_value_formatted <- sub("^0\\.", ".", r_value)
r_squared <- round(r_value^2, 2)
r_squared_formatted <- sub("^0\\.", ".", r_squared)
cor_label <- paste("r =", r_value_formatted, ", R² =", r_squared_formatted, ",", format_p_value(cor_test_inhibition$p.value))

#Plot
SF_inhibition <- ggplot(df_final, aes(y=zscore_SF, x=zscore_inhibition))+
  geom_point()+
  geom_smooth(method=lm , color="black", fill="lightgray", se=TRUE) +
  annotate("text", x = -1.5, y = 2.5, label = cor_label, hjust = 0) +  # Add custom label
  theme_pubr()+
  xlab("Inhibition (z-score)")+
  ylab("Space Fortress (z-score)")+
  theme(plot.title = element_text(hjust = 0.1))

# Correlations results
cor_test_updating
cor_test_shifting
cor_test_inhibition

# Plot GRID with EF sub-scores
EFsubscores_SF <- plot_grid(SF_updating,SF_shifting,SF_inhibition, ncol = 3, nrow = 1)

# Save plot
filename <- paste0(figure_path, "/SF_EFsubscores.pdf")
ggsave(filename, plot = EFsubscores_SF, width = 12, height = 4, units = "in")


# Function to format p-values correctly, ensuring correct display for very small values
format_p_value2 <- function(p) {
  if (p < 0.001) {
    "$<$ .001"
  } else {
    sprintf("= %.3f", p)  # Keeping three decimal places for consistency
  }
}

# Function to calculate and format the 95% CI of a correlation coefficient
format_ci <- function(cor_object) {
  se <- sqrt((1 - cor_object$estimate^2) / (cor_object$parameter))
  lower <- cor_object$estimate - qt(0.975, cor_object$parameter) * se
  upper <- cor_object$estimate + qt(0.975, cor_object$parameter) * se
  paste("[", round(lower, 2), ",", round(upper, 2), "]", sep = "")
}

# Format correlation results into a string
format_correlation <- function(cor_test, hypothesis) {
  r_value <- cor_test$estimate
  p_value <- cor_test$p.value
  r_squared <- r_value^2
  ci <- format_ci(cor_test)
  formatted_string <- paste("r =", sub("^0\\.", ".", format(r_value, digits = 2)),
                            "(95\\% CI", ci, ", p", format_p_value2(p_value), 
                            ", R² =", sub("^0\\.", ".", format(round(r_squared, 3), digits = 3)),
                            ") for the", hypothesis)
  formatted_string
}

# Calculate formatted strings
updating_result <- format_correlation(cor_test_updating, "updating")
shifting_result <- format_correlation(cor_test_shifting, "shifting")
inhibition_result <- format_correlation(cor_test_inhibition, "inhibition")

# Create the paragraph
paragraph <- paste("The correlations between the EF sub-scores and SF score were",
                   updating_result, ";",
                   shifting_result, ";",
                   inhibition_result, ". These results are presented in the Figure \\ref{fig:EFsSFcorr}.")

# Print the corrected paragraph
print(paragraph)
