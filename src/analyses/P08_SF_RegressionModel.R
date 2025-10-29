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
model1 <- lm(formula = zscore_SF ~ zscore_EF + Age + EducationLevel + VGexp + Sex, data = df_final)
summary(model1)

# Convert the model summary to an xtable
model1_table <- xtable(summary(model1))

# Print the xtable with LaTeX formatting
print(model1_table, type = "latex", include.rownames = FALSE)