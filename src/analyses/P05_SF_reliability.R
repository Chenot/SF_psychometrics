## P05_SF_reliability.R
# Author: Quentin Chenot
# Date: 2025-05-05
# Description: This script analyzes Space Fortress performance reliability
# Dependencies: ggplot2, dplyr, tidyr, ggpubr, e1071, nortest, psych, rstudioapi
# Inputs: Combined dataframe saved in 'results/combined_data/data.csv'
# Outputs: 
#   - Printed reliability of SF (ICC)
#   - Printed table of ICCs (Latex)

## LOAD LIBRARIES
# Function to check if each required package is installed, and install it if not
required_packages <- c("ggplot2", "dplyr", "tidyr", "ggpubr", "e1071", "nortest", "psych", "rstudioapi", "irr")
install_if_not_present <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
  }
}
lapply(required_packages, install_if_not_present) # Apply the function to each required package

# Load the libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(e1071)
library(nortest)
library(psych)
library(rstudioapi)

## PATH MANAGEMENT
# Get the directory and path to this file
this_file <- rstudioapi::getSourceEditorContext()$path  # if using RStudio
this_dir <- dirname(this_file)
setwd(this_dir)
project_dir <- dirname(dirname(this_dir)) # Get the project directory

# Data & Figure paths
data_path <- file.path(project_dir, "data") # Define the relative path to the data files
df_final <- read.csv(file.path(project_dir,"results", "combined_data", "data.csv"))
df_final <- df_final[df_final$Inclusion == 1, ]
figure_path <- file.path(project_dir, "results" , "figures") # Define the relative path to the data and results


#################
## RELIABILITY ##
#################
# Select multi games columns
df_icc <- df_final[, c("SF_multi_01", "SF_multi_02", "SF_multi_03", "SF_multi_04", "SF_multi_05")]

# for a single measurement (model="twoway", type="agreement", unit="single")
icc_irr_single <- icc(df_icc, model = "twoway", type = "agreement", unit = "single")
print(icc_irr_single)

# for the average measurement (model="twoway", type="agreement", unit="single"), k=5
icc_irr_average <- icc(df_icc, model = "twoway", type = "agreement", unit = "average")
print(icc_irr_average)
