## P02_behavioral_data_transformation.R
# Author: Quentin Chenot
# Date: 2025-05-05
# Description: This script performs various transformations on combined data, calculates z-scores, and saves the processed dataframe.
# Dependencies: dplyr, rstudioapi
# Inputs: Combined dataframe saved in 'results/combined_data/data.csv'
# Outputs: Processed dataframe with z-scores saved as 'results/combined_data/data_zscored.csv'

## LOAD LIBRARIES
# Function to check if each required package is installed, and install it if not
required_packages <- c("dplyr", "rstudioapi") # List of packages required for this script
install_if_not_present <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
  }
}
lapply(required_packages, install_if_not_present) # Apply the function to each required package

# Load the libraries
library(dplyr)
library(rstudioapi)


## PATH MANAGEMENT
# Get the directory and path to this file
this_file <- rstudioapi::getSourceEditorContext()$path  # if using RStudio
this_dir <- dirname(this_file)
setwd(this_dir)
project_dir <- dirname(dirname(this_dir)) # Get the project directory
data_path <- file.path(project_dir, "results" , "combined_data") # Define the relative path to your data and results

# Load data
file_path <- file.path(data_path, "data.csv")
df_final <- read.csv(file_path)

# Arcsine transform the data
df_final$keeptrack_asin <- asin(sqrt(df_final$keeptrack / max(df_final$keeptrack)))
df_final$dualnback_asin <- asin(sqrt(df_final$dualnback / max(df_final$dualnback)))
df_final$lettermemory_asin <- asin(sqrt(df_final$lettermemory / max(df_final$lettermemory)))

# Compute the z-score
df_final <- df_final %>%
  mutate(zscore_antisaccade = as.vector(scale(-antisaccade)),
         zscore_categoryswitch = as.vector(scale(-categoryswitch)),
         zscore_colorshape = as.vector(scale(-colorshape)),
         zscore_dualnback = as.vector(scale(dualnback)),
         zscore_keeptrack = as.vector(scale(keeptrack)),
         zscore_lettermemory = as.vector(scale(lettermemory)),
         zscore_stopsignal = as.vector(scale(-stopsignal)),
         zscore_stroop = as.vector(scale(-stroop)),
         zscore_numberletter = as.vector(scale(-numberletter)),
         zscore_SF = as.vector(scale(SF)))

# Summarise how many z-scores lie beyond ±3 SD before clamping
zscore_cols <- grep("^zscore_", names(df_final), value = TRUE)

clamp_stats <- data.frame(
  variable = zscore_cols,
  perc_below_3sd = sapply(df_final[zscore_cols], function(x) mean(x < -3, na.rm = TRUE) * 100),
  perc_above_3sd = sapply(df_final[zscore_cols], function(x) mean(x > 3, na.rm = TRUE) * 100)
)
clamp_stats$perc_total <- clamp_stats$perc_below_3sd + clamp_stats$perc_above_3sd
print(clamp_stats)

# Clamp z-scores to the ±3 SD bounds
df_final <- df_final %>%
  mutate(across(all_of(zscore_cols), ~pmax(pmin(.x, 3), -3)))

# Calculate mean z-score
df_final$zscore_inhibition <- rowMeans(subset(df_final, select = c(zscore_stroop, zscore_stopsignal, zscore_antisaccade)), na.rm = TRUE)
df_final$zscore_WM <- rowMeans(subset(df_final, select = c(zscore_keeptrack, zscore_lettermemory, zscore_dualnback)), na.rm = TRUE)
df_final$zscore_shifting <- rowMeans(subset(df_final, select = c(zscore_numberletter, zscore_categoryswitch, zscore_colorshape)), na.rm = TRUE)
df_final$zscore_EF <- rowMeans(subset(df_final, select = c(
  zscore_stroop,
  zscore_stopsignal,
  zscore_antisaccade,
  zscore_keeptrack,
  zscore_lettermemory,
  zscore_dualnback,
  zscore_numberletter,
  zscore_categoryswitch,
  zscore_colorshape)),
  na.rm = TRUE)


# Save the final dataframe into a CSV file
results_file <- file.path(data_path, "data_zscored.csv")
write.csv(df_final, results_file, row.names = FALSE)



