## P06_SupplementaryMaterial_datatransformationstats
# Author: Quentin Chenot
# Date: 2025-05-05
# Description: This script combines results from executive functions tasks and demographics data into one dataframe.
# Dependencies: dplyr, rstudioapi
# Inputs: 
#   - Demographics data: 'data/participants_demographics.csv'
#   - Task data: 'results/combined_data/behavior/*.csv' (multiple files)
# Outputs: Combined dataframe saved as 'results/combined_data/data.csv'

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
data_path <- file.path(project_dir, "results" , "combined_data","behavior") # Define the relative path to the data and results

# Load data from demographics and executive functions tasks
df_demographics <- read.csv(file.path(project_dir,"data", "participants_demographics.csv"))
df_antisaccade <- read.csv(file.path(data_path, "antisaccade.csv"))
df_categoryswitch <- read.csv(file.path(data_path, "categoryswitch.csv"))
df_colorshape <- read.csv(file.path(data_path, "colorshape.csv"))
df_dualnback <- read.csv(file.path(data_path, "dualnback.csv"))
df_keeptrack <- read.csv(file.path(data_path, "KeepTrack.csv"))
df_lettermemory <- read.csv(file.path(data_path, "lettermemory.csv"))
df_stopsignal <- read.csv(file.path(data_path, "StopSignal.csv"))
df_stroop <- read.csv(file.path(data_path, "stroop.csv"))
df_numberletter <- read.csv(file.path(data_path, "numberletter.csv"))

# List of task dataframes and their names
task_dfs <- list(
  antisaccade = df_antisaccade,
  categoryswitch = df_categoryswitch,
  colorshape = df_colorshape,
  dualnback = df_dualnback,
  keeptrack = df_keeptrack,
  lettermemory = df_lettermemory,
  stopsignal = df_stopsignal,
  stroop = df_stroop,
  numberletter = df_numberletter
)

# Extract percentage_excluded_data for each task
excluded_stats <- lapply(names(task_dfs), function(task) {
  df <- task_dfs[[task]]
  if ("percentage_excluded_data" %in% names(df)) {
    perc <- df$percentage_excluded_data
    data.frame(
      Task = task,
      Mean = round(mean(perc, na.rm = TRUE), 2),
      Min = round(min(perc, na.rm = TRUE), 2),
      Max = round(max(perc, na.rm = TRUE), 2)
    )
  } else {
    data.frame(Task = task, Mean = NA, Min = NA, Max = NA)
  }
})

# Combine into one table
excluded_table <- do.call(rbind, excluded_stats)
print(excluded_table)







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
df_final <- df_final[df_final$Inclusion == 1, ]

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

