## _main_SF.R
# Author: Quentin Chenot
# Date: 2024-05-22
# Description: This script sources task-specific R scripts and runs them to process SF task
# Dependencies: Requires the following R packages - readr, dplyr, tidyr, stringr and RStudio API. 
# Inputs : raw behavioral data from the SF task stored in a '/data' directory
# Outputs : processed behavioral data of the SF task saved in a '/derived_data' directory

## LOAD LIBRARIES
# Function to check if each required package is installed, and install it if not
required_packages <- c("readr", "dplyr", "tidyr", "stringr", "rstudioapi") # List of packages required for this script
install_if_not_present <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
  }
}
lapply(required_packages, install_if_not_present) # Apply the function to each required package

# Load the libraries
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(rstudioapi)

## PATH MANAGEMENT
# Get the directory and path to this file
this_file <- rstudioapi::getSourceEditorContext()$path  # if using RStudio
this_dir <- dirname(this_file)
setwd(this_dir)
project_dir <- dirname(dirname(dirname(this_dir))) # Get the project directory
data_path <- file.path(project_dir, "data") # Define the relative path to the input data
results_path <- file.path(project_dir, "derived_data") # Define the relative path to the output data
result <- file.path(project_dir, "results", "combined_data", "behavior") # Define the relative path to the output data (summary)

# List all participant directories
participant_dirs <- list.dirs(path = data_path, recursive = FALSE, full.names = TRUE)

## CREATE SUMMARY FILE FOR EACH PARTICIPANT
# Function to process each participant
process_participant <- function(participant_dir) {
  # Extract participant ID
  participant_id <- basename(participant_dir)
  
  # Define the directory path for behavior files
  behavior_dir <- file.path(participant_dir, "ses-1", "behavior")
  
  # List all CSV files in the directory
  file_list <- list.files(path = behavior_dir, pattern = paste0(participant_id, "_SF_G\\d{2}\\.csv"), full.names = TRUE)
  
  # Initialize an empty data frame to store results
  final_results <- tibble()
  
  # Function to process each file and return a summarized table
  process_file <- function(file_path) {
    # Extract participant and game info from file name
    file_name <- basename(file_path)
    file_info <- str_match(file_name, "sub-(\\d+)_SF_G(\\d{2})\\.csv")
    participant <- sprintf("sub-%03d", as.integer(file_info[2]))
    game <- sprintf("G%s", file_info[3])
    
    # Read the first line to get the column names
    header <- read_lines(file_path, n_max = 1)
    column_names <- str_split(header, "\t")[[1]]
    
    # Prepend a placeholder for the unnamed first column
    column_names <- c("Timestamp", column_names)
    
    # Read the rest of the file
    data <- read_delim(file_path, delim = "\t", skip = 1, col_names = column_names, show_col_types = FALSE)
    
    # Calculate the sum of points for each type of Group
    sum_points_per_group <- data %>%
      filter(Group %in% c("Bonus", "Flight", "Fortress", "Mine")) %>%
      group_by(Group) %>%
      summarise(total_points = sum(Point, na.rm = TRUE)) %>%
      pivot_wider(names_from = Group, values_from = total_points, values_fill = list(total_points = NA))
    
    # Calculate the number of occurrences of Press in the Group column
    num_press <- data %>%
      filter(Group == "Press") %>%
      tally() %>%
      rename(Press = n)
    
    # Calculate total score
    total_points <- sum(data$Point, na.rm = TRUE)
    
    # Combine results into a new table
    result <- tibble(
      Participant = participant,
      Game = game,
      TotalScore = total_points
    ) %>%
      bind_cols(sum_points_per_group) %>%
      bind_cols(num_press)
    
    # Ensure all necessary columns are present
    necessary_columns <- c("Bonus", "Flight", "Fortress", "Mine", "Press")
    for (col in necessary_columns) {
      if (!col %in% names(result)) {
        result[[col]] <- NA
      }
    }
    
    # Arrange the columns in the desired order
    result <- result %>%
      select(Participant, Game, TotalScore, Bonus, Flight, Fortress, Mine, Press)
    
    return(result)
  }
  
  # Loop through each file and process it
  for (file in file_list) {
    result <- process_file(file)
    final_results <- bind_rows(final_results, result)
  }
  
  # Define the output directory and create it if it doesn't exist
  output_dir <- file.path(results_path, participant_id, "behavior")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Define the output file path
  output_file <- file.path(output_dir, paste0(participant_id, "_SF_summary.csv"))
  
  # Write the final results to a CSV file
  write_csv(final_results, output_file)
  
  # Return the final results for checking
  return(final_results)
}

# Process each participant
for (participant_dir in participant_dirs) {
  process_participant(participant_dir)
}

# Print a message indicating completion
cat("Processing complete.\n")



## CREATE SUMMARY FILE FOR ALL PARTICIPANTS
# List all summary CSV files in the derived_data directory
summary_files <- list.files(path = results_path, pattern = "sub-\\d{3}_SF_summary\\.csv", recursive = TRUE, full.names = TRUE)

# Initialize an empty data frame to store merged results
merged_results <- tibble()

# Function to read and bind each summary file
read_and_bind <- function(file_path) {
  # Read the summary file
  summary_data <- read_csv(file_path, show_col_types = FALSE)
  return(summary_data)
}

# Loop through each summary file and bind them into a common dataframe
for (file in summary_files) {
  summary_data <- read_and_bind(file)
  merged_results <- bind_rows(merged_results, summary_data)
}

# Add Type column based on Game
merged_results <- merged_results %>%
  mutate(Type = if_else(Game %in% c("G01", "G03", "G05", "G07", "G09"), "monotask", "multitask"))

# Save the merged results to a CSV file
output_file <- file.path(result, "SF.csv")
write_csv(merged_results, output_file)

# Print a message indicating completion
cat("The merged dataframe has been saved to", output_file, "\n")