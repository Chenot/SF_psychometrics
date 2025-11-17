## 0_get_results.R
# Author: Quentin Chenot
# Date: 2025-11-06
# Description: Simple script to display formatted manuscript text from all analyses.
#              This script sources 0_all_results.R (which runs all analyses) and then
#              provides easy access to formatted text for each section.
#
# Usage:
#   Simply run this script to see all formatted text, or access individual sections:
#   - demographics_results
#   - distribution_results  
#   - reliability_results
#   - concurrent_validity_results
#   - covariates_results
#   - regression_results

################################################################################
## SETUP
################################################################################

# Set working directory to script location
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  this_file <- rstudioapi::getSourceEditorContext()$path
  this_dir <- dirname(this_file)
  setwd(this_dir)
}

# Source all analyses (this runs everything and stores formatted_results)
cat("Running all analyses...\n")
cat(rep("=", 80), "\n\n", sep = "")

source("0_all_results.R")

################################################################################
## CREATE EASY ACCESS VARIABLES
################################################################################

# Extract formatted text for each section
demographics_results <- formatted_results$demographics
distribution_results <- formatted_results$distribution
reliability_results <- formatted_results$reliability
concurrent_validity_results <- formatted_results$concurrent_validity
covariates_results <- formatted_results$covariates
regression_results <- formatted_results$regression

################################################################################
## DISPLAY ALL FORMATTED TEXT
################################################################################

print_section <- function(title) {
  cat("\n")
  cat(rep("=", 80), "\n", sep = "")
  cat("  ", title, "\n", sep = "")
  cat(rep("=", 80), "\n\n", sep = "")
}

print_section("FORMATTED MANUSCRIPT TEXT - ALL SECTIONS")

cat("=== DEMOGRAPHICS ===\n\n")
cat(strwrap(demographics_results, width = 70), sep = "\n")
cat("\n\n")

cat("=== SPACE FORTRESS DISTRIBUTION ===\n\n")
cat(strwrap(distribution_results, width = 70), sep = "\n")
cat("\n\n")

cat("=== RELIABILITY ===\n\n")
cat(strwrap(reliability_results, width = 70), sep = "\n")
cat("\n\n")

cat("=== CONCURRENT VALIDITY ===\n\n")
cat(strwrap(concurrent_validity_results, width = 70), sep = "\n")
cat("\n\n")

cat("=== SUPPLEMENTARY: COVARIATES ===\n\n")
cat(strwrap(covariates_results, width = 70), sep = "\n")
cat("\n\n")

cat("=== SUPPLEMENTARY: REGRESSION MODEL ===\n\n")
cat(strwrap(regression_results, width = 70), sep = "\n")
cat("\n\n")

cat(rep("=", 80), "\n", sep = "")
cat("All formatted text displayed above.\n")
cat("Access individual sections using:\n")
cat("  - demographics_results\n")
cat("  - distribution_results\n")
cat("  - reliability_results\n")
cat("  - concurrent_validity_results\n")
cat("  - covariates_results\n")
cat("  - regression_results\n")
cat(rep("=", 80), "\n\n", sep = "")

################################################################################
## HELPER FUNCTIONS
################################################################################

#' Display a specific section
#' 
#' @param section Name of section: "demographics", "distribution", "reliability",
#'                "concurrent_validity", "covariates", or "regression"
#' @examples
#' show_section("demographics")
#' show_section("reliability")
show_section <- function(section) {
  valid_sections <- c("demographics", "distribution", "reliability", 
                      "concurrent_validity", "covariates", "regression")
  
  if (!section %in% valid_sections) {
    stop(sprintf("Invalid section. Choose from: %s", 
                 paste(valid_sections, collapse = ", ")))
  }
  
  section_titles <- c(
    demographics = "DEMOGRAPHICS",
    distribution = "SPACE FORTRESS DISTRIBUTION",
    reliability = "RELIABILITY",
    concurrent_validity = "CONCURRENT VALIDITY",
    covariates = "SUPPLEMENTARY: COVARIATES",
    regression = "SUPPLEMENTARY: REGRESSION MODEL"
  )
  
  cat("\n")
  cat(rep("=", 80), "\n", sep = "")
  cat("  ", section_titles[section], "\n", sep = "")
  cat(rep("=", 80), "\n\n", sep = "")
  cat(strwrap(formatted_results[[section]], width = 70), sep = "\n")
  cat("\n\n")
}

#' Copy a section to clipboard (Windows only)
#' 
#' @param section Name of section to copy
#' @examples
#' copy_section("demographics")
copy_section <- function(section) {
  valid_sections <- c("demographics", "distribution", "reliability", 
                      "concurrent_validity", "covariates", "regression")
  
  if (!section %in% valid_sections) {
    stop(sprintf("Invalid section. Choose from: %s", 
                 paste(valid_sections, collapse = ", ")))
  }
  
  text <- formatted_results[[section]]
  
  # Try to copy to clipboard
  if (.Platform$OS.type == "windows") {
    writeClipboard(text)
    cat(sprintf("'%s' section copied to clipboard!\n", section))
  } else {
    cat("Clipboard function only works on Windows.\n")
    cat("Here's the text:\n\n")
    cat(strwrap(text, width = 70), sep = "\n")
    cat("\n")
  }
}

#' Save all formatted text to a file
#' 
#' @param filename Output file path (default: "formatted_results.txt")
#' @examples
#' save_formatted_text()
#' save_formatted_text("my_results.txt")
save_formatted_text <- function(filename = "formatted_results.txt") {
  output_path <- file.path(dirname(dirname(getwd())), "results", filename)
  
  sink(output_path)
  
  cat("FORMATTED MANUSCRIPT TEXT - ALL SECTIONS\n")
  cat(rep("=", 80), "\n\n", sep = "")
  
  cat("=== DEMOGRAPHICS ===\n\n")
  cat(strwrap(demographics_results, width = 70), sep = "\n")
  cat("\n\n")
  
  cat("=== SPACE FORTRESS DISTRIBUTION ===\n\n")
  cat(strwrap(distribution_results, width = 70), sep = "\n")
  cat("\n\n")
  
  cat("=== RELIABILITY ===\n\n")
  cat(strwrap(reliability_results, width = 70), sep = "\n")
  cat("\n\n")
  
  cat("=== CONCURRENT VALIDITY ===\n\n")
  cat(strwrap(concurrent_validity_results, width = 70), sep = "\n")
  cat("\n\n")
  
  cat("=== SUPPLEMENTARY: COVARIATES ===\n\n")
  cat(strwrap(covariates_results, width = 70), sep = "\n")
  cat("\n\n")
  
  cat("=== SUPPLEMENTARY: REGRESSION MODEL ===\n\n")
  cat(strwrap(regression_results, width = 70), sep = "\n")
  cat("\n\n")
  
  sink()
  
  cat(sprintf("\nFormatted text saved to: %s\n", output_path))
}

cat("\n=== HELPER FUNCTIONS AVAILABLE ===\n")
cat("  - show_section('section_name')  : Display a specific section\n")
cat("  - copy_section('section_name')  : Copy section to clipboard (Windows)\n")
cat("  - save_formatted_text()         : Save all text to file\n\n")