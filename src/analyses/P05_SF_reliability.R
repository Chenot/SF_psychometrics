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

# Helper to extract numbers from irr::icc print output
extract_irr_stats <- function(icc_obj) {
  txt <- capture.output(print(icc_obj))
  # ICC value (first line that contains "ICC(")
  icc_line_idx <- grep("ICC\\(", txt)[1]
  icc_val <- sub(".*=\\s*([0-9eE.+-]+).*", "\\1", txt[icc_line_idx])

  # Subjects / Raters (optional)
  subj_idx <- grep("^\\s*Subjects\\s*=\\s*", txt)
  subjects <- if (length(subj_idx)) sub(".*=\\s*([0-9]+).*", "\\1", txt[subj_idx]) else NA
  raters_idx <- grep("^\\s*Raters\\s*=\\s*", txt)
  raters <- if (length(raters_idx)) sub(".*=\\s*([0-9]+).*", "\\1", txt[raters_idx]) else NA

  # F, df1, df2, p
  f_idx <- grep("^F\\(", txt)
  f_line <- if (length(f_idx)) txt[f_idx] else ""
  m <- regexec("F\\s*\\(\\s*([0-9.eE+-]+)\\s*,\\s*([0-9.eE+-]+)\\s*\\)\\s*=\\s*([0-9.eE+-]+)\\s*,\\s*p\\s*=\\s*([0-9eE.+-]+)", f_line)
  regs <- regmatches(f_line, m)
  if (length(regs) && length(regs[[1]]) >= 5) {
    df1 <- regs[[1]][2]; df2 <- regs[[1]][3]; Fval <- regs[[1]][4]; pval <- regs[[1]][5]
  } else {
    df1 <- df2 <- Fval <- pval <- NA
  }

  # 95% CI (next line after the CI header)
  ci_idx <- grep("95%-Confidence|95%.*Confidence", txt)
  ci_line <- if (length(ci_idx)) txt[ci_idx + 1] else ""
  m2 <- regexec("\\s*([0-9eE.+-]+)\\s*<\\s*ICC\\s*<\\s*([0-9eE.+-]+)", ci_line)
  regs2 <- regmatches(ci_line, m2)
  if (length(regs2) && length(regs2[[1]]) >= 3) {
    lower <- regs2[[1]][2]; upper <- regs2[[1]][3]
  } else {
    lower <- upper <- NA
  }

  list(icc = as.numeric(icc_val),
       lower = as.numeric(lower),
       upper = as.numeric(upper),
       F = as.numeric(Fval),
       df1 = as.numeric(df1),
       df2 = as.numeric(df2),
       p = pval,
       subjects = as.numeric(subjects),
       raters = as.numeric(raters))
}

s_single <- extract_irr_stats(icc_irr_single)
s_avg    <- extract_irr_stats(icc_irr_average)

# Build LaTeX table (string)
tex_lines <- c(
  "\\begin{table}[ht]",
  "\\centering",
  "\\caption{Within-session reliability of Space Fortress scores (two-way mixed, absolute agreement).}",
  "\\label{tab:icc_sf}",
  "\\begin{tabular}{lccccccc}",
  "\\toprule",
  "Measure & ICC & 95\\% CI (lower) & 95\\% CI (upper) & F & df1 & df2 & p \\\\",
  "\\midrule",
  sprintf("Single (ICC(A,1)) & %.3f & %.3f & %.3f & %.2f & %d & %s & %s \\\\",
          s_single$icc, s_single$lower, s_single$upper, s_single$F,
          ifelse(!is.na(s_single$df1), s_single$df1, 0),
          ifelse(!is.na(s_single$df2), format(s_single$df2, trim = TRUE), "NA"),
          ifelse(!is.na(s_single$p), s_single$p, "NA")),
  sprintf("Average (ICC(A,%d)) & %.3f & %.3f & %.3f & %.2f & %d & %s & %s \\\\",
          ifelse(!is.na(s_avg$raters), s_avg$raters, ncol(df_icc)),
          s_avg$icc, s_avg$lower, s_avg$upper, s_avg$F,
          ifelse(!is.na(s_avg$df1), s_avg$df1, 0),
          ifelse(!is.na(s_avg$df2), format(s_avg$df2, trim = TRUE), "NA"),
          ifelse(!is.na(s_avg$p), s_avg$p, "NA")),
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)

# Ensure output directory exists and save the LaTeX table
out_dir <- file.path(project_dir, "results", "tables")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_file <- file.path(out_dir, "SF_ICC_table.tex")
writeLines(tex_lines, out_file)

# Also print to console for quick copy/paste
cat(paste(tex_lines, collapse = "\n"), "\n")
