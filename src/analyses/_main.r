# Set working directory to script location
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  this_file <- rstudioapi::getSourceEditorContext()$path
  this_dir <- dirname(this_file)
  setwd(this_dir)
}

# Source all results silently
invisible(capture.output(source("0_get_results.R")))


show_section("demographics")
show_section("reliability")
show_section("concurrent_validity")
show_section("covariates")
show_section("regression")


