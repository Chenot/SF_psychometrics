## 7_aposteriori_power.R
# A posteriori (post-hoc) power calculations using the pwr package
# Inputs based on reported results in the manuscript (N = 145)
# - Pearson r = 0.50 (R2 = 0.25)
# - Multiple regression R2 = 0.467 (F(5,139) reported)
# - Independent t-test: d = 1.28 (t = 7.64); group sizes assumed ~73/72
# - ICC reported (single = 0.78, average = 0.95) -> approximate with r for illustration

# Purpose: compute achieved power for these effects with alpha = 0.05

## No file output requested; results will be printed to console only

# Load packages
if (!requireNamespace("pwr", quietly = TRUE)) {
  install.packages("pwr", repos = "https://cloud.r-project.org")
}
library(pwr)

# Basic parameters
N <- 145L
alpha <- 0.05

out_lines <- list()
append_line <- function(text) {
  out_lines <<- c(out_lines, text)
}

append_line(sprintf("A posteriori power analysis (alpha = %.3f)\nSample size: N = %d\n", alpha, N))

## 1) Pearson correlation (two-sided)
r_val <- 0.50
p_corr <- pwr.r.test(n = N, r = r_val, sig.level = alpha, alternative = "two.sided")
append_line("Pearson correlation test:")
append_line(sprintf("  r = %.3f, N = %d", r_val, N))
append_line(sprintf("  power = %.4f (computed by pwr.r.test)", p_corr$power))
append_line("")

## 2) Multiple regression (overall R2) using Cohen's f^2
R2 <- 0.467
u <- 5L   # numerator df = number of predictors
v <- N - u - 1L  # denominator df (residual df)
if (1 - R2 <= 0) {
  f2 <- Inf
} else {
  f2 <- R2 / (1 - R2)
}
p_reg <- pwr.f2.test(u = u, v = v, f2 = f2, sig.level = alpha)
append_line("Multiple regression (overall F) test:")
append_line(sprintf("  R2 = %.3f, f2 = %.3f, df1 = %d, df2 = %d, N = %d", R2, f2, u, v, N))
append_line(sprintf("  power = %.4f (computed by pwr.f2.test)", p_reg$power))
append_line("")

## 3) Independent two-sample t-test (Cohen's d)
d_val <- 1.28
n1 <- 80L # Men
n2 <- 65L # Women
# Use pwr.t2n.test for unequal group sizes
p_t_uneq <- pwr.t2n.test(n1 = n1, n2 = n2, d = d_val, sig.level = alpha, alternative = "two.sided")
# Also compute power for equal-group approximation (n per group = floor(N/2))
nequal <- floor(N / 2)
p_t_eq <- pwr.t.test(n = nequal, d = d_val, sig.level = alpha, type = "two.sample", alternative = "two.sided")
append_line("Independent two-sample t-test (Cohen's d):")
append_line(sprintf("  d = %.3f, assumed n1 = %d, n2 = %d", d_val, n1, n2))
append_line(sprintf("  power (unequal n, pwr.t2n.test) = %.4f", p_t_uneq$power))
append_line(sprintf("  power (equal-n approx, n per group = %d) = %.4f", nequal, p_t_eq$power))
append_line("")

## 4) ICC (approximate): pwr does not provide a dedicated ICC power test.
# For illustration only, approximate ICC as a correlation between measurements and compute power for that r.
icc_single <- 0.78
icc_average <- 0.95
p_icc_single <- pwr.r.test(n = N, r = icc_single, sig.level = alpha, alternative = "two.sided")
p_icc_average <- pwr.r.test(n = N, r = icc_average, sig.level = alpha, alternative = "two.sided")
append_line("ICC (approximate via correlation for illustration):")
append_line(sprintf("  ICC single = %.3f -> approximate power (pwr.r.test) = %.4f", icc_single, p_icc_single$power))
append_line(sprintf("  ICC average = %.3f -> approximate power (pwr.r.test) = %.4f", icc_average, p_icc_average$power))
append_line("")

## Print results to console
cat(paste(out_lines, collapse = "\n\n"), sep = "\n\n")
