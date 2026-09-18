## 7_SupplementaryMaterial.R
# Author: Quentin Chenot
# Date: 2025-11-18
# Description: Generates supplementary material outputs (task distributions, exclusion stats, EF by sex).
#              Returns a list containing figures, descriptive tables, and inferential results.
# Dependencies: utils.R (load_data, load_packages, print_section)

################################################################################
## SETUP
################################################################################

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  this_file <- rstudioapi::getSourceEditorContext()$path
  this_dir <- dirname(this_file)
  setwd(this_dir)
}

source("utils.R")

required_packages <- c(
  "dplyr", "ggplot2", "ggpubr", "cowplot", "gridExtra",
  "ggdist", "rstudioapi", "e1071", "stats", "xtable", "nortest"
)
load_packages(required_packages)

project_dir <- dirname(dirname(getwd()))
figure_path <- file.path(project_dir, "results", "figures")
dir.create(figure_path, recursive = TRUE, showWarnings = FALSE)

data_combined_path <- file.path(project_dir, "results", "combined_data")
behavior_path <- file.path(data_combined_path, "behavior")

df_raw <- load_data(project_dir, z_scored = FALSE)
df_z <- load_data(project_dir, z_scored = TRUE)

################################################################################
## EXCLUDED DATA PERCENTAGE
################################################################################

# Load data from demographics and executive functions tasks
df_demographics <- read.csv(file.path(project_dir,"data", "participants_demographics.csv"))
df_antisaccade <- read.csv(file.path(behavior_path, "antisaccade.csv"))
df_categoryswitch <- read.csv(file.path(behavior_path, "categoryswitch.csv"))
df_colorshape <- read.csv(file.path(behavior_path, "colorshape.csv"))
df_dualnback <- read.csv(file.path(behavior_path, "dualnback.csv"))
df_keeptrack <- read.csv(file.path(behavior_path, "KeepTrack.csv"))
df_lettermemory <- read.csv(file.path(behavior_path, "lettermemory.csv"))
df_stopsignal <- read.csv(file.path(behavior_path, "StopSignal.csv"))
df_stroop <- read.csv(file.path(behavior_path, "stroop.csv"))
df_numberletter <- read.csv(file.path(behavior_path, "numberletter.csv"))

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

################################################################################
## EXCLUDED DATA PERCENTAGE
################################################################################

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


################################################################################
## EXECUTIVE FUNCTION DISTRIBUTION PLOTS
################################################################################

create_hist_density <- function(data, column, title, xlab, binwidth, fill_hex) {
  ggplot(data, aes(x = .data[[column]])) +
    geom_histogram(aes(y = ..density..), binwidth = binwidth,
                   colour = "black", fill = fill_hex, alpha = 0.5) +
    geom_density() +
    geom_rug() +
    labs(title = title, x = xlab, y = "Density") +
    theme_pubr() +
    theme(plot.title = element_text(hjust = 0.5))
}

raw_specs <- list(
  list(col = "antisaccade", title = "Antisaccade", xlab = "Mean response time (ms)", bin = 20, fill = "#DC267F"),
  list(col = "stopsignal", title = "Stop Signal", xlab = "Stop-signal RT (ms)", bin = 15, fill = "#DC267F"),
  list(col = "stroop", title = "Stroop", xlab = "Inhibition cost (ms)", bin = 33, fill = "#DC267F"),
  list(col = "dualnback", title = "Dual n-back", xlab = "Correct responses (%)", bin = 0.025, fill = "#648FFF"),
  list(col = "lettermemory", title = "Letter Memory", xlab = "Correct responses (%)", bin = 0.083333, fill = "#648FFF"),
  list(col = "keeptrack", title = "Keep Track", xlab = "Correct responses (%)", bin = 0.0357, fill = "#648FFF"),
  list(col = "categoryswitch", title = "Category Switch", xlab = "Switching cost (ms)", bin = 33, fill = "#FFB000"),
  list(col = "colorshape", title = "Color Shape", xlab = "Switching cost (ms)", bin = 40, fill = "#FFB000"),
  list(col = "numberletter", title = "Number Letter", xlab = "Switching cost (ms)", bin = 66, fill = "#FFB000")
)

raw_plots <- lapply(raw_specs, function(spec) {
  create_hist_density(df_raw, spec$col, spec$title, spec$xlab, spec$bin, spec$fill)
})
ef_raw_grid <- do.call(plot_grid, c(raw_plots, ncol = 3, nrow = 3))
ef_raw_grid <- ef_raw_grid + ggtitle("Histogram and density of EF scores (raw)") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
ef_raw_plot_path <- file.path(figure_path, "SuppFig1_EF_score_distribution.pdf")
ggsave(ef_raw_plot_path, ef_raw_grid, width = 14, height = 10)

z_specs <- lapply(raw_specs, function(spec) {
  list(col = paste0("zscore_", spec$col), title = spec$title, xlab = "z-score", bin = 0.4, fill = spec$fill)
})

z_plots <- lapply(z_specs, function(spec) {
  create_hist_density(df_z, spec$col, spec$title, spec$xlab, spec$bin, spec$fill)
})
ef_z_grid <- do.call(plot_grid, c(z_plots, ncol = 3, nrow = 3))
ef_z_grid <- ef_z_grid + ggtitle("Histogram and density of EF scores (z-scored)") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
ef_z_plot_path <- file.path(figure_path, "SuppFig2_EF_z_distribution.pdf")
ggsave(ef_z_plot_path, ef_z_grid, width = 14, height = 10)

calculate_metrics <- function(df_z, column) {
  original <- sub("^zscore_", "", column)
  mean_val <- mean(df_raw[[original]], na.rm = TRUE)
  sd_val <- sd(df_raw[[original]], na.rm = TRUE)
  if (original %in% c("dualnback", "keeptrack", "lettermemory")) {
    mean_val <- mean_val * 100
    sd_val <- sd_val * 100
  }
  data.frame(
    task = original,
    mean = round(mean_val, 2),
    sd = round(sd_val, 2),
    skewness = round(skewness(df_z[[column]], na.rm = TRUE), 2),
    kurtosis = round(kurtosis(df_z[[column]], na.rm = TRUE), 2)
  )
}
ef_metrics <- do.call(rbind, lapply(z_cols, calculate_metrics, df_z = df_z))

print(ef_raw_grid)
print(ef_z_grid)

################################################################################
## EF COMPOSITE BY SEX
################################################################################

df_ef <- df_z

ef_group_stats <- df_ef %>%
  group_by(Sex) %>%
  summarise(
    n = n(),
    mean = mean(zscore_EF, na.rm = TRUE),
    sd = sd(zscore_EF, na.rm = TRUE),
    .groups = "drop"
  )

ef_ttest <- t.test(zscore_EF ~ Sex, data = df_ef)

calculate_cohens_d <- function(group_stats) {
  n1 <- group_stats$n[1]
  n2 <- group_stats$n[2]
  m1 <- group_stats$mean[1]
  m2 <- group_stats$mean[2]
  sd1 <- group_stats$sd[1]
  sd2 <- group_stats$sd[2]
  s_pooled <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  (m1 - m2) / s_pooled
}
ef_cohens_d <- calculate_cohens_d(ef_group_stats)

df_ef$Sex_numeric <- as.numeric(as.factor(df_ef$Sex))
ef_plot_label <- sprintf(
  "t(%.0f) = %.2f\np = %.3f\nd = %.2f",
  ef_ttest$parameter,
  ef_ttest$statistic,
  ef_ttest$p.value,
  abs(ef_cohens_d)
)

plot_EF_Sex <- ggplot(df_ef, aes(x = Sex, y = zscore_EF, fill = Sex, color = Sex)) +
  ggdist::stat_halfeye(
    width = 0.5,
    .width = 0,
    justification = -0.2,
    point_colour = NA,
    alpha = 0.5
  ) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.5) +
  geom_point(
    aes(x = Sex_numeric - 0.2),
    size = 1.5,
    alpha = 0.6,
    shape = 16,
    position = position_jitter(width = 0.1, height = 0, seed = 123)
  ) +
  annotate("text", x = Inf, y = Inf, label = ef_plot_label,
           hjust = 1.1, vjust = 1.5, size = 3.5) +
  theme_pubr() +
  scale_x_discrete(labels = c("man" = "Men", "woman" = "Women")) +
  xlab("Sex") +
  ylab("EF composite (z-score)") +
  scale_color_manual(values = c("man" = "#003f5c", "woman" = "#ff8531")) +
  scale_fill_manual(values = c("man" = "#003f5c", "woman" = "#ff8531")) +
  theme(legend.position = "none")

print(plot_EF_Sex)

ef_sex_plot_path <- file.path(figure_path, "SuppFig3_EF_by_Sex.pdf")
ggsave(ef_sex_plot_path, plot_EF_Sex, width = 6, height = 5)

################################################################################
## STORE RESULTS
################################################################################

supplementary_results <- list(
  excluded_table = excluded_table,
  clamp_stats = clamp_stats,
  ef_metrics = ef_metrics,
  ef_raw_plot_path = ef_raw_plot_path,
  ef_z_plot_path = ef_z_plot_path,
  ef_sex_plot_path = ef_sex_plot_path,
  ef_sex_summary = list(
    group_stats = ef_group_stats,
    t_test = ef_ttest,
    cohens_d = ef_cohens_d
  )
)

print(supplementary_results)