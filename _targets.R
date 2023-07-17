# _targets.R file
library(targets)
source("R/functions.R")
tar_option_set(packages = c("tidyverse", "lme4", "purrr", "base", "ggeffects", "patchwork"))
list(
  # Load in data
  tar_target(rsa_file, "rsa_data.csv", format = "file"),
  tar_target(sprint_file, "sprint_data.csv", format = "file"),
  tar_target(rsa_data, get_rsa_data(rsa_file)),
  # tar_target(sprint_data, get_sprint_data(sprint_file)),
  
  # Fit models for repeated sprint ability (rsa) by disability and classification
  tar_target(disability_rsa_models, fit_disability_rsa_models(rsa_data)),
  tar_target(classif_rsa_models, fit_classif_rsa_models(rsa_data)),
  
  # Get model summaries
  tar_target(disability_rsa_summaries, get_rsa_summaries(disability_rsa_models)),
  tar_target(classif_rsa_summaries, get_rsa_summaries(classif_rsa_models)),
  
  # Get model predicted values
  tar_target(disability_rsa_predicted_values, get_disability_rsa_predicted_values(disability_rsa_models)),
  tar_target(classif_rsa_predicted_values, get_classif_rsa_predicted_values(classif_rsa_models)),
  
  # Make plots
  tar_target(disability_rsa_plot, make_disability_rsa_plot(rsa_data, disability_rsa_predicted_values)),
  tar_target(classif_rsa_plot, make_classif_rsa_plot(rsa_data, classif_rsa_predicted_values))
  
)