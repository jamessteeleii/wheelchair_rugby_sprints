# _targets.R file
library(targets)
library(tarchetypes)
source("R/functions.R")
tar_option_set(packages = c("tidyverse", "rstan", "brms", "base", "bayesplot",
                            "tidybayes", "broom.mixed", "quarto",
                            "kableExtra", "patchwork"))
list(
  ##### Repeated Sprint Trials
  # Load in data
  tar_target(rsa_file, "rsa_data.csv", format = "file"),
  tar_target(rsa_data, get_rsa_data(rsa_file)),
  # tar_target(sprint_data, get_sprint_data(sprint_file)),

  # Make individual data plots by disability and classification
  tar_target(ind_disability_rsa_plot, make_ind_disability_rsa_plot(rsa_data)),
  tar_target(ind_classif_rsa_plot, make_ind_classif_rsa_plot(rsa_data)),

  # Fit models for repeated sprint ability (rsa) by disability and classification
  tar_target(disability_rsa_model, fit_disability_rsa_model(rsa_data)),
  tar_target(classif_rsa_model, fit_classif_rsa_model(rsa_data)),

  # Get tidy model summaries
  tar_target(tidy_disability_rsa_model, get_tidy_model(disability_rsa_model)),
  tar_target(tidy_classif_rsa_model, get_tidy_model(classif_rsa_model)),

  # Diagnostic plots
  tar_target(rhat_plot_disability_rsa_model, make_rhat_plot(disability_rsa_model)),
  tar_target(trace_plot_disability_rsa_model, make_trace_plots(disability_rsa_model)),
  tar_target(pp_check_disability_rsa_model, make_pp_check(disability_rsa_model)),
  tar_target(rhat_plot_classif_rsa_model, make_rhat_plot(classif_rsa_model)),
  tar_target(trace_plot_classif_rsa_model, make_trace_plots(classif_rsa_model)),
  tar_target(pp_check_classif_rsa_model, make_pp_check(classif_rsa_model)),

  # Model plots i.e., global grand means
  tar_target(disability_rsa_model_plot, make_disability_rsa_model_plot(rsa_data, disability_rsa_model)),
  tar_target(classif_rsa_model_plot, make_classif_rsa_model_plot(rsa_data, classif_rsa_model)),


  ##### Sprint Trials
  tar_target(sprint_file, "sprint_data.csv", format = "file"),
  tar_target(sprint_data, get_sprint_data(sprint_file)),

  ### Velocity

  # Make individual data plots by disability and classification
  tar_target(ind_disability_velocity_plot, make_ind_disability_velocity_plot(sprint_data)),
  tar_target(ind_classif_velocity_plot, make_ind_classif_velocity_plot(sprint_data)),

  # Fit models for repeated sprint ability (velocity) by disability and classification
  tar_target(disability_velocity_model, fit_disability_velocity_model(sprint_data)),
  tar_target(classif_velocity_model, fit_classif_velocity_model(sprint_data)),

  # Get tidy model summaries
  tar_target(tidy_disability_velocity_model, get_tidy_model(disability_velocity_model)),
  tar_target(tidy_classif_velocity_model, get_tidy_model(classif_velocity_model)),

  # Diagnostic plots
  tar_target(rhat_plot_disability_velocity_model, make_rhat_plot(disability_velocity_model)),
  tar_target(trace_plot_disability_velocity_model, make_trace_plots(disability_velocity_model)),
  tar_target(pp_check_disability_velocity_model, make_pp_check(disability_velocity_model)),
  tar_target(rhat_plot_classif_velocity_model, make_rhat_plot(classif_velocity_model)),
  tar_target(trace_plot_classif_velocity_model, make_trace_plots(classif_velocity_model)),
  tar_target(pp_check_classif_velocity_model, make_pp_check(classif_velocity_model)),

  # Model plots i.e., global grand means
  tar_target(disability_velocity_model_plot, make_disability_velocity_model_plot(sprint_data, disability_velocity_model)),
  tar_target(classif_velocity_model_plot, make_classif_velocity_model_plot(sprint_data, classif_velocity_model)),

  ### Acceleration

  # Make individual data plots by disability and classification
  tar_target(ind_disability_acceleration_plot, make_ind_disability_acceleration_plot(sprint_data)),
  tar_target(ind_classif_acceleration_plot, make_ind_classif_acceleration_plot(sprint_data)),

  # Fit models for repeated sprint ability (acceleration) by disability and classification
  tar_target(disability_acceleration_model, fit_disability_acceleration_model(sprint_data)),
  tar_target(classif_acceleration_model, fit_classif_acceleration_model(sprint_data)),

  # Get tidy model summaries
  tar_target(tidy_disability_acceleration_model, get_tidy_model(disability_acceleration_model)),
  tar_target(tidy_classif_acceleration_model, get_tidy_model(classif_acceleration_model)),

  # Diagnostic plots
  tar_target(rhat_plot_disability_acceleration_model, make_rhat_plot(disability_acceleration_model)),
  tar_target(trace_plot_disability_acceleration_model, make_trace_plots(disability_acceleration_model)),
  tar_target(pp_check_disability_acceleration_model, make_pp_check(disability_acceleration_model)),
  tar_target(rhat_plot_classif_acceleration_model, make_rhat_plot(classif_acceleration_model)),
  tar_target(trace_plot_classif_acceleration_model, make_trace_plots(classif_acceleration_model)),
  tar_target(pp_check_classif_acceleration_model, make_pp_check(classif_acceleration_model)),

  # Model plots i.e., global grand means
  tar_target(disability_acceleration_model_plot, make_disability_acceleration_model_plot(sprint_data, disability_acceleration_model)),
  tar_target(classif_acceleration_model_plot, make_classif_acceleration_model_plot(sprint_data, classif_acceleration_model)),

  ##### Combine and save plots as tiffs

  ### Repeated sprint ability
  tar_target(combined_rsa_plot, combine_plots(ind_disability_rsa_plot, disability_rsa_model_plot,
                                              ind_classif_rsa_plot, classif_rsa_model_plot,
                                             "Disability - Repeated Sprints",
                                             "Classification - Repeated Sprints",
                                             "Model: Time ~ Disability * Sprint Number * Distance + (Sprint Number + Distance | ID)",
                                             "Model: Time ~ Classification * Sprint Number * Distance + (Sprint Number + Distance | ID)")),


  tar_target(combined_rsa_plot_tiff, make_plot_tiff(combined_rsa_plot, "plots/combined_rsa_plot.tiff",
                                                   width=15, height=7.5, device="tiff", dpi=300)),

  ### Velocity

  tar_target(combined_velocity_plot, combine_plots(ind_disability_velocity_plot, disability_velocity_model_plot,
                                              ind_classif_velocity_plot, classif_velocity_model_plot,
                                              "Disability - Velocity",
                                              "Classification - Velocity",
                                              "Model: Time ~ Disability * Distance + (Distance | ID)",
                                              "Model: Time ~ Classification * Distance + (Distance | ID)")),


  tar_target(combined_velocity_plot_tiff, make_plot_tiff(combined_velocity_plot, "plots/combined_velocity_plot.tiff",
                                                    width=15, height=7.5, device="tiff", dpi=300)),

  # # Acceleration

  tar_target(combined_acceleration_plot, combine_plots(ind_disability_acceleration_plot, disability_acceleration_model_plot,
                                                   ind_classif_acceleration_plot, classif_acceleration_model_plot,
                                                   "Disability - Acceleration",
                                                   "Classification - Acceleration",
                                                   "Model: Time ~ Disability * Distance + (Distance | ID)",
                                                   "Model: Time ~ Classification * Distance + (Distance | ID)")),


  tar_target(combined_acceleration_plot_tiff, make_plot_tiff(combined_acceleration_plot, "plots/combined_acceleration_plot.tiff",
                                                         width=15, height=7.5, device="tiff", dpi=300)),

  ##### Reporting
  # Render the report
  tar_quarto(report, "report.qmd"),

  # Render the supplementary material
  tar_quarto(diagnostic_plots, "diagnostic_plots.qmd")


)
