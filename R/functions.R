### Repeated Sprint Trials

# Get the repeated sprint trial data
get_rsa_data <- function(file) {
  data <- read.csv(file, fileEncoding = 'UTF-8-BOM') %>%
    mutate_if(is.character,as.factor) %>%
    pivot_longer(c("x5_m", "x10_m", "x15_m", "x20_m"), names_to = "distance", values_to = "time") %>%
    rename(disability = "sci")  %>%
    mutate(distance = factor(str_remove_all(distance,"x|_"), levels = c("5m", "10m", "15m", "20m")),
           classif = as.factor(classif))

  data$distance <- recode(data$distance,
                                   "5m" = "0-5m",
                                   "10m" = "5-10m",
                                   "15m" = "10-15m",
                                   "20m" = "15-20m")

  data
}

# Make individual data plots

make_ind_disability_rsa_plot <- function(data) {
  individual_data_plot <- data %>%
    ggplot(aes(x=sprint_number, color=disability)) +
    geom_line(aes(y=time, group=id), size=0.75, alpha=0.5,
              stat = "smooth", formula = y ~ x, method = "lm", se=FALSE,) +
    geom_point(aes(y=time), size=1, alpha=0.25) +
    labs(x="Sprint Number",
         y="Time (seconds)",
         subtitle = "Individual Data and Smooths",
         color = "Disability") +
    guides(fill = "none",
           color = "none") +
    scale_x_continuous(breaks = c(1:10)) +
    scale_y_continuous(breaks = c(2,3,4,5,6,7,8,9)) +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Dark2") +
    facet_grid(.~distance) +
    theme_classic()
}

make_ind_classif_rsa_plot <- function(data) {
  individual_data_plot <- data %>%
    ggplot(aes(x=sprint_number, color=classif)) +
    geom_line(aes(y=time, group=id), size=0.75, alpha=0.5,
              stat = "smooth", formula = y ~ x, method = "lm", se=FALSE,) +
    geom_point(aes(y=time), size=1, alpha=0.25) +
    labs(x="Sprint Number",
         y="Time (seconds)",
         subtitle = "Individual Data and Smooths",
         color = "Classification") +
    guides(fill = "none",
           color = "none") +
    scale_x_continuous(breaks = c(1:10)) +
    scale_y_continuous(breaks = c(2,3,4,5,6,7,8,9)) +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Dark2") +
    facet_grid(.~distance) +
    theme_classic()
}

# Repeated sprint models (rsa) for disability and classification
fit_disability_rsa_model <- function(data) {

  # run rstan quicker - for bayesian beta regression later on
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores()-1)

  disability_rsa_model <- brm(time ~ disability * sprint_number * distance + (sprint_number + distance | id),
                     data = data,
                     seed = 1988,
                     chains = 4,
                     iter = 8000, warmup = 4000,
                     cores = 4
  )
}

fit_classif_rsa_model <- function(data) {

  # run rstan quicker - for bayesian beta regression later on
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores()-1)

  classif_rsa_model <- brm(time ~ classif * sprint_number * distance + (sprint_number + distance | id),
                              data = data,
                              seed = 1988,
                              chains = 4,
                              iter = 8000, warmup = 4000,
                              cores = 4
  )
}

# Model plots i.e., global grand means

make_disability_rsa_model_plot <- function(data, model) {
  posterior_epred_draws <- crossing(sprint_number = seq(from = 1, to = 10, by=1),
                                    id = unique(data$id),
                                    disability = unique(data$disability),
                                    distance = unique(data$distance)) %>%
    add_epred_draws(model, re_formula = NA)

  model_data_plot <- posterior_epred_draws %>%
    ggplot(aes(x = sprint_number, y = .epred, color=disability, fill=disability)) +
    stat_lineribbon(aes(y = .epred), .width = .95, alpha = 0.25, show.legend=TRUE) +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Dark2") +
    scale_x_continuous(breaks = c(1:10)) +
    scale_y_continuous(breaks = c(2,3,4,5,6,7,8)) +
    facet_grid(.~distance) +
    labs(
      subtitle = "Global grand mean and 95% credible interval (CI)",
      x="Sprint Number",
      y="Time (seconds)",
      color = "Disability", fill = "Disability") +
    theme_classic()

}

make_classif_rsa_model_plot <- function(data, model) {
  posterior_epred_draws <- crossing(sprint_number = seq(from = 1, to = 10, by=1),
                                    id = unique(data$id),
                                    classif = unique(data$classif),
                                    distance = unique(data$distance)) %>%
    add_epred_draws(model, re_formula = NA)

  model_data_plot <- posterior_epred_draws %>%
    ggplot(aes(x = sprint_number, y = .epred, color=classif, fill=classif)) +
    stat_lineribbon(aes(y = .epred), .width = .95, alpha = 0.25, show.legend=TRUE) +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Dark2") +
    scale_x_continuous(breaks = c(1:10)) +
    scale_y_continuous(breaks = c(2,3,4,5,6,7,8)) +
    facet_grid(.~distance) +
    labs(
      subtitle = "Global grand mean and 95% credible interval (CI)",
      x="Sprint Number",
      y="Time (seconds)",
      color = "Classification", fill = "Classification") +
    theme_classic()

}


### Sprint trials

# Get sprint trial data
get_sprint_data <- function(file) {
  data <- read.csv(file, fileEncoding = 'UTF-8-BOM') %>%
    mutate_if(is.character,as.factor) %>%
    mutate(classif = as.factor(classif))

  velocity_data <- data %>%
    pivot_longer(c("x0_5_vel", "x5_10_vel", "x10_15_vel", "x15_20_vel"),
                 names_to = "distance", values_to = "velocity") %>%
    select(id, disability, classif, trial, distance, velocity)

  velocity_data$distance <- recode(velocity_data$distance,
                                   "x0_5_vel" = "0-5m",
                                   "x5_10_vel" = "5-10m",
                                   "x10_15_vel" = "10-15m",
                                   "x15_20_vel" = "15-20m")

  acceleration_data <- data %>%
    pivot_longer(c("accel_0_5m", "accel_5_10m", "accel_10_15m", "accel_15_20m"),
                 names_to = "distance", values_to = "acceleration") %>%
    select(id, disability, classif, trial, distance, acceleration)

  acceleration_data$distance <- recode(acceleration_data$distance,
                                       "accel_0_5m" = "0-5m",
                                       "accel_5_10m" = "5-10m",
                                       "accel_10_15m" = "10-15m",
                                       "accel_15_20m" = "15-20m")

  data <- left_join(velocity_data, acceleration_data, by = c("id", "trial", "distance",
                                                             "disability", "classif")) %>%
    mutate(distance = factor(distance, levels = c("0-5m", "5-10m", "10-15m", "15-20m")))

}

### Velocity data

# Make individual data plots
make_ind_disability_velocity_plot <- function(data) {
  individual_data_plot <- data %>%
    ggplot(aes(x=distance, color=disability)) +
    geom_line(aes(y=velocity, group=interaction(id,trial)), size=0.75, alpha=0.5) +
    geom_point(aes(y=velocity), size=1, alpha=0.25) +
    labs(x="Distance",
         y=bquote("Velocity (m\U00B7"*"s"^-1*")"),
         subtitle = "Individual Data") +
    guides(fill = "none",
           color = "none") +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Dark2") +
    scale_y_continuous(breaks = c(2,3,4,5,6,7,8,9)) +
    theme_classic()
}

make_ind_classif_velocity_plot <- function(data) {
  individual_data_plot <- data %>%
    ggplot(aes(x=distance, color=classif)) +
    geom_line(aes(y=velocity, group=interaction(id,trial)), size=0.75, alpha=0.5) +
    geom_point(aes(y=velocity), size=1, alpha=0.25) +
    labs(x="Distance",
         y=bquote("Velocity (m\U00B7"*"s"^-1*")"),
         subtitle = "Individual Data") +
    guides(fill = "none",
           color = "none") +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Dark2") +
    scale_y_continuous(breaks = c(2,3,4,5,6,7,8,9)) +
    theme_classic()
}

# Sprint trial models for disability and classification
fit_disability_velocity_model <- function(data) {

  # run rstan quicker - for bayesian beta regression later on
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores()-1)

  disability_velocity_model <- brm(velocity ~ disability * distance + (distance | id),
                              data = data,
                              seed = 1988,
                              chains = 4,
                              iter = 8000, warmup = 4000,
                              cores = 4
  )
}

fit_classif_velocity_model <- function(data) {

  # run rstan quicker - for bayesian beta regression later on
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores()-1)

  classif_velocity_model <- brm(velocity ~ classif * distance + (distance | id),
                           data = data,
                           seed = 1988,
                           chains = 4,
                           iter = 8000, warmup = 4000,
                           cores = 4
  )
}


# Model plots i.e., global grand means

make_disability_velocity_model_plot <- function(data, model) {
  posterior_epred_draws <- crossing(id = unique(data$id),
                                    disability = unique(data$disability),
                                    distance = unique(data$distance)) %>%
    add_epred_draws(model, re_formula = NA)

  model_data_plot <- posterior_epred_draws %>%
    ggplot(aes(x = distance, y = .epred, color=disability, fill=disability)) +
    stat_slabinterval(.width = .95, alpha = 0.5, position = position_dodge(width = 0.2),
                      size=1, scale = 1) +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Dark2") +
    labs(
      subtitle = "Global grand mean and 95% credible interval (CI)",
      x="Sprint Number",
      y=bquote("Velocity (m\U00B7"*"s"^-1*")"),
      color = "Disability", fill = "Disability") +
    theme_classic()

}

make_classif_velocity_model_plot <- function(data, model) {
  posterior_epred_draws <- crossing(id = unique(data$id),
                                    classif = unique(data$classif),
                                    distance = unique(data$distance)) %>%
    add_epred_draws(model, re_formula = NA)

  model_data_plot <- posterior_epred_draws %>%
    ggplot(aes(x = distance, y = .epred, color=classif, fill=classif)) +
    stat_slabinterval(.width = .95, alpha = 0.5, position = position_dodge(width = 0.2),
                      size=1, scale = 1) +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Dark2") +
    labs(
      subtitle = "Global grand mean and 95% credible interval (CI)",
      x="Sprint Number",
      y=bquote("Velocity (m\U00B7"*"s"^-1*")"),
      color = "Classification", fill = "Classification") +
    theme_classic()

}


### Acceleration data

# Make individual data plots
make_ind_disability_acceleration_plot <- function(data) {
  individual_data_plot <- data %>%
    ggplot(aes(x=distance, color=disability)) +
    geom_line(aes(y=acceleration, group=interaction(id,trial)), size=0.75, alpha=0.5) +
    geom_point(aes(y=acceleration), size=1, alpha=0.25) +
    labs(x="Distance",
         y=bquote("Acceleration (m\U00B7"*"s"^-1*")"),
         subtitle = "Individual Data") +
    guides(fill = "none",
           color = "none") +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Dark2") +
    theme_classic()
}

make_ind_classif_acceleration_plot <- function(data) {
  individual_data_plot <- data %>%
    ggplot(aes(x=distance, color=classif)) +
    geom_line(aes(y=acceleration, group=interaction(id,trial)), size=0.75, alpha=0.5) +
    geom_point(aes(y=acceleration), size=1, alpha=0.25) +
    labs(x="Distance",
         y=bquote("Acceleration (m\U00B7"*"s"^-1*")"),
         subtitle = "Individual Data") +
    guides(fill = "none",
           color = "none") +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Dark2") +
    theme_classic()
}

# Sprint trial models for disability and classification
fit_disability_acceleration_model <- function(data) {

  # run rstan quicker - for bayesian beta regression later on
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores()-1)

  disability_acceleration_model <- brm(acceleration ~ disability * distance + (distance | id),
                                   data = data,
                                   seed = 1988,
                                   chains = 4,
                                   iter = 8000, warmup = 4000,
                                   cores = 4
  )
}

fit_classif_acceleration_model <- function(data) {

  # run rstan quicker - for bayesian beta regression later on
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores()-1)

  classif_acceleration_model <- brm(acceleration ~ classif * distance + (distance | id),
                                data = data,
                                seed = 1988,
                                chains = 4,
                                iter = 8000, warmup = 4000,
                                cores = 4
  )
}


# Model plots i.e., global grand means

make_disability_acceleration_model_plot <- function(data, model) {
  posterior_epred_draws <- crossing(id = unique(data$id),
                                    disability = unique(data$disability),
                                    distance = unique(data$distance)) %>%
    add_epred_draws(model, re_formula = NA)

  model_data_plot <- posterior_epred_draws %>%
    ggplot(aes(x = distance, y = .epred, color=disability, fill=disability)) +
    stat_slabinterval(.width = .95, alpha = 0.5, position = position_dodge(width = 0.2),
                      size=1, scale = 1) +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Dark2") +
    labs(
      subtitle = "Global grand mean and 95% credible interval (CI)",
      x="Sprint Number",
      y=bquote("Acceleration (m\U00B7"*"s"^-1*")"),
      color = "Disability", fill = "Disability") +
    theme_classic()

}

make_classif_acceleration_model_plot <- function(data, model) {
  posterior_epred_draws <- crossing(id = unique(data$id),
                                    classif = unique(data$classif),
                                    distance = unique(data$distance)) %>%
    add_epred_draws(model, re_formula = NA)

  model_data_plot <- posterior_epred_draws %>%
    ggplot(aes(x = distance, y = .epred, color=classif, fill=classif)) +
    stat_slabinterval(.width = .95, alpha = 0.5, position = position_dodge(width = 0.2),
                      size=1, scale = 1) +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Dark2") +
    labs(
      subtitle = "Global grand mean and 95% credible interval (CI)",
      x="Sprint Number",
      y=bquote("Acceleration (m\U00B7"*"s"^-1*")"),
      color = "Classification", fill = "Classification") +
    theme_classic()

}


### General functions

get_tidy_model <- function(model) {
  tidy(model)
}

# Diagnostic plots
make_rhat_plot <- function(model) {
  mod_rhat <- enframe(brms::rhat(model))

  rhat_main_params <- mod_rhat$value

  mcmc_rhat(rhat_main_params) +
    scale_x_continuous(breaks = c(1,1.01,1.02,1.03,1.04,1.05)) +
    geom_vline(xintercept = 1.01, linetype="dashed", alpha = 0.25)
}

make_trace_plots <- function(model) {
  plot(model)
}

make_pp_check <- function(model) {
  pp_check(model)
}

# Plotting

combine_plots <- function(plot1, plot2, plot3, plot4, title1, title2, caption1, caption2) {
  p1 <- (plot1 / plot2) +
  plot_layout(guides = "collect") +
    plot_annotation(title = title1,
                      caption = caption1,
                      tag_level = "A",
                      tag_prefix = "(", tag_suffix = ")") &
    theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = 'bottom')

  p2 <- (plot3 / plot4) +
    plot_layout(guides = "collect") +
    plot_annotation(title = title2,
                    caption = caption2,
                    tag_level = list(c("(C)", "(D)"))) &
    theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = 'bottom')

  plot <- wrap_elements(p1) | wrap_elements(p2)

}



make_plot_tiff <- function(plot, path, width, height, device, dpi) {

  ggsave(filename = path, plot = plot, width = width, height = height, device = device, dpi = dpi)

  }
