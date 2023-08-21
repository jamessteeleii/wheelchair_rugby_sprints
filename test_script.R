### This is just my scrappy script for figuring stuff out

library(tidyverse)
# library(purrr)
# library(broom.mixed)
# library(lme4)
library(ggeffects)
library(patchwork)

data <- read.csv("rsa_data.csv", fileEncoding = 'UTF-8-BOM') %>%
  mutate_if(is.character,as.factor) %>%
  pivot_longer(c("x5_m", "x10_m", "x15_m", "x20_m"), names_to = "distance", values_to = "time") %>%
  rename(disability = "sci")  %>%
  mutate(distance = factor(str_remove_all(distance,"x|_"), levels = c("5m", "10m", "15m", "20m")),
         classif = as.factor(classif))

# nest_data <- data %>%
#   group_by(distance) %>%
#   nest()
#
# fit_model <- function(df) {
#   lmer(time ~ classif * sprint_number + (1 | id),
#        data = df,
#        REML = TRUE, control = lmerControl(optimizer="Nelder_Mead"))
# }
#
# models <- map(nest_data$data, fit_model)
#
#
# summaries <- models %>% map(summary)
#
# preds <- models %>%
#   map(function(models) ggpredict(models,terms =  c("classif","sprint_number [1:10]"))) %>%
#   bind_rows(.id = "distance") %>%
#   rename(classif = "x",
#          sprint_number = "group") %>%
#   mutate(sprint_number = as.numeric(sprint_number),
#          distance = rep(c("5m", "10m", "15m", "20m"), each = 20),
#          distance = factor(distance, levels = c("5m", "10m", "15m", "20m")))
#
# individual_data_plot <- data %>%
#   ggplot(aes(x=sprint_number, color=disability)) +
#   geom_point(aes(y=time), size=0.5, alpha=0.25) +
#   geom_smooth(aes(y=time, group=id), size=0.25, alpha=0.25,
#               se=FALSE) +
#   labs(x="Sprint Number",
#        y="Time (seconds)",
#        title = "Individual Data and Smooths",
#        color = "Disability") +
#   guides(fill = "none",
#          color = "none") +
#   scale_x_continuous(breaks = c(1:10)) +
#   scale_y_continuous(breaks = c(2,3,4,5,6,7,8,9)) +
#   scale_color_manual(values = c("#E69F00", "#56B4E9")) +
#   facet_grid(.~distance) +
#   theme_classic()
#
# model_data_plot <- preds %>%
#   ggplot(aes(x=sprint_number, fill=disability)) +
#   geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.25) +
#   geom_line(aes(y=predicted, color=disability), size=1) +
#   labs(x="Sprint Number",
#        y="Time (seconds)",
#        title = "Model Estimates",
#        subtitle = "Predicted values with 95% confidence intervals (ribbon)",
#        color = "Disability") +
#   guides(fill = "none") +
#   scale_x_continuous(breaks = c(1:10)) +
#   scale_y_continuous(breaks = c(2,3,4,5,6,7,8)) +
#   scale_color_manual(values = c("#E69F00", "#56B4E9")) +
#   facet_grid(.~distance) +
#   theme_classic() +
#   theme(legend.position = "bottom")
#
# (individual_data_plot / model_data_plot) +
#   plot_annotation(title = "Effects of Disability upon Repeated Sprint Times",
#                   caption = "Model: Time ~ Disability * Sprint Number + (Sprint Number | Participant)")
#
# ggsave("Disability.tiff", width = 7.5, height = 7.5, device = "tiff", dpi = 300)
#
#
# individual_data_plot <- data %>%
#   ggplot(aes(x=sprint_number, color=classif)) +
#   geom_point(aes(y=time), size=0.5, alpha=0.25) +
#   geom_smooth(aes(y=time, group=id), size=0.25, alpha=0.25,
#               se=FALSE) +
#   labs(x="Sprint Number",
#        y=bquote("Velocity (m\U00B7"*"s"^-1*")"),
#        title = "Individual Data and Slopes",
#        color = "Classification") +
#   guides(fill = "none",
#          color = "none") +
#   scale_x_continuous(breaks = c(1:10)) +
#   scale_y_continuous(breaks = c(2,3,4,5,6,7,8,9)) +
#   scale_color_manual(values = c("#E69F00", "#56B4E9")) +
#   facet_grid(.~distance) +
#   theme_classic()
#
# model_data_plot <- preds %>%
#   ggplot(aes(x=sprint_number, fill=classif)) +
#   geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.25) +
#   geom_line(aes(y=predicted, color=classif), size=1) +
#   labs(x="Sprint Number",
#        y="Time (seconds)",
#        title = "Model Estimates",
#        subtitle = "Predicted values with 95% confidence intervals (ribbon)",
#        color = "Classification") +
#   guides(fill = "none") +
#   scale_x_continuous(breaks = c(1:10)) +
#   scale_y_continuous(breaks = c(2,3,4,5,6,7,8)) +
#   scale_color_manual(values = c("#E69F00", "#56B4E9")) +
#   facet_grid(.~distance) +
#   theme_classic() +
#   theme(legend.position = "bottom")
#
#
#
#
#
# #
mod_all_dis <- lme4::lmer(time ~ disability * sprint_number * distance + (1 + sprint_number | id),
     data = data,
     REML = TRUE, control = lme4::lmerControl(optimizer="Nelder_Mead"))
#
#
# preds_all_dis <- ggpredict(mod_all_dis,terms =  c("classif","sprint_number [1:10]","distance")) %>%
#   rename(classif = "x",
#          sprint_number = "group",
#          distance = "facet") %>%
#   mutate(sprint_number = as.numeric(sprint_number),
#          distance = factor(distance, levels = c("5m", "10m", "15m", "20m")))
#
# preds_all_dis %>%
#   ggplot(aes(x=sprint_number)) +
#   geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=classif), alpha=0.25) +
#   geom_line(aes(y=predicted, color=classif), size=1) +
#   labs(x="Sprint Number",
#        y="Time (seconds)",
#        title = "Model Estimates",
#        subtitle = "Predicted values with 95% confidence intervals (ribbon)",
#        color = "Classification") +
#   guides(fill = "none") +
#   scale_x_continuous(breaks = c(1:10)) +
#   scale_y_continuous(breaks = c(2,3,4,5,6,7,8)) +
#   scale_color_manual(values = c("#E69F00", "#56B4E9")) +
#   facet_grid(.~distance) +
#   theme_classic() +
#   theme(legend.position = "bottom")

### try bayes
library(brms)
library(rstan)
library(tidybayes)
library(bayesplot)

# run rstan quicker - for bayesian beta regression later on
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores()-1)

# get_prior(time ~ 1 + disability * sprint_number * distance + (1 + sprint_number * distance | id),
#           data = data)
#
# prior_slope_5m <- (2.470566095 - 2.317656579)/10
# prior_slope_10m <- (4.164333032 - 3.929087624)/10
# prior_slope_20m <- (7.187236525 - 6.681458898)/10
#
# prior_sprint_number <- mean(c(prior_slope_5m, prior_slope_10m, prior_slope_20m))
#
# prior_sprint_number_distance10m <- prior_slope_10m - prior_slope_5m
# prior_sprint_number_distance20m <- prior_slope_20m - prior_slope_5m
#
# prior_distance5_intercept_m <- 2.317656579
# prior_distance10_m <- 3.929087624
# prior_distance20_m <- 6.681458898
#
# prior_distance5_intercept_sd <- 2.616418248 - 2.317656579
# prior_distance10_sd <- 4.423102981 - 3.929087624
# prior_distance20_sd <- 7.528342367 - 6.681458898
#
#
# prior_disabilitySCI <- (mean(c(2.597632251,2.988173534,3.235686106,3.423689619,3.423433816,3.135446247,
#                               3.836055541,3.874356214,3.763744709,4.43663427,4.105520655,4.072208139,
#                               4.270025182,4.092509588,3.893948391)) / mean(c(3.761465738, 3.937167456, 3.847729456, 3.864170607)))*100
#
#
# other_intercept <- data %>%
#   filter(disability == "Other" &
#            sprint_number == 1 &
#            distance == "5m") %>%
#   summarise(time = (mean(time)*1.05) - mean(time))
#
#
#
# priors <-
#   prior("normal(2.317656579, 0.2987617)", class = "b", coef = "") +
#   prior("normal(3.929087624, 0.4940154)", class = "b", coef = "distance10m") +
#   prior("normal(6.681458898, 0.8468835)", class = "b", coef = "distance20m") +
#   prior("normal(0.02979775, 0.001)", class = "b", coef = "sprint_number") +
#   prior("normal(0.008233589, 0.001)", class = "b", coef = "sprint_number:distance10m") +
#   prior("normal(0.03528681, 0.001)", class = "b", coef = "sprint_number:distance20m") +
#   prior("normal(0.115, 0.01)", class = "b", coef = "disabilitySCI")
#
#

mod_all_dis <- brm(time ~ disability * sprint_number * distance + (1 + sprint_number * distance | id),
                    data = data,
                   seed = 1988,
                   chains = 4,
                   iter = 8000, warmup = 4000,
                   cores = 4,
                   # prior = priors,
                   sample_prior = TRUE
                   )

plot(mod_all_dis)

pp_check(mod_all_dis)

mod_rhat <- enframe(brms::rhat(mod_all_dis)) %>%
  filter(!str_detect(name, "^r_id"))

rhat_main_params <- mod_rhat$value

mcmc_rhat(rhat_main_params, size = 0.5) +
  scale_x_continuous(breaks = c(1,1.01,1.02,1.03,1.04,1.05)) +
  geom_vline(xintercept = 1.01, linetype="dashed", alpha = 0.25)

performance::check_model(mod_all_dis)

performance::model_performance(mod_all_dis)
?check_model

posterior_epred_draws <- crossing(sprint_number = seq(from = 1, to = 10, by=1),
                                  id = unique(data$id),
                                  disability = unique(data$disability),
                                  distance = unique(data$distance)) %>%
  add_epred_draws(mod_all_dis, re_formula = NA, ndraws = 4000)

model_data_plot <- posterior_epred_draws %>%
  ggplot(aes(x = sprint_number, y = .epred, color=disability, fill=disability)) +
  # geom_point(data=data, aes(x = sprint_number, y = time), alpha=0.75) +
  stat_lineribbon(aes(y = .epred), .width = .95, alpha = 0.5, show.legend=TRUE) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = c(1:10)) +
  scale_y_continuous(breaks = c(2,3,4,5,6,7,8)) +
  facet_grid(.~distance) +
  labs(
    title = "Expectation of the Posterior Predictive Distribution",
    subtitle = "Global grand mean and 95% credible interval (CI)",
    x="Sprint Number",
    y="Time (seconds)",
    color = "Classification", fill = "Classification") +
  theme_bw() +
  theme(panel.grid=element_blank())




# renv::install("marginaleffects")

library(marginaleffects)


tidy_mod <- broom.mixed::tidy(mod_all_dis)

### sprint data

data <- read.csv("sprint_data.csv", fileEncoding = 'UTF-8-BOM') %>%
  mutate_if(is.character,as.factor)

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

acceleration_data$distance <- recode(velocity_data$distance,
                                 "accel_0_5m" = "0-5m",
                                 "accel_5_10m" = "5-10m",
                                 "accel_10_15m" = "10-15m",
                                 "accel_15_20m" = "15-20m")

data <- left_join(velocity_data, acceleration_data, by = c("id", "trial", "distance",
                                                           "disability", "classif")) %>%
  mutate(distance = factor(distance, levels = c("0-5m", "5-10m", "10-15m", "15-20m")))



?targets::tar_visnetwork


individual_data_plot <- data %>%
  ggplot(aes(x=distance, color=disability)) +
  geom_line(aes(y=velocity, group=interaction(id,trial)), size=0.75, alpha=0.5) +
  geom_point(aes(y=velocity), size=1, alpha=0.25) +
  labs(x="Distance",
       y=bquote("Velocity (m\U00B7"~"s"^-1~")"),
       title = "Individual Data",
       color = "Disability") +
  guides(fill = "none",
         color = "none") +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(breaks = c(2,3,4,5,6,7,8,9)) +
  theme_classic()









mod_all_dis <- brm(velocity ~ disability * distance + (1 + distance | id),
                   data = data,
                   seed = 1988,
                   chains = 4,
                   iter = 8000, warmup = 4000,
                   cores = 4,
                   # control = list(adapt_delta = 0.99, max_treedepth = 12), init = 0
)

plot(mod_all_dis)

pp_check(mod_all_dis)

mod_rhat <- enframe(brms::rhat(mod_all_dis))

rhat_main_params <- mod_rhat$value

mcmc_rhat(rhat_main_params, size = 0.5) +
  scale_x_continuous(breaks = c(1,1.01,1.02,1.03,1.04,1.05)) +
  geom_vline(xintercept = 1.01, linetype="dashed", alpha = 0.25)

performance::check_model(mod_all_dis)

performance::model_performance(mod_all_dis)
?check_model

posterior_epred_draws <- crossing(id = unique(data$id),
                                  disability = unique(data$disability),
                                  distance = unique(data$distance)) %>%
  add_epred_draws(mod_all_dis, re_formula = NA)

model_data_plot <- posterior_epred_draws %>%
  ggplot(aes(x = distance, y = .epred, color=disability, fill=disability)) +
  stat_slabinterval(.width = .95, alpha = 0.5, position = position_dodge(width = 0.2),
                    size=1, scale = 1.5) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Expectation of the Posterior Predictive Distribution",
    subtitle = "Global grand mean and 95% credible interval (CI)",
    x="Sprint Number",
    y=bquote("Velocity (m\U00B7"~"s"^-1~")"),
    color = "Disability", fill = "Disability") +
  theme_bw() +
  theme(panel.grid=element_blank())

df <- targets::tar_load(velocity_data)

ggplot() + labs(title = bquote("Velocity (m\U00B7","s"^-1~")"))
