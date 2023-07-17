# Get the repeated sprint trial data
get_rsa_data <- function(file) {
  read.csv(file, fileEncoding = 'UTF-8-BOM') %>% 
    mutate_if(is.character,as.factor) %>%
    pivot_longer(c("x5_m", "x10_m", "x15_m", "x20_m"), names_to = "distance", values_to = "time") %>%
    rename(disability = "sci")  %>%
    mutate(distance = factor(str_remove_all(distance,"x|_"), levels = c("5m", "10m", "15m", "20m")),
           classif = as.factor(classif))
} 

# # Get the sprint trial data
# get_sprint_data <- function(file) {
#   read.csv(file, fileEncoding = 'UTF-8-BOM') %>% 
#     mutate_if(is.character,as.factor)
# } 


# Repeated sprint models (rsa) for each distance by disability and classification
fit_disability_rsa_models <- function(data) {
  nest_data <- data %>% 
    group_by(distance) %>%  
    nest()
  
  fit_model <- function(df) {
    lmer(time ~ disability * sprint_number + (1 | id),
         data = df,
         REML = TRUE, control = lmerControl(optimizer="Nelder_Mead"))
  }
  
  models <- map(nest_data$data, fit_model)
}
  


fit_classif_rsa_models <- function(data) {
  nest_data <- data %>% 
    group_by(distance) %>%  
    nest()
  
  fit_model <- function(df) {
    lmer(time ~ classif * sprint_number + (1 | id),
         data = df,
         REML = TRUE, control = lmerControl(optimizer="Nelder_Mead"))
  }
  
  models <- map(nest_data$data, fit_model)
}

# Summaries of repeated sprint models 
get_rsa_summaries <- function(models) {
  summaries <- models %>%
    map(summary)
} 

# Predicted values for each model for plotting
get_disability_rsa_predicted_values <- function(models) {
  models %>% 
    map(function(models) ggpredict(models, terms =  c("disability","sprint_number [1:10]"))) %>%
    bind_rows(.id = "distance") %>%
    rename(disability = "x",
           sprint_number = "group") %>%
    mutate(sprint_number = as.numeric(sprint_number),
           distance = rep(c("5m", "10m", "15m", "20m"), each = 20),
           distance = factor(distance, levels = c("5m", "10m", "15m", "20m")))
} 

get_classif_rsa_predicted_values <- function(models) {
  models %>%
    map(function(models) ggpredict(models, terms =  c("classif","sprint_number [1:10]"))) %>%
    bind_rows(.id = "distance") %>%
    rename(classif = "x",
           sprint_number = "group") %>%
    mutate(sprint_number = as.numeric(sprint_number),
           distance = rep(c("5m", "10m", "15m", "20m"), each = 20),
           distance = factor(distance, levels = c("5m", "10m", "15m", "20m")))
}


# Make disability rsa plot

make_disability_rsa_plot <- function(data, preds) {
  individual_data_plot <- data %>%
  ggplot(aes(x=sprint_number, color=disability)) +
  geom_point(aes(y=time), size=0.5, alpha=0.25) +
  geom_smooth(aes(y=time, group=id), size=0.25, alpha=0.25,
              se=FALSE, method = "lm") +
  labs(x="Sprint Number",
       y="Time (seconds)",
       title = "Individual Data and Smooths",
       color = "Disability") +
  guides(fill = "none",
         color = "none") +
  scale_x_continuous(breaks = c(1:10)) +
  scale_y_continuous(breaks = c(2,3,4,5,6,7,8,9)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9")) +
  facet_grid(.~distance) +
  theme_classic()

model_data_plot <- preds %>%
  ggplot(aes(x=sprint_number, fill=disability)) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.25) +
  geom_line(aes(y=predicted, color=disability), size=1) +
  labs(x="Sprint Number",
       y="Time (seconds)",
       title = "Model Estimates",
       subtitle = "Predicted values with 95% confidence intervals (ribbon)",
       color = "Disability") +
  guides(fill = "none") +
  scale_x_continuous(breaks = c(1:10)) +
  scale_y_continuous(breaks = c(2,3,4,5,6,7,8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9")) +
  facet_grid(.~distance) +
  theme_classic() +
  theme(legend.position = "bottom")

(individual_data_plot / model_data_plot) +
  plot_annotation(title = "Effects of Disability upon Repeated Sprint Times",
                  caption = "Model: Time ~ Disability * Sprint Number + (1 | Participant)")

ggsave("plots/disability_rsa.tiff", width = 7.5, height = 7.5, device = "tiff", dpi = 300)

}

# Make classification rsa plot

make_classif_rsa_plot <- function(data, preds) {
  individual_data_plot <- data %>%
    ggplot(aes(x=sprint_number, color=classif)) +
    geom_point(aes(y=time), size=0.5, alpha=0.25) +
    geom_smooth(aes(y=time, group=id), size=0.25, alpha=0.25,
                se=FALSE) +
    labs(x="Sprint Number",
         y="Time (seconds)",
         title = "Individual Data and Smooths",
         color = "Classification") +
    guides(fill = "none",
           color = "none") +
    scale_x_continuous(breaks = c(1:10)) +
    scale_y_continuous(breaks = c(2,3,4,5,6,7,8,9)) +
    scale_color_manual(values = c("#E69F00", "#56B4E9")) +
    facet_grid(.~distance) +
    theme_classic()
  
  model_data_plot <- preds %>%
    ggplot(aes(x=sprint_number, fill=classif)) +
    geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.25) +
    geom_line(aes(y=predicted, color=classif), size=1) +
    labs(x="Sprint Number",
         y="Time (seconds)",
         title = "Model Estimates",
         subtitle = "Predicted values with 95% confidence intervals (ribbon)",
         color = "Classification") +
    guides(fill = "none") +
    scale_x_continuous(breaks = c(1:10)) +
    scale_y_continuous(breaks = c(2,3,4,5,6,7,8)) +
    scale_color_manual(values = c("#E69F00", "#56B4E9")) +
    facet_grid(.~distance) +
    theme_classic() +
    theme(legend.position = "bottom")
  
  (individual_data_plot / model_data_plot) +
    plot_annotation(title = "Effects of Classification upon Repeated Sprint Times",
                    caption = "Model: Time ~ Classification * Sprint Number + (1 | Participant)")
  
  ggsave("plots/classif_rsa.tiff", width = 7.5, height = 7.5, device = "tiff", dpi = 300)
}



