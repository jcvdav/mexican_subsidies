# Load packages
pacman::p_load(
  here,
  tidyverse,
  broom,
  fixest
)

# Load models
semi_mod <- readRDS(file = here("data/output/semi_elasticity_twfe_model.rds"))
elasticity_mod <- readRDS(here("data/output/elasticity_twfe_model.rds"))

# Extract coefficients for fishing time (log(hours))
semi_coef <- tidy(semi_mod$`Fishing time`, conf.int = TRUE) %>%
  filter(term == "treated") %>%
  mutate(model_type = "Semi-elasticity",
         var = "Fishing time")

elasticity_coef <- tidy(elasticity_mod$`Fishing time`, conf.int = TRUE) %>%
  filter(term == "log(subsidy_pesos)") %>%
  mutate(model_type = "Elasticity",
         var = "Fishing time")

# Create a range of % changes in subsidy amount for visualization
subsidy_changes <- seq(0, 100, by = 1)  

# Calculate predicted changes in fishing time for each model type
# Semi-elasticity (log-linear): log(y) = β * treated
# When treated goes from 0 to 1: % change in y = (exp(β) - 1) * 100
semi_elasticity_effect <- (exp(semi_coef$estimate) - 1) * 100

semi_elasticity_pred <- data.frame(
  subsidy_change_pct = subsidy_changes,
  fishing_time_change_pct = rep(semi_elasticity_effect, length(subsidy_changes)),
  model_type = "Semi-elasticity"
)

# Elasticity (log-log): log(y) = β * log(x)
# % change in y = β * % change in x (this is correct as is)
elasticity_pred <- data.frame(
  subsidy_change_pct = subsidy_changes,
  fishing_time_change_pct = elasticity_coef$estimate * subsidy_changes,
  model_type = "Elasticity"
)

# Combine predictions
combined_pred <- bind_rows(semi_elasticity_pred, elasticity_pred) %>%
  mutate(model_type = factor(model_type, levels = c("Elasticity", "Semi-elasticity")))

# Create comparison plot showing the relationship
p <- ggplot(combined_pred, aes(x = subsidy_change_pct, y = fishing_time_change_pct, 
                              color = model_type)) +
  geom_line(size = 1.2) +
  labs(
    title = "Elasticity vs Semi-elasticity: Effect of Subsidy Changes on Fishing Time",
    subtitle = "Semi-elasticity shows constant effect of receiving subsidy; Elasticity shows proportional effect",
    x = "% Change in Subsidy Amount",
    y = "% Change in Fishing Time",
    color = "Model Type",
    caption = "Semi-elasticity: constant effect of receiving any subsidy vs. none\nElasticity: proportional effect that scales with subsidy amount"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    legend.position = "bottom"
  ) +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))

# Display the plot
print(p)

# Save the plot
ggsave(plot = p,
       filename = here("content", "figures", "fig_elasticity_vs_semi_elasticity.pdf"),
       width = 10,
       height = 6,
       units = "in")

