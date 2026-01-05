library(here)
library(ggridges)
library(cowplot)
library(tidyverse)

source(here("scripts", "00_setup.R"))

## Load data ###################################################################

shrimp_panel <- readRDS(file.path(project_path, "data", "processed_data", "shrimp_estimation_panel.rds")) %>% 
  mutate(treated = treated == 1)

agg_left_plot <-
  shrimp_panel %>% 
  filter(fuel_consumption_l <= predicted_subsidy_cap_l) %>% 
  ggplot(mapping = aes(x = log(fuel_consumption_l), fill = treated)) +
  geom_density(alpha = 0.5) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  xlim(0, 16) +
  labs(x = "log(Fuel consumption)",
       y = "Density",
       fill = "Subsidized",
       color = "Subsidized",
       subtitle = "Left of kink")

agg_right_plot <-
  shrimp_panel %>% 
  filter(fuel_consumption_l > predicted_subsidy_cap_l) %>% 
  ggplot(mapping = aes(x = log(fuel_consumption_l), fill = treated)) +
  geom_density(alpha = 0.5) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  xlim(0, 16) +
  labs(x = "log(Fuel consumption)",
       y = "Density",
       fill = "Subsidized",
       color = "Subsidized",
       subtitle = "Right of kink")

p2 <- plot_grid(agg_left_plot, agg_right_plot, ncol = 1)

ggsave(plot = p2,
       filename = here("results", "img", "consumption_densities_plot.pdf"),
       width = 8,
       height = 6)
