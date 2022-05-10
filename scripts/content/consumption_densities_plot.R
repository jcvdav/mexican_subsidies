library(here)
library(fixest)
library(modelsummary)
library(ggridges)
library(cowplot)
library(tidyverse)


shrimp <- read_csv(
  file = file.path(
    project_path, "data", "processed_data", "imputed_subsidy_economic_unit_annual_shrimp_panel.csv")) %>% 
  filter(between(year, 2012, 2019)) 

agg_left_plot <-
  shrimp %>% 
  filter(fuel_consumption_l <= predicted_subsidy_cap_l) %>% 
  ggplot(mapping = aes(x = fuel_consumption_l, fill = treated)) +
  geom_density(alpha = 0.5) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  lims(x = c(0, 1e6)) +
  labs(x = "Fuel consumption (L)",
       fill = "Subsidized",
       color = "Subsidized",
       subtitle = "Left of kink")

agg_right_plot <- shrim %>% 
  filter(fuel_consumption_l > predicted_subsidy_cap_l) %>% 
  ggplot(mapping = aes(x = fuel_consumption_l, fill = treated)) +
  geom_density(alpha = 0.5) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  lims(x = c(0, 1e6)) +
  labs(x = "Fuel consumption (L)",
       fill = "Subsidized",
       color = "Subsidized",
       subtitle = "Right of kink")

p2 <- plot_grid(agg_left_plot, agg_right_plot, ncol = 1)


ggsave(plot = p2,
       filename = here("results", "img", "consumption_densities_plot.pdf"),
       width = 8,
       height = 6)