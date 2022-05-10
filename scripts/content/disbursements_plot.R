######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(here)
library(cowplot)
library(tidyuverse)

# Read data
shrimp <- read_csv(
  file = file.path(
    project_path, "data", "processed_data", "imputed_subsidy_economic_unit_annual_shrimp_panel.csv")) %>% 
  filter(between(year, 2012, 2019)) %>% 
  mutate(extra_l = pmax(0, fuel_consumption_l - predicted_subsidy_cap_l) / 1e3)




subsidized_vessels <-
  shrimp %>%
  count(year, treated) %>% 
  ggplot(aes(x = year, y = n, fill = treated)) +
  geom_col(color = "black") +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Year",
       y = "Number of\neconomic units",
       fill = "Subsidized")

mean_subsidy_amount <-
  shrimp %>% 
  filter(treated) %>% 
  ggplot(aes(x = year, y = subsidy_cap_l / 1e6)) +
  stat_summary(geom = "pointrange", fun.data = "mean_se",
               fill = "steelblue",
               shape = 21,
               size = 1) +
  labs(x = "Year",
       y = "Mean subsidy cap\n(Million L)")


total_liters <- 
  shrimp %>% 
  group_by(year) %>% 
  summarize(tot = sum(subsidy_cap_l) / 1e6) %>% 
  ungroup() %>% 
  ggplot(aes(x = year, y = tot)) +
  geom_col() +
  labs(x = "Year",
       y = "Total subsidy\n(Million L)")





disbursements <- 
  plot_grid(subsidized_vessels, mean_subsidy_amount, total_liters,
          ncol =1,
          labels = "AUTO",
          label_x = 0.95)


ggsave(plot = disbursements,
       filename = here("results", "img", "disbursements_plot.pdf"),
       width = 6,
       height = 6.75)
