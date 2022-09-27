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
shrimp_panel <- readRDS(file.path(project_path, "data", "processed_data", "shrimp_estimation_panel.rds")) %>% 
  mutate(treated = treated == 1)


subsidized_vessels <-
  shrimp_panel %>%
  count(year, treated) %>% 
  ggplot(aes(x = year, y = n, fill = treated)) +
  geom_col(color = "black") +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Year",
       y = "Number of\neconomic units",
       fill = "Subsidized")

mean_subsidy_amount <-
  shrimp_panel %>% 
  filter(treated) %>% 
  ggplot(aes(x = year, y = (subsidy_cap_l / total_hp))) +
  stat_summary(geom = "pointrange", fun.data = mean_sdl, fun.args = list(mult = 1),
               fill = "steelblue",
               shape = 21,
               size = 1) +
  labs(x = "Year",
       y = "Norm. subsidy cap\n(L / HP)")


total_liters <- 
  shrimp_panel %>% 
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
