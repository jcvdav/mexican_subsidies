library(here)
library(fixest)
library(tidyverse)


shrimp_panel <- readRDS(here("data", "estimation_panels", "shrimp_estimation_panel.rds"))

subsidy_variation <- shrimp_panel %>% 
  filter(treated == 1) %>% 
  ggplot(mapping = aes(x = year, y = log(subsidy_pesos), group = eu)) +
  geom_line(size = 0.1, alpha = 0.3) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = 2011:2019) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Year",
       y = expression("log(Subsidy"~MXP[2019]~")")) 

effort_variation <- shrimp_panel %>% 
  filter(always == 1) %>% 
  ggplot(mapping = aes(x = year, y = log(hours), group = eu)) +
  geom_line(size = 0.1, alpha = 0.3) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = 2011:2019) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Year",
       y = expression("log(Subsidy"~MXP[2019]~")"),
       color = "Always subsidized") 

ggsave(plot = subsidy_variation,
       filename = here("results", "img", "cap_always_subsidized.pdf"),
       width = 6,
       height = 3)
