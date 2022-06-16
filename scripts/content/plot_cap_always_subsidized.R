library(here)
library(fixest)
library(tidyverse)


shrimp_panel <- readRDS(file.path(project_path, "data", "processed_data", "shrimp_estimation_panel.rds")) %>% 
  mutate(treated = treated == 1)


by_numbers <- shrimp_panel %>% 
  filter(treated) %>%
  group_by(eu, treated) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n == 8) %>% 
  mutate(dist = (fuel_consumption_l - predicted_subsidy_cap_l) / 1000,
         rel_dist = dist / predicted_subsidy_cap_l,
         left = fuel_consumption_l <= subsidy_cap_l)

variation <- shrimp_panel %>% 
  filter(eu %in% by_numbers$eu) %>% 
  ggplot(mapping = aes(x = year, y = log(subsidy_cap_l), group = eu)) +
  geom_line(size = 0.1, alpha = 0.3) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Year",
       y = "log[Subsidy cap (L)]",
       color = "Always subsidized") 

ggsave(plot = variation,
       filename = here("results", "img", "cap_always_subsidized.pdf"),
       width = 6,
       height = 3)
