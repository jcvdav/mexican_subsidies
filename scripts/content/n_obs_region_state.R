library(here)
library(fixest)
library(tidyverse)


shrimp <- read_csv(
  file = file.path(
    project_path, "data", "processed_data", "imputed_subsidy_economic_unit_annual_shrimp_panel.csv")) %>% 
  filter(between(year, 2012, 2019))


shrimp %>% 
  group_by(region, state) %>% 
  summarize(n_eu = n_distinct(eu_rnpa),
            n_obs = n()) %>% 
  arrange(region, desc(n_eu))
