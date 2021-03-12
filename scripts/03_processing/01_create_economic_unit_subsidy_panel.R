
library(here)
library(tidyverse)

all_fuel_clean <- readRDS(here("data", "all_fuel_clean.rds"))

economic_unit_panel <- all_fuel_clean %>% 
  group_by(year, rnpa, fishing_type, fuel_type, target, n_large_scale_vessels, n_small_scale_vessels, zone, state, municipality, location) %>% 
  summarize(subsidy_amount = sum(subsidy_amount, na.rm = T)) %>% 
  ungroup() %>% 
  complete(year, nesting(rnpa, fishing_type), fill = list(subsidy_amount = 0))

saveRDS(economic_unit_panel, here("data", "economic_unit_subsidy_panel.rds"))
