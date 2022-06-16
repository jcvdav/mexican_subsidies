######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(tidyverse)

cpi_t <- readRDS(file.path(project_path, "data", "processed_data", "cpi_t_rates.rds"))

state_prices <- readRDS(
  file.path(
    project_path,
    "data",
    "processed_data",
    "annual_state_diesel_prices.rds")) %>% 
  rename(p_stat = mean_diesel_price_mxn_l) %>% 
  left_join(cpi_t, by = "year") %>% 
  mutate(p_stat = p_stat * rate) %>% 
  select(-rate)

nino <- readRDS(file.path(project_path, "data", "processed_data", "annual_nino34.rds"))

shrimp <- read_csv(
  file = file.path(
    project_path, "data", "processed_data", "imputed_subsidy_economic_unit_annual_shrimp_panel.csv")) %>% 
  filter(between(year, 2012, 2019)) %>% 
  rename(eu = eu_rnpa) %>% 
  left_join(state_prices, by = c("year", "state")) %>% 
  left_join(nino, by = "year") %>% 
  mutate(delta = -2 * treated ,
         dist = (fuel_consumption_l - predicted_subsidy_cap_l) / 1e3,
         norm_dist = dist / fuel_consumption_l,
         left = 1 * (fuel_consumption_l <= predicted_subsidy_cap_l),
         right = 1 - left,
         treated = 1 * treated) %>% 
  mutate(phi = subsidy_cap_l / fuel_consumption_l,
         D = phi < 1L,
         R = 2,
         term1 = pl + (D * R),
         term2 = phi * R  * D)

saveRDS(object = shrimp,
        file = file.path(project_path, "data", "processed_data", "shrimp_estimation_panel.rds"))

write_csv(x = shrimp,
          file = file.path(project_path, "data", "processed_data", "shrimp_estimation_panel.csv"))
