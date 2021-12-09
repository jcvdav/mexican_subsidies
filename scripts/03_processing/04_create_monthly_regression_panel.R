######################################################
#               create regression panel              #
######################################################
# 
# Combines the panel on subsidy allocations with the
# one on fuel consmption
#
######################################################

## SET UP ##########################################################################################################################################

# Load packages
library(here)
library(startR)
library(tidyverse)

## Read data
# Fuel consumptiom
fuel_consumption_raw <-
  read_csv(file.path(project_path, "data", "processed_data", "vms_monthly_fuel_consumption.csv"))

# Subsidy panel
subsidy_panel_raw <-
  read_csv(file.path(project_path, "data", "processed_data", "economic_unit_subsidy_panel.csv"))

fuel_prices <- read_csv(file.path(project_path, "data", "raw_data", "monthly_diesel_prices.csv")) %>% 
  select(month = Month, price = Price) %>% 
  mutate(date = lubridate::mdy(month),
         month = lubridate::month(date),
         year = lubridate::year(date),
         fuel_type = "Diesel")

## PROCESSING ######################################################################################################################################
# Fuel consumption


fuel_consumption <- fuel_consumption_raw %>%
  group_by(year, month, eu_rnpa, fleet, fuel_type) %>% 
  summarize(total_hp = sum(engine_power_hp, na.rm = T),
            n_vessels = n_distinct(vessel_rnpa, na.rm = T),
            fuel_consumption_l = sum(fuel_consumption_l, na.rm = T),
            fuel_consumption_max_l = sum(fuel_consumption_max_l, na.rm = T),
            hours = sum(h, na.rm = T),
            tuna = sum(tuna) > 0,
            sardine = sum(sardine) > 0,
            shrimp = sum(shrimp) > 0,
            others = sum(others) > 0) %>% 
  ungroup()

# Fuel panel
subsidy_panel <- subsidy_panel_raw %>% 
  filter(fuel_type == "Diesel") %>%
  select(year,
         eu_rnpa,
         contains("subsidy"),
         fuel_type,
         treated,
         applied) %>% 
  group_by(eu_rnpa) %>% 
  mutate(n = n_distinct(fuel_type)) %>% 
  ungroup() %>% 
  filter(n == 1) %>% 
  select(-n)

  
panel <- fuel_consumption %>% 
  left_join(subsidy_panel, by = c("eu_rnpa", "year", "fuel_type")) %>% 
  left_join(fuel_prices, by = c("year", "month", "fuel_type")) %>% 
  replace_na(replace = list(subsidy_cap_l = 0, subsidy_cap_pv_l = 0, treated = F, applied = F)) %>%
  group_by(year, eu_rnpa) %>% 
  arrange(month) %>% 
  mutate(fuel_consumption_to_date_l = cumsum(fuel_consumption_l),
         exceeds_cap = fuel_consumption_to_date_l > subsidy_cap_l) %>% 
  ungroup() %>% 
  mutate(ph = price,
         pl = ph - 2,
         phi_to_date = subsidy_cap_l / fuel_consumption_to_date_l,
         D = phi_to_date <= 1L,
         R = ph - pl,
         term1 = pl + (D * R),
         term2 = (phi_to_date * R  * D)) %>% 
  filter(fuel_consumption_l > 0) %>% 
  filter(year <= 2019) %>% 
  select(year, month, date, eu_rnpa, fuel_consumption_l, fuel_consumption_to_date_l, subsidy_cap_l, subsidy_cap_pesos, exceeds_cap, ph, pl, phi_to_date, D, R, term1, term2, fleet, fuel_type, total_hp, n_vessels, tuna, sardine, shrimp, others, treated, applied)

write.csv(x = panel,
          file = file.path(project_path, "data", "processed_data", "monthly_estimation_panel.csv"),
          row.names = F)












