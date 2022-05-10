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
library(tidyverse)

## Read data
# Fuel consumptiom
fuel_consumption_raw <-
  read.csv(file.path(project_path, "data", "processed_data", "vms_annual_fuel_consumption.csv"),
           stringsAsFactors = F)

# Subsidy panel
vessel_subsidy_panel_raw <- 
  read.csv(file.path(project_path, "data", "processed_data", "vessel_subsidy_panel.csv"),
           stringsAsFactors = F)

eu_subsidy_panel_raw <- 
  read.csv(file.path(project_path, "data", "processed_data", "economic_unit_subsidy_panel.csv"),
           stringsAsFactors = F)
# These commented-out pirces are wrong
# fuel_prices <- read.csv(file.path(project_path, "data", "raw_data", "monthly_diesel_prices.csv")) %>% 
#   select(month = Month, price = Price) %>% 
#   mutate(date = lubridate::mdy(month),
#          month = lubridate::month(date),
#          year = lubridate::year(date),
#          fuel_type = "Diesel") %>% 
#   group_by(year) %>% 
#   summarize(mean_diesel_price_mxn_l = mean(price, na.rm = T))

fuel_prices <- 
  readRDS(
    file = file.path(
      project_path,
      "data",
      "processed_data",
      "annual_national_diesel_prices.rds"
    )
  )


## PROCESSING ######################################################################################################################################
# Fuel consumption
vessel_fuel_consumption <- fuel_consumption_raw %>%
  filter(fleet == "large scale") %>% 
  select(year, eu_rnpa, vessel_rnpa, fleet, engine_power_hp, engine_power_bin_hp, tuna, sardine, shrimp, others, fuel_type, hours, fuel_consumption_l, fuel_consumption_max_l)

eu_fuel_consumption <- fuel_consumption_raw %>%
  filter(fleet == "large scale") %>% 
  group_by(year, eu_rnpa, fleet, fuel_type) %>% 
  summarize(total_hp = sum(engine_power_hp),
            n_vessels = n_distinct(vessel_rnpa),
            fuel_consumption_l = sum(fuel_consumption_l, na.rm = T),
            fuel_consumption_max_l = sum(fuel_consumption_max_l, na.rm = T),
            hours = sum(hours, na.rm = T),
            tuna = sum(tuna) > 0,
            sardine = sum(sardine) > 0,
            shrimp = sum(shrimp) > 0,
            others = sum(others) > 0) %>% 
  ungroup()


# Fuel panel
vessel_subsidy_panel <- vessel_subsidy_panel_raw %>% 
  select(year, vessel_rnpa, state, subsidy_cap_l) %>% 
  mutate(treated = T)

eu_subsidy_panel <- eu_subsidy_panel_raw %>% 
  filter(fuel_type == "Diesel") %>% 
  select(year, eu_rnpa, subsidy_cap_l) %>% 
  mutate(treated = subsidy_cap_l > 0)

# Combine
vessel_panel <- vessel_fuel_consumption %>% 
  left_join(vessel_subsidy_panel, by = c("eu_rnpa" = "vessel_rnpa", "year")) %>% 
  left_join(fuel_prices, by = "year") %>% 
  replace_na(replace = list(subsidy_cap_l = 0, treated = F)) %>%
  rename(ph = mean_diesel_price_mxn_l) %>% 
  mutate(phi = subsidy_cap_l / fuel_consumption_l,
         q_div_qbar = fuel_consumption_l / subsidy_cap_l,
         left_of_kink = fuel_consumption_l < subsidy_cap_l,
         pl = ph - 2,
         p = ph - (2 * left_of_kink))

eu_panel <- eu_fuel_consumption %>% 
  left_join(eu_subsidy_panel, by = c("eu_rnpa", "year")) %>% 
  left_join(fuel_prices, by = "year") %>% 
  replace_na(replace = list(subsidy_cap_l = 0, treated = F)) %>%
  rename(ph = mean_diesel_price_mxn_l) %>% 
  mutate(phi = subsidy_cap_l / fuel_consumption_l,
         q_div_qbar = fuel_consumption_l / subsidy_cap_l,
         left_of_kink = fuel_consumption_l < subsidy_cap_l,
         pl = ph - 2,
         p = ph - (2 * left_of_kink * treated)) %>% 
  select(-c(fleet, fuel_type))

# Shrimp only
shrimp_eus <- eu_panel %>% 
  group_by(eu_rnpa) %>% 
  summarize(shrimp = all(shrimp),
            tuna = any(tuna),
            sardine = any(sardine),
            others = any(others),
            n = n()) %>% 
  filter(shrimp, !tuna, !sardine, !others, n >= 2) %>% 
  pull(eu_rnpa)

# Create panels of "unique fishers"
shrimp <- eu_panel %>% 
  filter(eu_rnpa %in% shrimp_eus)


write_csv(x = eu_panel,
          file = file.path(project_path, "data", "processed_data", "economic_unit_annual_panel.csv"))

write_csv(x = shrimp,
          file = file.path(project_path, "data", "processed_data", "shrimp_economic_unit_annual_panel.csv"))
