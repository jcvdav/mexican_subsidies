################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Combines the panel on subsidy allocations with the
# one on fuel consmption
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
library(here)
library(tidyverse)

## Read data -------------------------------------------------------------------
# Subsidy allocations
eu_subsidy_panel_raw <- readRDS(
  file = here("data",
              "processed",
              "economic_unit_subsidy_panel.rds"))

# Fuel prices
fuel_prices <- readRDS(
  here(
    "data",
    "processed",
    "annual_national_diesel_prices_2011_2020.rds"))

# Fuel consumption
fuel_consumption_raw <- readRDS(
  file = here("data",
              "processed",
              "vms_annual_fuel_consumption.rds"))

## PROCESSING ##################################################################
# Fuel consumption
eu_fuel_consumption <- fuel_consumption_raw %>%
  filter(fleet == "large scale") %>% 
  group_by(year, state, eu_rnpa, fleet, fuel_type) %>% 
  summarize(total_hp = sum(engine_power_hp),
            n_vessels = n_distinct(vessel_rnpa),
            fuel_consumption_l = sum(fuel_consumption_l, na.rm = T),
            hours = sum(hours, na.rm = T),
            fishing_hours = sum(fishing_hours, na.rm = T),
            tuna = sum(tuna) > 0,
            sardine = sum(sardine) > 0,
            shrimp = sum(shrimp) > 0,
            others = sum(others) > 0) %>% 
  ungroup() %>% 
  mutate(state = str_to_sentence(state))


# Fuel subsidypanel
eu_subsidy_panel <- eu_subsidy_panel_raw %>% 
  filter(fuel_type == "Diesel") %>% 
  select(year, eu_rnpa, subsidy_cap_l) %>% 
  mutate(treated = subsidy_cap_l > 0)

# Combine
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
  group_by(eu_rnpa, state) %>% 
  summarize(shrimp = all(shrimp),
            tuna = any(tuna),
            sardine = any(sardine),
            others = any(others),
            n = n()) %>% 
  ungroup() %>% 
  filter(shrimp, !tuna, !sardine, !others, n >= 2) %>% 
  pull(eu_rnpa)

# Create panels of "unique fishers"
shrimp <- eu_panel %>% 
  filter(eu_rnpa %in% shrimp_eus) %>% 
  mutate(region = case_when(state %in% c("Baja california", "Baja california sur", "Sinaloa", "Sonora", "Nayarit") ~ "GoC",
                            state %in% c("Campeche", "Tamaulipas", "Veracruz", "Quintana roo", "Yucatan") ~ "GoM",
                            state %in% c("Chiapas", "Oaxaca") ~ "Pacific")) 

## EXPORT ######################################################################
saveRDS(object = eu_panel,
        file = here("data", "processed", "economic_unit_annual_panel.rds"))

saveRDS(object = shrimp,
        file = here("data", "processed", "shrimp_economic_unit_annual_panel.rds"))


