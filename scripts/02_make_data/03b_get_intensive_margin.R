################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Combines the panel on subsidy allocations with the
# one on activity
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse
)

## Read data -------------------------------------------------------------------
# Subsidy allocations
# eu_subsidy_panel <- readRDS(
#   file = here("data",
#               "processed",
#               "economic_unit_subsidy_panel.rds"))

# # Fuel prices
# fuel_prices <- readRDS(
#   here(
#     "data",
#     "processed",
#     "annual_national_diesel_prices_2011_2020.rds"))

# Fuel consumption
vessel_activity_raw <- readRDS(
  file = here("data", "processed", "vms_annual_vessel_activity.rds"))

## PROCESSING ##################################################################
# Summarize vessel activity by economic unit -----------------------------------
eu_panel <- vessel_activity_raw %>%
  filter(fleet == "large scale") %>% 
  group_by(year, state, eu_rnpa, fleet, fuel_type) %>% 
  summarize(total_hp = sum(engine_power_hp),
            n_vessels = n_distinct(vessel_rnpa),
            hours = sum(hours, na.rm = T),
            tuna = 1 * (sum(tuna) > 0),
            sardine = 1 * (sum(sardine) > 0),
            shrimp = 1 * (sum(shrimp) > 0),
            others = 1 * (sum(others) > 0)) %>% 
  ungroup() %>% 
  mutate(state = str_to_sentence(state))

# Combine activity, fuel subsidies, and fuel prices ----------------------------
 # <- eu_activity %>% 
  # left_join(eu_subsidy_panel, by = c("eu_rnpa", "year")) %>% 
  # left_join(fuel_prices, by = "year") %>%
  # replace_na(replace = list(subsidy_cap_l = 0, treated = 0)) %>%
  # rename(mean_diesel_price = mean_diesel_price_mxn_l) %>%
  # select(-c(fleet, fuel_type))

# Keep shrimp EUs only ---------------------------------------------------------
shrimp_eus <- eu_panel %>% 
  group_by(eu_rnpa, state) %>% 
  summarize(shrimp = 1 * all(shrimp == 1),
            tuna = 1 * any(tuna == 1),
            sardine = 1 * any(sardine == 1),
            others = 1 * any(others == 1),
            n = n()) %>% 
  ungroup() %>% 
  filter(shrimp == 1, tuna == 0, sardine == 0, others == 0, n >= 2) %>% 
  pull(eu_rnpa)

# Create panels of "unique fishers"
shrimp <- eu_panel %>% 
  filter(eu_rnpa %in% shrimp_eus) %>% 
  mutate(region = case_when(state %in% c("Baja california", "Baja california sur", "Sinaloa", "Sonora", "Nayarit") ~ "GoC",
                            state %in% c("Campeche", "Tamaulipas", "Veracruz", "Quintana roo", "Yucatan") ~ "GoM",
                            state %in% c("Chiapas", "Oaxaca") ~ "Pacific")) 

## EXPORT ######################################################################
saveRDS(object = shrimp,
        file = here("data", "processed", "intensive_margin.rds"))


