######################################################
#       create economic unit subsidy panel          #
######################################################

# Set up #####################################################################################################################
## Load packages
library(here)
library(tidyverse)

## Load data #################################################################################################################
all_fuel_clean <- readRDS(here("data", "all_fuel_clean.rds"))

## Creat the panel ###########################################################################################################
economic_unit_panel <- all_fuel_clean %>%
  filter(fuel_type == "Diesel") %>%                                     # Keep diesel only        
  group_by(                                                             # Define groups
    year,
    rnpa,
    fishing_type,
    fuel_type,
    target,
    n_large_scale_vessels,
    n_small_scale_vessels,
    zone,
    state,
    municipality,
    location
  ) %>%
  summarize(subsidy_pesos = sum(subsidy_amount, na.rm = T)) %>%         # Calculate total subsidy amount for each economic unit
  ungroup() %>%                                                         # Undo groups
  complete(year, nesting(rnpa, fishing_type),                           # Balance the panel
           fill = list(subsidy_pesos = 0)) %>%                          #      filling it in with zeroes
  mutate(subsidy_pesos_pv = subsidy_pesos / n_large_scale_vessels,      # Calculate the per-vessel subsidy amount
         subsidy_liters = subsidy_pesos / 2,                            # Convert pesos to liters
         subsidy_liters_pv = subsidy_pesos_pv / 2) %>%                  # Convert per vessel pesos to per vessel liters
  rename(eu_rnpa = rnpa)

# Export the data ##############################################################################################################
saveRDS(economic_unit_panel,
        here("data", "economic_unit_subsidy_panel.rds"))

# END OF SCRIPT ################################################################################################################
