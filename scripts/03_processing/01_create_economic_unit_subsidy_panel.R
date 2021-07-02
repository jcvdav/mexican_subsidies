######################################################
#       create economic unit subsidy panel          #
######################################################

# Set up #####################################################################################################################
## Load packages
library(here)
library(tidyverse)

## Load data #################################################################################################################
fuel_caps <- readRDS(here("data", "economic_unit_subsidy_caps.rds"))

## Creat the panel ###########################################################################################################
economic_unit_panel <- fuel_caps %>%
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
  summarize(subsidy_cap_pesos = sum(subsidy_amount, na.rm = T)) %>%         # Calculate total subsidy amount for each economic unit
  ungroup() %>%                                                         # Undo groups
  mutate(applied = TRUE,
         treated = subsidy_cap_pesos > 0) %>% 
  complete(year, nesting(rnpa, fishing_type),                                   # Balance the panel
           fill = list(subsidy_pesos = 0,                                       # filling it in with zeroes
                       treated = FALSE,                                         # filling it in with FALSE
                       applied = FALSE)) %>%                                    # filling it in with FALSE
  mutate(subsidy_cap_l = subsidy_cap_pesos / 2,                                 # Convert pesos to liters
         subsidy_cap_pv_pesos = subsidy_cap_pesos / n_large_scale_vessels,      # Calculate the per-vessel subsidy amount
         subsidy_cap_pv_l = subsidy_cap_pv_pesos / 2) %>%                           # Convert per vessel pesos to per vessel liters
  rename(eu_rnpa = rnpa)

# Export the data ##############################################################################################################
saveRDS(economic_unit_panel,
        here("data", "economic_unit_subsidy_panel.rds"))

# END OF SCRIPT ################################################################################################################
