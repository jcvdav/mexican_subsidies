######################################################
#       create economic unit subsidy panel          #
######################################################

# Set up #####################################################################################################################
## Load packages
library(here)
library(tidyverse)

## Load data #################################################################################################################
fuel_caps <- read.csv(file.path(project_path, "data", "processed_data", "economic_unit_subsidy_caps.csv"),
                      stringsAsFactors = F)

## Create the panel ###########################################################################################################
economic_unit_panel <- fuel_caps %>%
  group_by(                                                             # Define groups
    year,
    eu_rnpa,
    # fishing_type,
    fuel_type#,
    # target,
    # n_large_scale_vessels,
    # n_small_scale_vessels,
    # zone
    # state,
    # municipality,
    # location
  ) %>%
  summarize(subsidy_cap_pesos = sum(subsidy_amount, na.rm = T)) %>%         # Calculate total subsidy amount for each economic unit
  ungroup() %>%                                                         # Undo groups
  mutate(applied = TRUE,
         treated = subsidy_cap_pesos > 0) %>% 
  complete(year, nesting(eu_rnpa, fuel_type),                                   # Balance the panel
           fill = list(subsidy_pesos = 0,                                       # filling it in with zeroes
                       treated = FALSE,                                         # filling it in with FALSE
                       applied = FALSE)) %>%                                    # filling it in with FALSE
  mutate(subsidy_cap_l = subsidy_cap_pesos / 2)                                 # Convert pesos to liters

# Export the data ##############################################################################################################
write.csv(x = economic_unit_panel,
          file = file.path(project_path, "data", "processed_data", "economic_unit_subsidy_panel.csv"),
          row.names = F)

# END OF SCRIPT ################################################################################################################
