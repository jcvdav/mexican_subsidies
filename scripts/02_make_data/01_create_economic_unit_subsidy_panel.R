################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
library(here)
library(tidyverse)

# Load data --------------------------------------------------------------------
fuel_caps <- readRDS(
  file = here("data",
              "processed",
              "economic_unit_subsidy_caps.rds"))

## PROCESSING ##################################################################

# Create the panel -------------------------------------------------------------
economic_unit_panel <- fuel_caps %>%
  group_by(                                                             # Define groups
    year,
    eu_rnpa,
    fuel_type
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

## EXPORT ######################################################################
saveRDS(object = economic_unit_panel,
        file = here("data", "processed", "economic_unit_subsidy_panel.rds"))
