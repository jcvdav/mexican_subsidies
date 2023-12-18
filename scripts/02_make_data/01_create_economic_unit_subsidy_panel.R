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
pacman::p_load(
  here,
  tidyverse
)

# Load data --------------------------------------------------------------------
cpi_t <- readRDS(
  file = here("data", "processed", "cpi_t_rates.rds"))

subsidy_caps <- readRDS(
  file = here("data",
              "processed",
              "economic_unit_subsidy_caps.rds"))

## PROCESSING ##################################################################
# Create the panel -------------------------------------------------------------
economic_unit_panel <- subsidy_caps %>%
  filter(fuel_type == "Diesel") %>% 
  group_by(                                                             # Define groups
    year,
    eu_rnpa
  ) %>%
  summarize(subsidy_pesos = sum(subsidy_amount, na.rm = T)) %>%         # Calculate total subsidy amount for each economic unit
  ungroup() %>%                                                         # Undo groups
  mutate(treated = 1 * (subsidy_pesos > 0)) %>% 
  complete(year, nesting(eu_rnpa),                                              # Balance the panel
           fill = list(subsidy_pesos = 0,                                       # filling it in with zeroes
                       treated = FALSE)) %>%                                    # filling it in with FALSE
  left_join(cpi_t, by = "year") %>%
  mutate(subsidy_pesos = subsidy_pesos * rate) %>%
  select(-rate)

## EXPORT ######################################################################
saveRDS(object = economic_unit_panel,
        file = here("data", "processed", "economic_unit_subsidy_panel.rds"))
