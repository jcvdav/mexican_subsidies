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
# Landings data
landings <- readRDS(
  file = file.path(
    "/Users/juancarlosvillasenorderbez/GitHub/",
    "data_mex_fisheries",
    "data",
    "mex_landings",
    "clean",
    "mex_annual_landings_by_eu.rds"
  )
)

## PROCESSING ##################################################################

# Filter shrimp only -----------------------------------------------------------
filtered <- landings %>%
  filter(main_species_group == "CAMARON") %>% 
  select(year, eu = eu_rnpa, landed_weight)

## EXPORT ######################################################################
saveRDS(object = filtered,
        file = here("data", "processed", "shrimp_landings_panel.rds"))
