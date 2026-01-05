################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
# En 2023 se contabilizaron 244 mil 043 toneladas de camarón. https://www.gob.mx/agricultura/es/articulos/arranca-captura-de-camaron-de-altamar-2024-2025?idiom=es
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse
)

# Load data --------------------------------------------------------------------
# Landings data
landings <- readRDS(file = url("https://github.com/jcvdav/mex_fisheries/raw/c4427fa0b9ab18641dc1fe52afe4ca3c04263710/data/mex_landings/clean/mex_annual_landings_by_eu.rds")) # Points to c4427fa from late August 2025

## PROCESSING ##################################################################

# Filter shrimp only -----------------------------------------------------------
filtered <- landings %>%
  filter(main_species_group == "CAMARON") %>% 
  select(year, eu = eu_rnpa, live_weight, landed_weight)

## EXPORT ######################################################################
saveRDS(object = filtered,
        file = here("data", "processed", "shrimp_landings_panel.rds"))
