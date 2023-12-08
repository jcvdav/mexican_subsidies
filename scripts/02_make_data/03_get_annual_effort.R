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
# Filter for depths between 9.15 m (minimum legal) 
# como una zona de refugio para la protección de diversas especies
# biológicas en la franja marina de la 0 a 9.15 m de profundidad, en
# donde está prohibida la pesca con el sistema de arrastre.
#
# Actualmente, hay cerca de 1,260 buques operando sobre
# la plataforma continental en zonas desde 9.15 m de profundidad
# hasta 100 m aproximadamente
# Info from
# Shrimp fishing in Mexico. Based on the work of D. Aguilar and J. Grande-Vidal https://www.fao.org/3/i0300e/i0300e02b.pdf
# INAPESCA: Redes de arrastre Cataologo sistemas de captura: https://www.inapesca.gob.mx/portal/documentos/publicaciones/CATALOGO%20DE%20SISTEMAS%20DE%20CAPTURA/CapI_Arrastre.pdf
# Villaseñor-Talavera. Capítulo 15. Pesca de camarón con sistema de arrastre y cambios tecnológicos implementados para mitigar sus efectos en el ecosistema.
#  y Sistema de Localización Satelital, este último es obligatorio para todas las embarcaciones mayores, especificado en la NOM-062-SAG/PESC-2014.
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  DBI,
  bigrquery,
  tidyverse
)

# Define some parameters -------------------------------------------------------
diesel_density <- 0.9 #ASTM D975 standard
hp_kw <- 0.7457

# Authenticate using local token -----------------------------------------------
bq_auth("juancarlos@ucsb.edu")

# Establish a connection to BigQuery -------------------------------------------
mex_fisheries <- dbConnect(
  bigquery(),
  project = "emlab-gcp",
  dataset = "mex_fisheries",
  billing = "emlab-gcp",
  use_legacy_sql = FALSE,
  allowLargeResults = TRUE
)


## PROCESSING ##################################################################

# vessel registry --------------------------------------------------------------
vessel_registry <- tbl(mex_fisheries, "vessel_info_v_20230803") %>% # "vessel_info_v_20221104") %>%
  group_by(vessel_rnpa) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n == 1,
         shrimp == 1, tuna == 0, sardine == 0, others == 0,
         fuel_type == "Diesel",
         str_detect(gear_type, "ARRASTRE"))

# tracks, filtered -------------------------------------------------------------
tracks <- tbl(mex_fisheries, "mex_vms_processed_v_20231207") %>% #"mex_vms_processed_v_20231003") %>% # "mex_vms_processed_v_20220323") %>%
  filter(between(implied_speed_knots, 1, 5)) %>%
  filter(between(depth_m, -100, -9.15)) %>% 
  select(-economic_unit)

# Annual -----------------------------------------------------------------------
annual_fuel_consumption <- tracks %>%
  inner_join(vessel_registry, by = "vessel_rnpa") %>%                                                                              # Add vessel info from the registry
  mutate(
    loading_factor = 0.9 * ((((implied_speed_knots / design_speed_kt) ^ 3) + (0.2 / (0.9 - 0.2))) / (1 + (0.2 / (0.9 - 0.2)))),# Calculate engine loading
    fuel_grams = hours * loading_factor * engine_power_hp * hp_kw * sfc_gr_kwh # Calculate fuel consumption
  ) %>%
  group_by(
    vessel_rnpa,
    eu_rnpa,
    state,
    year,
    engine_power_hp,
    engine_power_bin_hp,
    tuna,
    sardine,
    shrimp,
    others,
    fleet,
    fuel_type
  ) %>%                # Group daily (with characteristics)
  summarize(
    # Calculate total daily grams
    hours = sum(hours, na.rm = T),
    fishing_hours = sum(hours[between(implied_speed_knots, 1, 5) & between(depth_m, -100, -9.15)], na.rm = T),
    fuel_grams = sum(fuel_grams, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(fuel_consumption_l = (fuel_grams / 1e3) / diesel_density)              # Convert grams to liters


# Collect the query ------------------------------------------------------------
annual_fuel_consumption_local <- annual_fuel_consumption %>%
  collect() %>%
  drop_na(engine_power_hp)

## EXPORT ######################################################################
saveRDS(object = annual_fuel_consumption_local,
        file = here("data", "processed", "vms_annual_vessel_activity.rds"))
