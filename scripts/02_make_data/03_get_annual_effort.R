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

source(here("scripts/00_setup.R"))

# Authenticate using local token -----------------------------------------------
bq_auth("juancarlos.villader@gmail.com")

# Establish a connection to BigQuery -------------------------------------------
mex_fisheries <- dbConnect(
  bigquery(),
  project = "mex-fisheries",
  dataset = "mex_vms",
  billing = "mex-fisheries",
  use_legacy_sql = FALSE,
  allowLargeResults = TRUE
)


## PROCESSING ##################################################################

# vessel registry --------------------------------------------------------------
vessel_registry <- tbl(mex_fisheries, vi) %>%
  group_by(vessel_rnpa) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n == 1,
         # Keep only vessels that exclusively target shrimp
         target_finfish == 0,
         target_sardine == 0,
         target_shark == 0,
         target_shrimp == 1,
         target_tuna == 0,
         target_other == 0,
         # Keep only vessels that only use trawl nets
         gear_trawler == 1,
         gear_purse_seine == 0,
         gear_longline == 0,
         fuel_type == "Diesel",
         fleet == "large scale")

# tracks, filtered -------------------------------------------------------------
tracks <- tbl(mex_fisheries, vms) %>% 
  filter(between(year, 2011, 2019)) %>% 
  filter(between(implied_speed_knots, 1, 5)) %>% # Trawling occurs between 1 and 5 knots
  filter(between(depth_m, -100, -9.15)) %>%  # And at depths between 9.15m and 100m
  select(-economic_unit)

# Annual -----------------------------------------------------------------------
annual_activity <- tracks %>%
  inner_join(vessel_registry, by = "vessel_rnpa") %>%                                                                              # Add vessel info from the registry
  group_by(
    vessel_rnpa,
    eu_rnpa,
    state,
    year,
    main_engine_power_hp,
    # target_finfish,
    # target_sardine,
    # target_shark,
    # target_shrimp,
    # target_tuna,
    # target_other,
    fleet,
    fuel_type) %>%
  summarize(hours = sum(hours, na.rm = T)) %>%
  ungroup()

# Collect the query ------------------------------------------------------------
annual_activity_local <- annual_activity %>%
  collect() %>%
  drop_na(main_engine_power_hp)

## EXPORT ######################################################################
saveRDS(object = annual_activity_local,
        file = here("data", "processed", "vms_annual_vessel_activity.rds"))
