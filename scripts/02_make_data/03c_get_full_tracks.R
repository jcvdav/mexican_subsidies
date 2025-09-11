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
  DBI,
  bigrquery,
  magrittr,
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
         fleet == "large scale") |> 
  select(eu_rnpa, vessel_rnpa, state, gear_type, main_engine_power_hp)

# tracks, filtered -------------------------------------------------------------
tracks <- tbl(mex_fisheries, vms) %>% 
  inner_join(vessel_registry, by = "vessel_rnpa") %>% 
  filter(between(year, 2011, 2024)) %>% 
  filter(between(implied_speed_knots, 1, 5)) %>% # Trawling occurs between 1 and 5 knots
  filter(between(depth_m, -100, -9.15)) %>%  # And at depths between 9.15m and 100m
  arrange(vessel_rnpa, datetime) %>% 
  select(vessel_rnpa, eu_rnpa, year, lat, lon, implied_speed_knots, depth_m, course, hours) %>% 
  mutate(year_outside = year)

shrimp_tracks <- tracks %>%
  collect() %>% 
  group_by(year_outside) %>% 
  nest()

# Build a function to write them out by year -----------------------------------
my_write <- function(year, data) {
  name <- here("data", "processed", paste0(year, "_shrimp_tracks.rds"))
  saveRDS(object = data,
          file = name)
}

## EXPORT ######################################################################
shrimp_tracks %$%
  walk2(year_outside, data, my_write)
