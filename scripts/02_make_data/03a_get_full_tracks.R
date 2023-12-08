# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  DBI,
  bigrquery,
  magrittr,
  tidyverse
)

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
         str_detect(gear_type, "ARRASTRE")) %>% 
  select(eu_rnpa, vessel_rnpa, state, gear_type, engine_power_hp)

# tracks, filtered -------------------------------------------------------------
tracks <- tbl(mex_fisheries, "mex_vms_processed_v_20231207") %>%# "mex_vms_processed_v_20231003") %>% # "mex_vms_processed_v_20220323") %>%
  inner_join(vessel_registry, by = "vessel_rnpa") %>% 
  filter(between(year, 2011, 2019)) %>% 
  filter(between(implied_speed_knots, 1, 5)) %>%
  filter(between(depth_m, -100, -9.15)) %>% 
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
