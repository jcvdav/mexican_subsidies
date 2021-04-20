######################################################
#              Download fishing effort               #
######################################################

## Set up #############################################################################################################################################################################
# Load packages
library(here)
library(startR)
library(connections)
library(bigrquery)
library(tidyverse)

# Authenticate using local token 
bq_auth("juancarlos@ucsb.edu")

bq_con <- DBI::dbConnect(
  bigquery(),
  project = "world-fishing-827"
)

# Establish a connection to BigQuery
mxn_vms <- connection_open(
  bigquery(),
  project = "world-fishing-827",
  dataset = "pipe_mexico_production_v20190128",
  billing = "emlab-gcp",
  use_legacy_sql = FALSE,
  allowLargeResults = TRUE
)

# This is a case-specific modified version of startR::bq_unnest
bq_unnest <- function (input_tbl, select_columns, array_column) {
  db_connection <- input_tbl$src$con
  select_columns <- paste0(select_columns, collapse = ", ")
  sql_query <- dbplyr::build_sql(con = db_connection, "SELECT ", 
                                 dbplyr::sql(select_columns), ", ", "ocean", 
                                 "\n", "FROM \n", dbplyr::sql(input_tbl$ops$x), "\n", 
                                 "CROSS JOIN \n", "UNNEST(", dbplyr::sql(array_column), 
                                 ") AS ocean")
  return(dplyr::tbl(db_connection, dbplyr::sql(sql_query)))
}

## START DEFINING TABLES #################################################################################################################################################################
# good vms segments
good_vms_segs <- tbl(mxn_vms, "segment_info") %>% 
  filter(!noise) %>% 
  select(seg_id)


# veszel registry
vessel_registry <- tbl(mxn_vms, "emlab-gcp.jc_mxn_subsidies.vessel_registry") %>% 
  mutate(n_rnp = sql("IF(CHAR_LENGTH(CAST(vessel_rnpa AS string)) > 8,
                         CAST(vessel_rnpa AS string),
                         CONCAT(REPEAT('0',8 - CHAR_LENGTH(CAST(vessel_rnpa AS string))), CAST(vessel_rnpa AS string)))"))   # Convert vessel_rnpa to character

# tracks_fixed
tracks_fixed <- tbl(mxn_vms, "messages_scored_*") %>% 
  bq_unnest(select_columns = c("ssvid", "timestamp", "seg_id", "rnp", "distance_from_shore_m", "speed"),                     # Unnest and keep ocean column
            array_column = "regions.ocean") %>% 
  filter(distance_from_shore_m > 100) %>%                                                                                    # Filter positions that are within 100 m of the coast
  inner_join(good_vms_segs, by = "seg_id") %>%                                                                               # Filter out noisy segments
  mutate(n_rnp = sql("IF (CHAR_LENGTH(rnp) > 8,
                         rnp,
                         CONCAT(REPEAT('0', 8 - CHAR_LENGTH(rnp)), rnp))"))                                                  # Fix broken character string (should have 8 digits with trailin 0)

# tracks
fuel_consumption <- tracks_fixed %>% 
  inner_join(vessel_registry, by = "n_rnp") %>%                                                                              # Add vessel info from the registry
  mutate(vessel_rnpa = n_rnp,                                                                                                # Rename
         species = target_species,                                                                                           # Rename
         eu_rnpa = as.character(eu_rnpa),                                                                                    # Rename and change class
         year = sql("EXTRACT(YEAR from timestamp)"),                                                                         # Use sql to extract the year
         month = sql("EXTRACT(MONTH from timestamp)"),                                                                       # Use sql to extract the month
         date = sql("EXTRACT(DATE from timestamp)"),                                                                         # Use sql to extract the date
         loading_factor = 0.9 * ((((speed/design_speed_kt) ^ 3) + (0.2 / (0.9 - 0.2)))/(1 + (0.2 / (0.9 - 0.2)))),           # Calculate engine loading
         fuel_grams = loading_factor * engine_power_hp * 0.7457 * sfc_gr_kwh) %>%                                            # Calculate fuel consumption
  group_by(vessel_rnpa, eu_rnpa, ocean, year, month, date, engine_power_hp, engine_power_bin_hp, species) %>%                # Group daily (with characteristics)
  summarize(fuel_grams = sum(fuel_grams, na.rm = T)) %>%                                                                     # Calculate total daily grams
  ungroup() %>%                                                                                                              # Ungroup
  mutate(fuel_liters = (fuel_grams / 1000) / 0.91)                                                                           # Convert grams to liters

# Collect the query
fuel_consumption_local <- fuel_consumption %>% 
  collect()

## Save data #############################################################################################################################################################################
saveRDS(object = fuel_consumption_local,
        file = here("data", "vms_daily_fuel_consumption.rds"))


## END OF SCRIPT #########################################################################################################################################################################















