######################################################
#              Download fishing effort               #
######################################################

## Set up #############################################################################################################################################################################
# Load packages
library(connections)
library(bigrquery)
library(tidyverse)

# Define some parameters
diesel_density <- 0.9 #ASTM D975 standard
hp_kw <- 0.7457

# Authenticate using local token 
bq_auth("juancarlos@ucsb.edu")

# Establish a connection to BigQuery
mex_fisheries <- connection_open(
  bigquery(),
  project = "emlab-gcp",
  dataset = "mex_fisheries",
  billing = "emlab-gcp",
  use_legacy_sql = FALSE,
  allowLargeResults = TRUE
)


## START DEFINING TABLES #################################################################################################################################################################

# vessel registry
vessel_registry <- tbl(mex_fisheries, "vessel_info") %>% 
  group_by(vessel_rnpa) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  filter(n == 1,
         fuel_type == "Diesel")

# tracks, filtered
tracks <- tbl(mex_fisheries, "mex_vms_processed_v_20220323") %>% 
  filter(speed > 0) %>% 
  select(-economic_unit)

# Annual
annual_fuel_consumption <- tracks %>% 
  inner_join(vessel_registry, by = "vessel_rnpa") %>%                                                                              # Add vessel info from the registry
  mutate(loading_factor = 0.9 * ((((speed/design_speed_kt) ^ 3) + (0.2 / (0.9 - 0.2)))/(1 + (0.2 / (0.9 - 0.2)))),           # Calculate engine loading
         fuel_grams = hours * loading_factor * engine_power_hp * hp_kw * sfc_gr_kwh,                                                # Calculate fuel consumption
         fuel_grams_max =  1.2 * hours * engine_power_hp * hp_kw * 280
  ) %>% 
  group_by(vessel_rnpa, eu_rnpa, state, year, engine_power_hp, engine_power_bin_hp, tuna, sardine, shrimp, others, fleet, fuel_type) %>%                # Group daily (with characteristics)
  summarize(hours = sum(hours, na.rm = T),
            fuel_grams = sum(fuel_grams, na.rm = T),
            fuel_grams_max = sum(fuel_grams_max, na.rm = T)) %>%                                                                        # Calculate total daily grams
  ungroup() %>%                                                                                                              # Ungroup
  mutate(fuel_consumption_l = (fuel_grams / 1000) / diesel_density,
         fuel_consumption_max_l = (fuel_grams_max / 1000) / diesel_density)                                                                      # Convert grams to liters


# Collect the query
annual_fuel_consumption_local <- annual_fuel_consumption %>% 
  collect() %>% 
  drop_na(engine_power_hp)

## Save data #############################################################################################################################################################################
write.csv(x = annual_fuel_consumption_local,
          file.path(project_path, "data", "processed_data", "vms_annual_fuel_consumption.csv"),
          row.names = F)

## END OF SCRIPT #########################################################################################################################################################################















