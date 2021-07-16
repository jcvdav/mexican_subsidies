######################################################
#              Download fishing effort               #
######################################################

## Set up #############################################################################################################################################################################
# Load packages
library(startR)
library(connections)
library(bigrquery)
library(tidyverse)

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
  filter(n == 1)

# tracks, filtered
tracks <- tbl(mex_fisheries, "mex_vms") %>% 
  filter(speed > 0) %>%                                                                                     # Filter positions that are within 100 m of the coast
  select(-economic_unit)


# tracks
fuel_consumption <- tracks %>% 
  inner_join(vessel_registry, by = "vessel_rnpa") %>%                                                                              # Add vessel info from the registry
  mutate(species = target_species,                                                                                           # Rename
         # eu_rnpa = as.character(eu_rnpa),                                                                                    # Rename and change class
         loading_factor = 0.9 * ((((speed/design_speed_kt) ^ 3) + (0.2 / (0.9 - 0.2)))/(1 + (0.2 / (0.9 - 0.2)))),           # Calculate engine loading
         fuel_grams = loading_factor * engine_power_hp * 0.7457 * sfc_gr_kwh,                                                # Calculate fuel consumption
         fuel_grams_max =  1.2 * engine_power_hp * 0.7457 * 280
  ) %>% 
  group_by(vessel_rnpa, eu_rnpa, year, engine_power_hp, engine_power_bin_hp, species, fleet, fuel_type) %>%                # Group daily (with characteristics)
  summarize(h = n(),
            fuel_grams = sum(fuel_grams, na.rm = T),
            fuel_grams_max = sum(fuel_grams_max, na.rm = T)) %>%                                                                        # Calculate total daily grams
  ungroup() %>%                                                                                                              # Ungroup
  mutate(fuel_consumption_l = (fuel_grams / 1000) / 0.91,
         fuel_consumption_max_l = (fuel_grams_max / 1000) / 0.91)                                                                      # Convert grams to liters
    
# Collect the query
fuel_consumption_local <- fuel_consumption %>% 
  collect()

## Save data #############################################################################################################################################################################
write.csv(x = fuel_consumption_local,
          file.path(project_path, "data", "processed_data", "vms_annual_fuel_consumption.csv"),
          row.names = F)


## END OF SCRIPT #########################################################################################################################################################################















