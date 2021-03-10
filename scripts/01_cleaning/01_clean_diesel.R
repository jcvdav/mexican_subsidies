# cleaning diesel dataset

# Load packages
library(janitor)
library(startR)
library(tidyverse)

# Read-in the Diesel subsidies data
diesel_raw <-
  read.csv(file.path(project_path, "raw_data", "B1_combustibles.csv"),
           stringsAsFactors = F)

# Start cleaning the data
diesel_clean <- diesel_raw %>%
  clean_names() %>%                                                          # Clean column names to snake case
  select(                                                                    # Select and translate column names
    year = ano,
    economic_unit = unidadeconmica,
    rnpa,
    fishing_type = actividadproductiva,
    target = pesqueria_estandar,
    subsidy_amount = monto_conapesca,
    large_scale_vessels = no_emb_mayores,
    small_scale_vessels = no_emb_menores,
    zone = zona,
    state = entidad,
    state_code = cve_ent,
    municipality = municipio,
    municipality_code = cve_mun,
    location = localidad,
    location_code = cve_loc
  ) %>% 
  mutate(                                                                     # Translate variable values
    fishing_type = case_when(
      fishing_type == "" ~ "Large scale",
      fishing_type == "PESCA RIBEREÑA" ~ "Small_scale",
      T ~ NA_character_)
    ) %>% 
  group_by(year, rnpa, economic_unit, fishing_type, target, large_scale_vessels, small_scale_vessels, zone, state, municipality, location) %>% 
  summarize(subsidy_amount = sum(subsidy_amount, na.rm = T)) %>% 
  ungroup()

# Export a clean version
saveRDS(object = diesel_clean,
        file = file.path(project_path, "processed_data", "diesel_clean.rds"))


