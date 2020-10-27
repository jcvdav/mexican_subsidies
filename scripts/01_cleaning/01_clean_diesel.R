# cleaning diesel dataset

# Load packages
library(janitor)
library(startR)
library(tidyverse)

# Read-in the Diesel subsidies data
diesel_raw <-
  read.csv(file.path(project_path, "raw_data", "Diesel UE (2011-2016).csv"),
           stringsAsFactors = F)

# Start cleaning the data
diesel_clean <- diesel_raw %>%
  clean_names() %>%                                                          # Clean column names to snake case
  filter(tipo_actividad == "CAPTURA") %>%                                    # Keep only boats who are catching
  select(                                                                    # Select and translate column names
    main_id,
    year = ano,
    economic_unit = nombre_ue,
    rnpa,
    legal_rep = representante_legal,
    fishing_type = tipo_pesca,
    diesel_liters = monto_diesel,
    vessels = total_emb,
    large_scale_vessels = emb_may,
    small_scale_cessels = emb_men,
    state = entidad,
    state_code = cve_ent,
    municipality = municipio,
    municipality_code = cve_mun,
    location = localidad,
    location_code = cve_loc,
    poverty_level = gm_2010,
    lat = lat_dec,
    lon = lon_dec
  ) %>% 
  mutate(                                                                     # Translate variable values
    poverty_level = case_when(
      poverty_level == "ALTO" ~ "High",
      poverty_level == "MEDIO" ~ "Medium",
      poverty_level == "BAJO" ~ "Low",
      poverty_level == "MUY BAJO" ~ "Very low",
      T ~ NA_character_),
    fishing_type = case_when(
      fishing_type == "ALTURA" ~ "Large scale",
      fishing_type == "RIBERENA" ~ "Small_scale",
      fishing_type == "AMBAS" ~ "Both",
      T ~ NA_character_)
  )

# Export a clean version
saveRDS(object = diesel_clean,
        file = file.path(project_path, "processed_data", "diesel_clean.rds"))


