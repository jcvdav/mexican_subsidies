# cleaning diesel dataset

# Load packages
library(here)
library(janitor)
library(startR)
library(tidyverse)

source(here("scripts", "00_setup.R"))

# Read-in the Diesel subsidies data
all_fuel_raw <-
  read_csv(file.path(project_path, "raw_data", "B1_combustibles.csv"),
           col_types = cols(
             .default = col_character(),
             año = col_double(),
             monto_conapesca = col_double(),
             cve_ent = col_character(),
             cve_mun = col_character(),
             cve_loc = col_character(),
             cve_inegi = col_character(),
             no_emb_mayores = col_double(),
             no_emb_menores = col_double(),
             no_inst_acuicola = col_double(),
             rnpa = col_character()
           ))

# Start cleaning the data
all_fuel_clean <- all_fuel_raw %>%
  clean_names() %>%                                                            # Clean column names to snake case
  select(                                                                      # Select and translate column names
    year = ano,
    economic_unit = unidadeconmica,
    beneficiary = beneficiario,
    fuel_type = tipo_combustible,
    subsidy_amount = monto_conapesca,
    rnpa,
    fishing_type = actividadproductiva,
    target = pesqueria_estandar,
    n_large_scale_vessels = no_emb_mayores,
    n_small_scale_vessels = no_emb_menores,
    zone = zona,
    state = entidad,
    state_code = cve_ent,
    municipality = municipio,
    municipality_code = cve_mun,
    location = localidad,
    location_code = cve_loc
  ) %>% 
  drop_na(rnpa) %>% 
  mutate(                                                                     # Translate variable values
    fishing_type = case_when(
      fishing_type == "" ~ "Large scale",
      fishing_type == "PESCA RIBEREÑA" ~ "Small scale",
      T ~ NA_character_),
    target = case_when(target == "NO DISPONIBLE" ~ NA_character_,
                       target == "MULTIESPECIE" ~ "Multispecies",
                       target == "ESCAMA" | target == "ESCAMAS" ~ "Finfish",
                       target == "ALMEJAS" ~ "Clams",
                       target == "CAMARONES" ~ "Shrimp",
                       target == "CALAMARES" ~ "Squids",
                       target == "JAIBAS" ~ "Swimming crab",
                       target == "CANGREJO" ~ "Crabs",
                       target == "CARACOLES" ~ "Snails",
                       target == "Tiburones y Rayas" ~ "Sharks and rays"),
    fuel_type = case_when(fuel_type == "DIESEL MARINO" ~ "Diesel",
                          fuel_type == "GASOLINA RIBEREÑA" ~ "Gasoline")
  )

# Export a clean version
saveRDS(object = all_fuel_clean,
        file = here("data", "all_fuel_clean.rds"))


