
library(here)
library(startR)
library(janitor)
library(readxl)
library(tidyverse)



# Other tables we'll need
mdl_raw <- read_csv(file.path(project_path, "raw_data", "maximum_daily_liters.csv")) %>%         # Maximum dayl liters for each engine size
  filter(fuel_type == "diesel")
engine_power_bins <- unique(mdl_raw$engine_power_hp)

# Define functions we'll use
str_fix <- function(x){
  x <- str_to_upper(x)
  x <- str_trim(x)
  x <- str_squish(x)
  return(x)
}

engine_match <- function(engine_power, bins){
  bins <- c(0, bins)
  bins <- bins[bins <= engine_power]
  match <- max(bins)
  
  return(match)
}

design_speed <- function(engine_power_hp){
  engine_power_kwh <- engine_power_hp / 1.34102
  10.4818 + (1.2e-3 * engine_power_kwh) - (3.84e-8 * engine_power_kwh ^ 2)
}


# Creating a vessel_info table
vessel_engines_ls_raw <- read_excel(path = file.path(project_path,                     # Vessel registry
                                                  "raw_data",
                                                  "ANEXO-DGPPE-147220",
                                                  "ANEXO SISI 147220 - EmbarcacionesMayores.xlsx"),
                                 sheet = 2, 
                                 col_types = c("skip",
                                               "text",
                                               "text",
                                               "text",
                                               "text",
                                               "numeric",
                                               "text"))

# vessel_engines_ss_raw <- read_excel(path = file.path(project_path,                     # Vessel registry
#                                                      "raw_data",
#                                                      "ANEXO-DGPPE-147220",
#                                                      "ANEXO SISI 147220 - EmbarcacionesMenores.xlsx"),
#                                     sheet = 1) %>% #, 
#                                     # col_types = c("skip", "text", "text", "text", "text", "numeric", "text")) %>% 
#   janitor::clean_names()





# vessel_gears_ls_raw <- read_excel(path = file.path(project_path,                     # Vessel registry
#                                                 "raw_data",
#                                                 "ANEXO-DGPPE-147220",
#                                                 "ANEXO SISI 147220 - EmbarcacionesMayores.xlsx"),
#                                sheet = 3,
#                                col_types = c("skip",
#                                              "text",
#                                              "text",
#                                              "text",
#                                              "numeric",
#                                              "numeric",
#                                              "numeric",
#                                              "numeric",
#                                              "text",
#                                              "numeric"))







assets_raw <- read_excel(path = file.path(project_path,                    # Vessel registry
                                           "raw_data",
                                           "ANEXO-DGPPE-147220",
                                           "ANEXO SISI 147220 - EmbarcacionesMayores.xlsx"),
                          sheet = 1,
                          col_types = "text")




## Selections
vessel_engines <- vessel_engines_ls_raw %>% 
  clean_names() %>% 
  filter(principal == "SI") %>%
  rename(vessel_rnpa = rnpa_emb_mayor,
         brand = marca,
         model = modelo,
         serial_number = serie,
         engine_power_hp = potencia,
         main = principal) %>% 
  mutate_at(vars(brand, model), str_fix) %>% 
  group_by(vessel_rnpa) %>% 
  mutate(engine_power_hp = sum(engine_power_hp, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(engine_power_bin_hp = map_dbl(engine_power_hp, engine_match, engine_power_bins),
         design_speed_kt = design_speed(engine_power_hp)) %>% 
  select(vessel_rnpa, engine_power_hp, engine_power_bin_hp, design_speed_kt, -c(serial_number, main, brand, model)) %>% 
  distinct()

# vessel_gears <- vessel_gears_ls_raw %>% 
#   clean_names() %>% 
#   mutate(species = case_when(especie_principal == "ATUN" ~ "tuna",
#                              especie_principal == "CAMARON" ~ "shrimp",
#                              especie_principal == "SARDINA" ~ "sardine",
#                              is.na(especie_principal) ~ "any",
#                              T ~ "any")) %>% 
#   rename(vessel_rnpa = rnpa_emb_mayor,
#          gear_type = arte_o_equipo,
#          target_species = especie_principal,
#          gear_amount = cantidad,
#          gear_length_m = long_mts,
#          gear_height_m = altura,
#          mesh_size_mm = malla_mms,
#          construction_material = material_construccion,
#          number_of_hooks = reynales) %>% 
#   group_by(vessel_rnpa) %>% 
#   mutate(n = n()) %>% 
#   ungroup() %>% 
#   mutate(species = ifelse(n > 1 & species == "shrimp", paste(species, "plus"), species)) %>% 
#   filter(n == 1 | (n > 1 & !species == "any")) %>% 
#   group_by(vessel_rnpa) %>% 
#   mutate(n2 = n()) %>% 
#   ungroup() %>% 
#   filter(!(vessel_rnpa == "00039933" & species == "sardine"),
#          !(vessel_rnpa == "00064410" & species == "tuna"),
#          !(vessel_rnpa == "00037549" & species == "tuna")) %>% 
#   select(-c(n, n2)) %>% 
#   rename(rnpa_species = species)

plan("multisession")
# Clean assets
assets <- assets_raw %>% 
  clean_names() %>% 
  rename(eu_rnpa = rnpa_9,
         economic_unit = unidad_economica,
         vessel_rnpa = rnpa_11,
         vessel_name = activo,
         vessel_type = uso,
         owner_rnpa = rnpa_14,
         owner_name = propietario,
         hull_identifier = matricula,
         home_port = puerto,
         captain_num = patron,
         engineer_num = motorista,
         s_fisher_num = pescador_esp,
         fisher_nm = pescador,
         construction_year = ano_construccion,
         hull_material = material_casco,
         preservation_system = sistema_conservacion,
         gear_type = arte_pesca,
         target_species = pesqueria,
         vessel_length_m = eslora,
         vessel_beam_m = manga,
         vessel_height_m = puntal,
         vessel_draft_m = calado,
         vessel_gross_tonnage = cap_carga,
         detection_gear = equipo_deteccion) %>%                                                    ### REVIEW SPECIES ASSIGNMENT (TO BETTER-MATCH DPC)
  mutate(target_species = case_when(target_species == "ATÚN" ~ "tuna",
                                    target_species == "CAMARÓN" ~ "shrimp",
                                    target_species == "SARDINA" ~ "sardine",
                                    T ~ "any")) %>% 
  filter(estatus == "ACTIVO") %>% 
  select(eu_rnpa, economic_unit, vessel_rnpa, vessel_name, owner_rnpa, owner_name, hull_identifier,
         home_port,
         construction_year,
         hull_material,
         preservation_system,
         gear_type,
         detection_gear,
         contains("vessel_"),
         contains("_num")) %>% 
  distinct() %>% 
  mutate(vessel_name = furrr::future_map_chr(vessel_name, normalize_shipname)) %>% 
  drop_na(eu_rnpa, vessel_rnpa)

plan("sequential")

# COMBINE TABLES

vessel_registry <- assets %>% 
  left_join(vessel_engines, by = "vessel_rnpa") %>% 
  filter(!is.na(engine_power_hp))


write.csv(vessel_registry, here("data", "vessel_registry.csv"))














