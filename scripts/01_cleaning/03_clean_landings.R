# Load packages
library(startR)
library(janitor)
library(furrr)
library(tidyverse)

# Read in data
landings <- readRDS(file.path(project_path, "raw_data", "conapesca.rds")) %>% 
  clean_names() %>% 
  rename(vessel_name = nombre_activo,
         landing_port = sitio_desembarque,
         economic_unit = unidad_economica,
         state = estado,
         office = oficina,
         fishing_site = lugar_de_captura,
         month = mes,
         year = ano,
         main_group = nombre_principal,
         common_name = nombre_comun,
         scientific_name = nombre_cientifico,
         landed_weight_kg = peso_desembarcado,
         caught_weight_kg = peso_vivo,
         price = precio,
         value = valor) %>% # REMOVE ALL THE STUFF BELOW THIS
  group_by(economic_unit, main_group, year) %>% 
  summarize(landed_weight = sum(landed_weight_kg, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(type = stri_enc_mark(economic_unit)) %>% 
  filter(!type == "native") %>% 
  mutate(economic_unit = str_trim(economic_unit),
         economic_unit = toupper(economic_unit))

fx <- function(x){paste(x, "jc")}

normalize_economic_unit <- function(x){
  
}


plan(multiprocess, workers = 12)
tic()
vessel_names <- landings %>% 
  select(vessel_name) %>% 
  distinct() %>% 
  mutate(vessel_name = str_remove_all(vessel_name, "<e2><bf>")) %>% 
  mutate(vessel_name_norm = future_map_chr(vessel_name, normalize_shipname))

toc()













