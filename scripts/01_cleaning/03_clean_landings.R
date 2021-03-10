# Load packages
library(startR)
library(janitor)
library(furrr)
library(stringi)
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
         value = valor) %>% 
  filter(!str_detect(economic_unit, "^Scpa"),
         !str_detect(economic_unit, "^Scppa"),
         !str_detect(fishing_site, "cult"),
         !str_detect(fishing_site, "Acuacultura"),
         !str_detect(fishing_site, "Ac y tur"),
         !str_detect(fishing_site, "cuac")) %>% 
  mutate(type = stri_enc_mark(economic_unit)) %>% 
  filter(!type == "native") %>% 
  mutate(economic_unit = str_trim(economic_unit),
         economic_unit = toupper(economic_unit),
         economic_unit = normalize_economic_unit(economic_unit)) %>% 
  group_by(economic_unit, year) %>% 
  summarize(landed_weight = sum(landed_weight_kg, na.rm = T)) %>% 
  ungroup()

# Vessel name cleaning here
# plan(multiprocess, workers = 12)
# tic()
# vessel_names <- landings %>% 
#   select(vessel_name) %>% 
#   distinct() %>% 
#   mutate(vessel_name = str_remove_all(vessel_name, "<e2><bf>")) %>% 
#   mutate(vessel_name_norm = future_map_chr(vessel_name, normalize_shipname))
# 
# toc()



normalize_economic_unit <- function(economic_unit) {
  economic_unit <- economic_unit %>%
    stringr::str_to_upper() %>%
    stringr::str_replace_all(pattern = "Ñ", replacement = "N") %>%
    stringr::str_replace(pattern = "PE¿ASCO", replacement = "PENASCO") %>%
    stringr::str_replace(pattern = "COMPA¿¿A", replacement = "COMPANIA") %>%
    stringr::str_replace(pattern = "PENA", replacement = "") %>%
    stringr::str_replace(pattern = "CAJ¿N DOM¿NGUEZ", replacement = "CAJAN DOMINGUEZ") %>%
    stringr::str_replace(pattern = "RUISE¿OR", replacement = "RUISENOR") %>%
    stringr::str_replace(pattern = "MAR¿AS", replacement = "MARIAS") %>%
    stringr::str_replace(pattern = "ART¿CULO", replacement = "ARTICULO") %>%
    stringr::str_replace(pattern = "PROGRESE¿O", replacement = "PROGRESENO") %>%
    stringr::str_remove_all(pattern = "[^[:alnum:]|\\s]") %>%
    stringr::str_remove(pattern = " DE RL MI DE IP Y CV") %>%
    stringr::str_remove(pattern = "SC DE RL DE CV") %>%
    stringr::str_remove(pattern = "S DE RL DE C V") %>%
    stringr::str_remove(pattern = "S DE R L DE CV") %>%
    stringr::str_remove(pattern = "SC DE RL DE CV") %>%
    stringr::str_remove(pattern = "S DE RL DE CV") %>%
    stringr::str_remove(pattern = "SAPI DE CV") %>%
    stringr::str_remove(pattern = "S DE RL MI") %>%
    stringr::str_remove(pattern = "SCL DE CV") %>%
    stringr::str_remove(pattern = "SPR DE RI") %>%
    stringr::str_remove(pattern = "SRL DE CV") %>%
    stringr::str_remove(pattern = "SA DE C V") %>%
    stringr::str_remove(pattern = "S A DE C V") %>% 
    stringr::str_remove(pattern = "S C DE RL") %>%
    stringr::str_remove(pattern = "S  DE C V") %>%
    stringr::str_remove(pattern = "SC DE RL") %>%
    stringr::str_remove(pattern = "SA DE CV") %>%
    stringr::str_remove(pattern = "SC RL CV") %>%
    stringr::str_remove(pattern = "SC DE CV") %>%
    stringr::str_remove(pattern = "RL DE CV") %>%
    stringr::str_remove(pattern = "SCL Y CV") %>%
    stringr::str_remove(pattern = "S DE CV") %>%
    stringr::str_remove(pattern = "S  DE CV") %>%
    stringr::str_remove(pattern = "SADE CV") %>%
    stringr::str_remove(pattern = "RLDE CV") %>%
    stringr::str_remove(pattern = "SCRLCV") %>%
    stringr::str_remove(pattern = "SC RL$") %>%
    stringr::str_remove(pattern = "^SCPP") %>%
    stringr::str_remove(pattern = "DE RL") %>%
    stringr::str_remove(pattern = "SA CV") %>%
    stringr::str_remove(pattern = "SC L$") %>%
    stringr::str_remove(pattern = "SPR$") %>%
    stringr::str_remove(pattern = " AC$") %>% 
    stringr::str_remove(pattern = " SC$") %>% 
    stringr::str_remove(pattern = " C$") %>% 
    stringr::str_remove(pattern = "SRL") %>%
    stringr::str_remove(pattern = "SCL") %>%
    stringr::str_remove(pattern = "^SC") %>%
    stringr::str_remove(pattern = "SSS") %>% 
    stringr::str_remove(pattern = "S DE S S") %>% 
    stringr::str_remove(pattern = " [:digit:]$") %>% 
    stringr::str_trim() %>% 
    stringr::str_squish()
  
  economic_unit[economic_unit == ""] <- NA
  
  return(economic_unit)
  
}


dictionary <- landings %>% 
  count(economic_unit) %>%
  ungroup() %>% 
  mutate(economic_unit_normalized = normalize_economic_unit(economic_unit))








