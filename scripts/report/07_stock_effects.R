library(raster)
library(sf)
library(rmapshaper)
library(rnaturalearth)
library(cowplot)
library(tidyverse)


effort <- readRDS(file.path(project_path, "data", "processed_data", "gridded_annual_eurnpa_fuel_consumption.rds"))

overfishing <- readRDS(file.path(project_path, "data", "output_data", "simulated_counterfactuals.rds")) %>% 
  select(year, alpha, eu_rnpa, pct)

america <- rnaturalearth::ne_countries(continent = c("North America", "South America"), returnclass = "sf") %>% 
  st_crop(xmin = -150, ymin = -25, xmax = -70, ymax = 40)

coast <- rnaturalearth::ne_countries(country = "Mexico", returnclass = "sf", scale = "large") %>% 
  st_make_valid()

mex_eez <- st_read(file.path(data_path, "marine-regions-eez-v11/World_EEZ_v11_20191118_gpkg/eez_v11.gpkg")) %>% 
  filter(ISO_TER1 == "MEX")

seas <- st_read(file.path(data_path, "world-seas-v3", "World_Seas_IHO_v3"), "World_Seas_IHO_v3") %>% 
  filter(NAME %in% c("Gulf of California", "Gulf of Mexico", "North Pacific Ocean", "Caribbean Sea", "South Pacific Ocean")) %>% 
  st_make_valid()

mex_seas <- seas %>% 
  st_intersection(mex_eez)

mex_seas_viz <- ms_simplify(mex_seas)

high_seas <- seas %>% 
  st_difference(mex_eez)



effort_counterfactual <- effort %>% 
  left_join(overfishing, by = c("year", "eu_rnpa")) %>% 
  mutate(species = case_when(species == "shrimp plus" ~ "shrimp",
                             species == "any" ~ "others",
                             is.na(species) ~ "others",
                             T ~ species)) %>% 
  replace_na(replace = list(pc = 0, alpha = 1)) %>% 
  filter(lat_bin_center < 90, lon_bin_center > -360) %>% 
  mutate(additional_fuel_consumption_l = fuel_consumption_l * pct) %>% 
  group_by(alpha, species, lat_bin_center, lon_bin_center) %>% 
  summarize(additional_fuel_consumption_l = sum(additional_fuel_consumption_l, na.rm = T),
            fuel_consumption_l = sum(fuel_consumption_l, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(pct = (additional_fuel_consumption_l / fuel_consumption_l) * 100,
         response = ifelse(alpha == 1, "Marginal", "Average")) 


effort_rasters <- effort_counterfactual %>% 
  select(x = lon_bin_center, y = lat_bin_center, base_fuel_consumption_l = fuel_consumption_l, additional_fuel_consumption_l, response, species) %>% 
  pivot_wider(names_from = c(response, species), values_from = c(base_fuel_consumption_l, additional_fuel_consumption_l), values_fill = 0) %>% 
  rasterFromXYZ(res = 0.1)


seas_zonal_stats <- seas %>% 
  select(name = NAME) %>% 
  cbind(raster::extract(effort_rasters, ., fun = "sum", data.frame = T, na.rm = T)) %>% 
  st_drop_geometry() %>%
  pivot_longer(cols = contains("fuel"),
               names_to = "layer",
               values_to = "fuel") %>% 
  mutate(layer = str_remove_all(layer, "_fuel_consumption_l")) %>% 
  separate(layer, into = c("consumption", "response", "species"), sep = "_") %>% 
  pivot_wider(names_from = consumption, values_from = fuel) %>% 
  mutate(pct = additional / base)

out <- seas_zonal_stats %>% 
  filter(!base == 0) %>% 
  mutate(name = str_replace_all(name, " ", "_"),
         name = str_to_lower(name),
         stock = paste(species, name, sep = "_"),
         counterfactual_liters = base - additional) %>% 
  select(response, stock, observed_liters = base, counterfactual_liters, additional_liters = additional, additional_div_observed = pct) %>% 
  filter(stock %in% c("sardine_gulf_of_california", "shrimp_gulf_of_california", "tuna_north_pacific_ocean"))


stock_info <- read.csv(file.path(project_path, "data", "raw_data", "mangin_stock_status.csv")) %>% 
  mutate(stock = paste(species, location, sep = "_")) %>% 
  select(stock, everything(), -c(Scientific.name, location, species)) %>% 
  left_join(out, by = "stock")

write.csv(stock_info, file = file.path(project_path, "data", "output_data", "effort_by_stock.csv"), row.names = FALSE)


location	species	Scientific name	g	K	b_div_bmsy	k_div_kmsy
gulf_of_california	sardine	Sardinops sagax	0.17	6980198.73	0.9	1.63
gulf_of_california	shrimp	Litopenaeus stylirostris	0.2	75531.55	0.7	2.09
north_pacific_ocean	tuna	Thunnus albacares	0.03	9155828.24	0.85	1.93


















