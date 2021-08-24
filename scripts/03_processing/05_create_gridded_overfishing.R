
library(sf)
library(rmapshaper)
library(rnaturalearth)
library(tidyverse)


effort <- readRDS(file.path(project_path, "data", "processed_data", "gridded_annual_eurnpa_fuel_consumption.rds")) 

overfishing <- readRDS(file.path(project_path, "data", "output_data", "simulated_counterfactuals.rds")) %>% 
  select(year, alpha, eu_rnpa, pct)

america <- rnaturalearth::ne_countries(continent = c("North America", "South America"), returnclass = "sf") %>% 
  st_crop(xmin = -150, ymin = -25, xmax = -70, ymax = 40)

coast <- rnaturalearth::ne_countries(country = "Mexico", returnclass = "sf")

mex_eez <- st_read(file.path(data_path, "marine-regions-eez-v11/World_EEZ_v11_20191118_gpkg/eez_v11.gpkg")) %>% 
  filter(ISO_TER1 == "MEX") %>% 
  ms_simplify()

mex_seas <- st_read(file.path(data_path, "world-seas-v3", "World_Seas_IHO_v3"), "World_Seas_IHO_v3") %>% 
  filter(NAME %in% c("Gulf of California", "Gulf of Mexico", "North Pacific Ocean", "Caribbean Sea")) %>% 
  ms_simplify() %>% 
  st_intersection(mex_eez)

effort_counterfactual <- left_join(effort, overfishing, by = c("year", "eu_rnpa")) %>% 
  drop_na(alpha) %>% 
  replace_na(replace = list(pc = 0)) %>%
  filter(lat_bin_center < 90, lon_bin_center > -360) %>% 
  mutate(additional_fuel_consumption_l = fuel_consumption_l * pct) %>% 
  group_by(year, alpha, lat_bin_center, lon_bin_center) %>% 
  summarize(additional_fuel_consumption_l = sum(additional_fuel_consumption_l, na.rm = T),
            fuel_consumption_l = sum(fuel_consumption_l)) %>% 
  ungroup() %>% 
  mutate(pct = (additional_fuel_consumption_l / fuel_consumption_l) * 100,
         response = ifelse(alpha == 1, "Marginal", "Average")) %>% 
  filter(year == 2019)


ggplot(effort_counterfactual) +
  geom_sf(data = america, fill = "transparent") +
  geom_sf(data = coast, color = "black") +
  geom_tile(aes(x = lon_bin_center, y = lat_bin_center, fill = pct)) +
  geom_sf(data = mex_seas, fill = "transparent", color = "black") +
  scale_fill_viridis_c(trans = "log10") +
  facet_grid(~response) +
  lims(x = c(-150, -70), y = c(-20, 40)) +
  theme_void(base_size = 14) +
  theme(legend.position = "bottom") +
  theme(strip.text.x=element_text(margin=margin(b = 1))) +
  guides(fill = guide_colorbar(title = "% Effort caused by subsidy\n(log-10 scale)",
                               direction = "horizontal",
                               frame.colour = "black",
                               ticks.colour = "black",
                               barwidth = 10,
                               title.position = "top"))  +
  labs(x = "", y = "")





