
library(raster)
library(sf)
library(rmapshaper)
library(rnaturalearth)
library(cowplot)
library(tidyverse)


effort <- readRDS(file.path(project_path, "data", "processed_data", "gridded_annual_eurnpa_fuel_consumption.rds")) %>% 
  replace_na(replace = list(species = "any")) %>% 
  filter(year == 2019) %>% 
  distinct()

overfishing <- readRDS(file.path(project_path, "data", "output_data", "simulated_counterfactuals.rds")) %>% 
  filter(year == 2019) %>% 
  group_by(alpha, eu_rnpa) %>% 
  summarize(overfishing = sum(overfishing, na.rm = T),
            fuel_consumption_l = sum(fuel_consumption_l, na.rm = T),
            pct = overfishing / fuel_consumption_l) %>% 
  distinct() %>% 
  select(-fuel_consumption_l)

america <- rnaturalearth::ne_countries(continent = c("North America", "South America"), returnclass = "sf") %>% 
  st_crop(xmin = -150, ymin = -25, xmax = -70, ymax = 40)

# coast <- rnaturalearth::ne_countries(country = "Mexico", returnclass = "sf", scale = "large") %>% 
#   st_make_valid()

mex_eez <- st_read(file.path(data_path, "marine-regions-eez-v11/World_EEZ_v11_20191118_gpkg/eez_v11.gpkg")) %>% 
  filter(ISO_TER1 == "MEX")

seas <- st_read(file.path(data_path, "world-seas-v3", "World_Seas_IHO_v3"), "World_Seas_IHO_v3") %>% 
  filter(NAME %in% c("Gulf of California", "Gulf of Mexico", "North Pacific Ocean", "Caribbean Sea")) %>% 
  st_make_valid()

mex_seas <- seas %>% 
  st_intersection(mex_eez)

mex_seas_viz <- ms_simplify(mex_seas)


effort_counterfactual <- inner_join(effort, overfishing, by = "eu_rnpa") %>% 
  replace_na(replace = list(pct = 0, alpha = 1)) %>%
  filter(fuel_consumption_l > 0) %>% 
  filter(lat_bin_center < 90, lon_bin_center > -360) %>% 
  mutate(additional_fuel_consumption_l = fuel_consumption_l * pct) %>% 
  group_by(alpha, lat_bin_center, lon_bin_center) %>% 
  summarize(additional_fuel_consumption_l = sum(additional_fuel_consumption_l, na.rm = T),
            fuel_consumption_l = sum(fuel_consumption_l, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(pct = (additional_fuel_consumption_l / fuel_consumption_l) * 100,
         response = ifelse(alpha == 1, "Marginal", "Average")) 


map <- ggplot(effort_counterfactual) +
  geom_sf(data = america, fill = "transparent", size = 0.1) +
  geom_tile(aes(x = lon_bin_center, y = lat_bin_center, fill = pct)) +
  geom_sf(data = mex_seas_viz, fill = "transparent", color = "black") +
  scale_fill_viridis_c(trans = "log10") +
  facet_grid(~response) +
  theme_void(base_size = 14) +
  scale_x_continuous(expand = c(0, 0), limits = c(-150, -70)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-20, 40)) +
  theme(legend.position = "top",
        strip.text.x=element_text(margin=margin(b = 2)),
        strip.background = element_blank(),
        panel.spacing = unit(10, "pt")) +
  guides(fill = guide_colorbar(title = "% Effort caused by\nsubsidy (log-10 scale)",
                               frame.colour = "black",
                               ticks.colour = "black",
                               barwidth = 10
                               ))  +
  labs(x = "", y = "")

# Zonal stats

effort_rasters <- effort_counterfactual %>% 
  select(x = lon_bin_center, y = lat_bin_center, base_fuel_consumption_l = fuel_consumption_l, additional_fuel_consumption_l, response) %>% 
  pivot_wider(names_from = response, values_from = c(base_fuel_consumption_l, additional_fuel_consumption_l), values_fill = 0) %>% 
  rasterFromXYZ(res = 0.1)

seas_zonal_stats <- mex_seas %>% 
  select(name = NAME) %>% 
  cbind(raster::extract(effort_rasters, ., fun = "sum", data.frame = T, na.rm = T)) %>% 
  st_drop_geometry() %>%
  pivot_longer(cols = contains("fuel"),
               names_to = "layer",
               values_to = "fuel") %>% 
  mutate(layer = str_remove_all(layer, "_fuel_consumption_l")) %>% 
  separate(layer, into = c("consumption", "response"), sep = "_") %>% 
  pivot_wider(names_from = consumption, values_from = fuel) %>% 
  mutate(pct = additional / base)

abs_bar <- ggplot(seas_zonal_stats, aes(x = name, y = additional / 1e6, fill = response, alpha = pct)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  scale_alpha_continuous(labels = scales::percent) +
  scale_y_continuous() +
  labs(x = "", y = "Additional fuel consumption (million L)") +
  guides(fill = guide_legend("Response"),
         alpha = guide_legend("% Change")) +
  coord_flip()

spatial <- plot_grid(map, abs_bar, ncol = 1, rel_heights = c(2, 1))

ggsave(spatial,
       filename = file.path(project_path, "results", "figures", "spatial_subsidy_footprint.png"),
       width = 8,
       height = 6)


