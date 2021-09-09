library(raster)
library(sf)
library(rmapshaper)
library(rnaturalearth)
library(cowplot)
library(tidyverse)


effort <- readRDS(file.path(project_path, "data", "processed_data", "gridded_annual_eurnpa_fuel_consumption.rds"))

overfishing <- readRDS(file.path(project_path, "data", "output_data", "simulated_counterfactuals.rds")) %>% 
  select(year, alpha, eu_rnpa, pct)

seas <- st_read(file.path(data_path, "world-seas-v3", "World_Seas_IHO_v3"), "World_Seas_IHO_v3") %>% 
  filter(NAME %in% c("Gulf of California", "Gulf of Mexico", "North Pacific Ocean", "Caribbean Sea", "South Pacific Ocean")) %>% 
  st_make_valid()

# mex_seas <- seas %>% 
#   st_intersection(mex_eez)
# 
# mex_seas_viz <- ms_simplify(mex_seas)
# 
# high_seas <- seas %>% 
#   st_difference(mex_eez)



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


DF_withSubsidy <- stock_info %>%
  mutate(f=f_div_fmsy,b=b_div_bmsy,factor=additional_div_observed) %>%
  select(stock,g,K,f,b,response,factor) %>%
  mutate(phi=.188) %>%
  mutate(bss = ((1+phi)*(1-f*phi/(1+phi)))^(1/phi)) %>%
  mutate(Scenario = "With Subsidy")

DF_noSubsidy <- stock_info %>%
  mutate(f=f_div_fmsy,b=b_div_bmsy,factor=additional_div_observed) %>%
  mutate(f=f*(1-factor)) %>%
  select(stock,g,K,f,b,response,factor) %>%
  mutate(phi=.188) %>%
  mutate(bss = ((1+phi)*(1-f*phi/(1+phi)))^(1/phi)) %>%
  mutate(Scenario = "No Subsidy")

DF <- bind_rows(DF_withSubsidy,DF_noSubsidy) %>%
  filter(response == "Average") %>% 
  mutate(stock = str_replace_all(stock, "_", " "),
         stock = str_to_sentence(stock))

kobe <- ggplot(data = DF) +
  geom_point(aes(x = bss, y = f, fill = stock, shape = Scenario), size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_vline(xintercept = 1, linetype = "dashed") +
  xlab("B/Bmsy") +
  ylab("F/Fmsy") +
  xlim(0,1.3) +
  ylim(0.5,2.2) +
  theme_bw() +
  scale_fill_brewer(palette = "Set1") +
  scale_shape_manual(values = c(21, 24)) +
  theme(legend.justification = c(0, 0),
        legend.position = c(0, 0),
        legend.background = element_blank())

ggsave(kobe, filename = file.path(project_path, "results", "figures", "kobe_plot.png"), width = 5, height = 5)

















