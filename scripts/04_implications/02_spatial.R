################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  rnaturalearth,
  sf,
  tidyverse
)

# Load data --------------------------------------------------------------------
regions <- st_read(here("data", "raw", "mexico_fishing_regions.gpkg"))

mex <- rnaturalearth::ne_countries(country = "Mexico", returnclas = "sf")

continent <- rnaturalearth::ne_countries(continent = "North America", returnclass = "sf") %>% 
  sf::st_crop(sf::st_buffer(mex, dist = 1.5))

shrimp_tracks <- readRDS(here("data", "processed",  "2019_shrimp_tracks.rds"))

res <- 0.1

## PROCESSING ##################################################################
treated_in_2019 <- shrimp_panel %>% 
  filter(year == 2019,
         treated == 1) %>% 
  pull(eu)


# X ----------------------------------------------------------------------------
tracks_info <- shrimp_tracks %>% 
  mutate(lon = (floor(lon / res) * res) + (res / 2),
         lat = (floor(lat / res) * res) + (res / 2),
         treated = 1 * (eu_rnpa %in% treated_in_2019)) %>% 
  # expand_grid(pct = seq(0.1, 0.9, by = .2)) %>%
  mutate(factor = factor) %>% 
  mutate(additional = treated * (hours - (factor * hours))) %>% 
  group_by(lat, lon) %>% 
  summarize(hours = sum(hours, na.rm = T),
            additional = sum(additional, na.rm = T),
            n_eus = n_distinct(eu_rnpa)) %>% 
  filter(lat < 35) %>% 
  mutate(difference = additional / hours) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_join(regions) %>% 
  bind_cols(st_coordinates(.)) %>% 
  st_drop_geometry() %>% 
  replace_na(replace = list(region = 0)) %>% 
  rename(lon = X, lat = Y)

# Baseline plot
baseline_map <- ggplot() +
  geom_sf(data = continent, color = "black") +
  geom_sf(data = regions, color = "black", fill = "transparent") +
  geom_sf(data = mex, color = "black") +
  geom_tile(data = tracks_info, aes(x = lon, y = lat, fill = log(hours))) +
  scale_fill_viridis_c() +
  guides(fill = guide_colorbar(title = "log(Hours)",
                               frame.colour = "black",
                               ticks.colour = "black")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "",
       y = "")

baseline_col <- tracks_info %>% 
  group_by(region) %>% 
  summarize(hours = sum(hours)) %>% 
  ggplot(aes(x = region, y = hours)) +
  geom_col()

cowplot::plot_grid(baseline_map,
                   baseline_col,
                   ncol = 1,
                   align = "hv",
                   axis = "l",
                   rel_heights = c(1, 0.5))

# % Subsidy
ggplot() +
  geom_tile(data = tracks_info, aes(x = lon,
                                    y = lat,
                                    fill = difference)) +
  geom_sf(data = continent, color = "black") +
  geom_sf(data = mex, color = "black") +
  scale_fill_viridis_c(labels = scales::percent, option = "B") +
  guides(fill = guide_colorbar(title = "% Attributable to subsidy",
                               frame.colour = "black",
                               ticks.colour = "black")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "",
       y = "")

ggplot() +
  geom_tile(data = tracks_info, aes(x = lon, y = lat, fill = log(additional))) +
  geom_sf(data = continent, color = "black") +
  geom_sf(data = mex, color = "black") +
  scale_fill_viridis_c() +
  guides(fill = guide_colorbar(title = "log(Subsidized Hours)",
                               frame.colour = "black",
                               ticks.colour = "black")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "",
       y = "")

ggplot() +
  geom_tile(data = tracks_info %>% mutate(pct = paste0(pct * 100, "% reduction")), aes(x = lon, y = lat, fill = difference)) +
  geom_sf(data = continent, color = "black") +
  geom_sf(data = mex, color = "black") +
  scale_fill_viridis_c(labels = scales::percent, option = "B") +
  # scale_fill_gradient(low = "steelblue", high = "red", labels = scales::percent) +
  guides(fill = guide_colorbar(title = "% Reduction",
                               frame.colour = "black",
                               ticks.colour = "black",
                               title.position = "top",
                               direction = "horizontal")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal() +
  theme(legend.position = c(1, 0),
        legend.justification = c(1, 0)) +
  labs(x = "",
       y = "",
       title = "Expected reduction in fishing activity from five\nsubsidy reduction policies",
       subtitle = "Maps are produced from 2019 fishing activity on a 0.1° grid") +
  facet_wrap(~pct, ncol = 2)



## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------