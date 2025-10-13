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
  fixest,
  tidyverse
)

# Load data --------------------------------------------------------------------
shrimp_panel <- readRDS(here("data", "estimation_panels", "shrimp_estimation_panel.rds"))
shrimp_tracks <- readRDS(here("data", "processed",  "2019_shrimp_tracks.rds"))
semi_mod <- readRDS(here("data/output/semi_elasticity_twfe_model.rds"))

regions <- st_read(here("data", "raw", "mexico_fishing_regions.gpkg")) %>% 
  mutate(region = as.character(as.roman(region)))
mex <- rnaturalearth::ne_countries(country = "Mexico", returnclass = "sf")
continent <- rnaturalearth::ne_countries(continent = "North America", returnclass = "sf") %>% 
  sf::st_crop(sf::st_buffer(mex, dist = 1.5))

res <- 0.1

semi <- coef(semi_mod$`Fishing time`)[[1]]
change <- (exp(semi)-1)
factor <- 1 - change

## PROCESSING ##################################################################
treated_in_2019 <- shrimp_panel %>% 
  filter(year == 2019,
         treated == 1) %>% 
  pull(eu)


# X ----------------------------------------------------------------------------
tracks_info <- shrimp_tracks %>% 
  filter(year == 2019) %>% 
  mutate(lon = (floor(lon / res) * res) + (res / 2),
         lat = (floor(lat / res) * res) + (res / 2),
         treated = 1 * (eu_rnpa %in% treated_in_2019)) %>% 
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
  drop_na() %>% 
  rename(lon = X, lat = Y) %>% 
  mutate(rank = percent_rank(additional))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

theme_set(theme_minimal(base_size = 10) +
            theme(legend.position = "inside",
                  legend.position.inside = c(1, 1),
                  legend.justification.inside = c(1, 1),
                  legend.title.position = "top",
                  legend.direction = "horizontal", legend.background = element_rect(color = "black", fill = "white")))

# Baseline plot
total_hours <- ggplot() +
  geom_sf(data = continent,
          fill = "gray50",
          color = "black",
          linewidth = 0.1) +
  geom_sf(data = regions, color = "black", fill = "transparent") +
  geom_sf_text(data = regions, aes(label = region)) +
  geom_sf(data = mex,
          fill = "gray50",
          color = "black",
          linewidth = 0.1) +
  geom_tile(data = tracks_info, aes(x = lon, y = lat, fill = log(hours))) +
  scale_fill_viridis_c(option = "D") +
  guides(fill = guide_colorbar(title = "log(Hours)",
                               frame.colour = "black",
                               ticks.colour = "black")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "",
       y = "")

subsidized_hours <- ggplot() +
  geom_sf(data = continent,
          fill = "gray50",
          color = "black",
          linewidth = 0.1) +
  geom_sf(data = regions, color = "black", fill = "transparent") +
  geom_sf_text(data = regions, aes(label = region)) +
  geom_sf(data = mex,
          fill = "gray50",
          color = "black",
          linewidth = 0.1) +
  geom_tile(data = tracks_info, aes(x = lon, y = lat, fill = log(additional))) +
  scale_fill_viridis_c(option = "B") +
  guides(fill = guide_colorbar(title = "log(Hours)",
                               frame.colour = "black",
                               ticks.colour = "black")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "",
       y = "")

# % Subsidy
relative <- ggplot() +
  geom_sf(data = continent,
          fill = "gray50",
          color = "black",
          linewidth = 0.1) +
  geom_sf(data = regions, color = "black", fill = "transparent") +
  geom_sf_text(data = regions, aes(label = region)) +
  geom_sf(data = mex,
          fill = "gray50",
          color = "black",
          linewidth = 0.1) +
  geom_tile(data = tracks_info, aes(x = lon, y = lat, fill = difference)) +
  scale_fill_viridis_c(labels = scales::percent, option = "C") +
  guides(fill = guide_colorbar(title = "% Subsidized",
                             frame.colour = "black",
                             ticks.colour = "black")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "",
       y = "")

rank <- ggplot() +
  geom_sf(data = continent,
          fill = "gray50",
          color = "black",
          linewidth = 0.1) +
  geom_sf(data = regions, color = "black", fill = "transparent") +
  geom_sf_text(data = regions, aes(label = region)) +
  geom_sf(data = mex,
          fill = "gray50",
          color = "black",
          linewidth = 0.1) +
  geom_tile(data = tracks_info, aes(x = lon, y = lat, fill = rank)) +
  scale_fill_viridis_c(labels = scales::percent, option = "mako") +
  guides(fill = guide_colorbar(title = "% Rank",
                               frame.colour = "black",
                               ticks.colour = "black")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "",
       y = "")

# X ----------------------------------------------------------------------------
p <- cowplot::plot_grid(total_hours,
                        subsidized_hours,
                        relative,
                        rank, align = "hv", labels = c("a)", "b)", "c)", "d)"))

## EXPORT ######################################################################
out_dir <- here("content/figures/")

ggsave(plot = p,
       filename = here(out_dir, "fig_spatial_attribution.pdf"),
       width = 10,
       height = 6)

ggsave(plot = total_hours,
       filename = here(out_dir, "fig_total_hours_spatial_attribution.pdf"),
       width = 10,
       height = 6)

ggsave(plot = subsidized_hours,
       filename = here(out_dir, "fig_subsidized_hours_spatial_attribution.pdf"),
       width = 10,
       height = 6)

ggsave(plot = relative,
       filename = here(out_dir, "fig_relative_spatial_attribution.pdf"),
       width = 10,
       height = 6)

ggsave(plot = rank,
       filename = here(out_dir, "fig_rank_spatial_attribution.pdf"),
       width = 10,
       height = 6)

