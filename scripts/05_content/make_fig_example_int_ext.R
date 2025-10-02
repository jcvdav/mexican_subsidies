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
  cowplot,
  ggspatial,
  terra,
  sf,
  rnaturalearth,
  tidyverse
)

# Load data --------------------------------------------------------------------
shrimp_panel <- readRDS(here("data", "estimation_panels", "shrimp_estimation_panel.rds"))

theme_set(theme_minimal(base_size = 10))

## PROCESSING ##################################################################

## Define spatial resolution
res <- 0.1

# X ----------------------------------------------------------------------------
hour_changes <- shrimp_panel %>% 
  filter(n_vessels == 1,
         sometimes == 1,
         year <= 2019) %>% 
  group_by(eu, treated) %>% 
  summarize(max = max(hours, na.rm = T),
            min = min(hours, na.rm = T),
            .groups = "drop") %>% 
  mutate(hours = ifelse(treated == 0, min, max),
         treated = ifelse(treated == 0, "not", "sub")) %>% 
  select(eu, treated, hours) %>% 
  pivot_wider(names_from = treated,
              values_from = hours) %>% 
  drop_na() %>% 
  mutate(difference = sub - not) %>% 
  arrange(desc(difference))

hour_highest <- head(hour_changes, 2) |> 
  tail(1)

shrimp_panel %>% 
  filter(eu %in% hour_highest$eu,
         hours %in% c(hour_highest$not, hour_highest$sub)) |> 
  select(year, eu, treated) |> 
  arrange(eu, treated)

hours_tracks_least <- readRDS(here("data", "processed", "2011_shrimp_tracks.rds")) %>% 
  filter(eu_rnpa == hour_highest$eu)

hours_tracks_most <- readRDS(here("data", "processed", "2017_shrimp_tracks.rds")) %>% 
  filter(eu_rnpa == hour_highest$eu)

hour_tracks <- bind_rows(hours_tracks_least,
                         hours_tracks_most) %>% 
  mutate(year = ifelse(year == 2011, "Not subsidized", "Subsidized")) %>% 
  mutate(lon = (floor(lon / res) * res) + (res / 2),
         lat = (floor(lat / res) * res) + (res / 2)) %>% 
  group_by(year, lat, lon) %>% 
  summarize(hours = sum(hours, na.rm = T)) %>% 
  filter(hours >= 1)

hour_raster <- rasterize(x = hour_tracks %>% 
                           vect(geom = c("lon", "lat"),
                                crs = "EPSG:4326"),
                         y = rast(xmin = -99, xmax = -90,
                                  ymin = 18, ymax = 26.5,
                                  res = res, crs = "EPSG:4326"),
                         fun = "mean",
                         field = "hours",
                         by = "year")

areas <- (!is.na(hour_raster)) * cellSize(hour_raster, unit = "km")

area_unsub <- values(areas[[1]]) %>% 
  sum()
area_sub <- values(areas[[2]]) %>% 
  sum()

hr_stats <- hour_tracks %>% 
  group_by(year) %>% 
  summarize(hours = sum(hours))

stats <- hr_stats |> 
  mutate(area = c(area_unsub, area_sub)) |> 
  mutate(hours = paste(round(hours / 24), "days"),
         area = paste0("'",format(round(area), big.mark = ","),"'", "~km^{2}"))

mex <- ne_countries(country = c("Mexico", "United States of America"),
                    returnclass = "sf",
                    scale = "large") |> 
  st_crop(hour_raster)

## VISUALIZE ###################################################################
hour_map <- ggplot(data = hour_tracks) + 
  geom_tile(aes(x = lon, y = lat, fill = log(hours))) +
  geom_sf(data = mex,
          fill = "gray50",
          color = "black",
          linewidth = 0.5) +
  geom_text(data = stats, aes(x = -94, y = 26, label = hours)) +
  geom_text(data = stats, aes(x = -94, y = 25.5, label = area), parse = T) +
  facet_wrap(~year) +
  scale_fill_gradient(low = "#FEFEE4", high = "#003C30") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "inside",
        legend.position.inside = c(1, 1),
        legend.justification.inside = c(1, 1),
        axis.title = element_blank()) +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black")) +
  labs(fill = "Time fishing\n(log-hours)") +
  annotation_scale(location = 'bl')

hour_map

hour_diff_map <- hour_tracks %>% 
  pivot_wider(names_from = "year",
              values_from = "hours") %>% 
  replace_na(replace = list(`Not subsidized` = 0,
                            Subsidized = 0)) %>% 
  mutate(difference = Subsidized - `Not subsidized`,
         year = "Difference") %>% 
  ggplot() +
  geom_tile(aes(x = lon, y = lat, fill = difference)) +
  geom_sf(data = mex,
          fill = "gray50",
          color = "black",
          linewidth = 0.5) +
  facet_wrap(~year) +
  scale_fill_gradient2(midpoint = 0) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "inside",
        legend.position.inside = c(1, 1),
        legend.justification.inside = c(1, 1),
        axis.title = element_blank(),
        axis.text.y = element_blank()) +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black")) +
  labs(fill = "Difference \n(hours)") +
  annotation_scale(location = 'bl')


hours <- plot_grid(hour_map, hour_diff_map,
                   rel_widths = c(2.05, 1))

hours

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
output_dir <- "content/figures"

ggsave(plot = hours,
       filename = here(output_dir, "fig_example_int_ext_diff.pdf"),
       width = 11,
       height = 4,
       units = "in")

ggsave(plot = hour_map,
       filename = here(output_dir, "fig_example_int_ext.pdf"),
       width = 8,
       height = 4,
       units = "in")


