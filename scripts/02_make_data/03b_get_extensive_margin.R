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
  dbscan,
  sf,
  furrr,
  tidyverse
)

# sf_use_s2(F)

# Load data --------------------------------------------------------------------
shrimp_tracks <- tibble(file = list.files(path = here("data"),
                                          pattern = "_shrimp_tracks.rds",
                                          full.names = T)) %>% 
  mutate(year = str_extract(file, pattern = "[:digit:]{4}"),
         data = map(file, readRDS)) %>% 
  select(-file)

## PROCESSING ##################################################################

# Define area calculations -----------------------------------------------------
area_concave_hull <- function(x) {
  # browser()
  # Find clusters
  clusters <- x %>%
    st_distance() %>%
    dbscan(eps = 50000, #km
           minPts = 5 # Minimum points per cluster
    )
  
  # Calculate convex hull and area of each cluster
  fishing_grounds <- x %>%
    mutate(cluster = clusters$cluster) %>%
    filter(cluster > 0) %>%
    group_by(cluster) %>%
    summarize(ground_hours = sum(hours, na.rm = T)) %>%
    st_convex_hull() %>%
    mutate(area = st_area(.),
           area = units::set_units(area, km^2))
  
  return(tibble(fg_area_km = sum(fishing_grounds$area, na.rm = T),
                fg_hours = sum(fishing_grounds$ground_hours, na.rm = T)))
}

# X ----------------------------------------------------------------------------
get_extensive <- function(data) {
  # Find vessels for which we don't have enough points'
  not_enough <- data %>%
    count(vessel_rnpa) %>%
    filter(n < 5) %>% 
    pull(vessel_rnpa)
  
  # Convert to points
  pts <- data %>% 
    filter(!vessel_rnpa %in% not_enough,
           between(speed, 1.5, 4)) %>%
    select(eu_rnpa, vessel_rnpa, lon, lat, hours) %>% 
    group_by(eu_rnpa, vessel_rnpa) %>% 
    nest() %>% 
    mutate(data = map(data, st_as_sf, coords = c("lon", "lat"), crs = 4326),
           data = map(data, ~st_transform(x = .x, crs = "EPSG:4485")))
  
  # browser()
  # Calculate area of fishing grounds
  cch <- pts %>%
    mutate(data = map(data, area_concave_hull)) %>%
    unnest(data)
  
  return(cch)
}

plan(multisession, workers = 9)
extensive <- shrimp_tracks %>% 
  mutate(data = future_map(data, get_extensive)) %>% 
  unnest(data) %>% 
  group_by(year, eu_rnpa) %>%
  summarize(fg_area_km = sum(fg_area_km),
            fg_hours = sum(fg_hours))
plan(sequential)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------

saveRDS(object = extensive,
        file = here("data", "extensive_margin.rds"))



