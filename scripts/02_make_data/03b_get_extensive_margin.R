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

sf_use_s2(F)

# Load data --------------------------------------------------------------------
shrimp_tracks <- tibble(file = list.files(path = here("data", "processed"),
                                          pattern = "_shrimp_tracks.rds",
                                          full.names = T)) %>% 
  mutate(year = str_extract(file, pattern = "[:digit:]{4}"),
         data = map(file, readRDS)) %>% 
  select(-file)

## PROCESSING ##################################################################

get_extensive <- function(data) {
  # browser()
  
  npts <- dim(data)[1]
  
  results <- data %>% 
    select(year, eu_rnpa, vessel_rnpa) %>% 
    distinct() %>% 
    mutate(fg_area_km = 0,
           fg_hours = 0,
           fg_n = 0,
           n_pts = npts)
  
  # Only proceed if there are more than 5 observations
  if(npts >= 6) {
    
    # Build spatial object
    spat <- data %>%
      st_as_sf(coords = c("lon", "lat"),
               crs = 4326) %>%
      st_transform(crs = "+proj=lcc +lat_0=12 +lon_0=-102 +lat_1=17.5 +lat_2=29.5 +x_0=2500000 +y_0=0")  # https://epsg.io/6361
    
    
    # Find clusters
    clusters <- spat %>%
      st_distance() %>%
      dbscan(eps = 50000, # Distance in meters
             minPts = 6 # Minimum points per cluster
      )
    
    n_clust <- max(clusters$cluster)
    
    # Only proceed if there is at least one cluster (cluster # 0 is "noise" so it doesn't count)
    if(n_clust > 0) {
      # Calculate convex hull and area of each cluster
      results <- spat %>%
        mutate(cluster = clusters$cluster) %>%
        filter(!cluster == 0) %>% # Remove points not part of a cluster
        group_by(year, eu_rnpa, vessel_rnpa, cluster) %>%
        summarize(ground_hours = sum(hours, na.rm = T)) %>%
        st_convex_hull() %>% 
        mutate(area = st_area(.),
               area = units::set_units(area, km^2)) %>% 
        st_drop_geometry() %>% 
        group_by(year, eu_rnpa, vessel_rnpa) %>% 
        summarize(fg_area_km = sum(area, na.rm = T),
                  fg_hours = sum(ground_hours, na.rm = T),
                  .groups = "drop") %>% 
        mutate(fg_area_km = as.numeric(fg_area_km),
               fg_n = n_clust-1,
               n_pts = npts)
    }
  }
  
  # Return
  return(results)
}

plan(multisession, workers = 14)
tic()
vessel_info <- shrimp_tracks %>% 
  pull(data) %>% 
  bind_rows() %>%
  select(year, eu_rnpa, vessel_rnpa, lon, lat, hours) %>% 
  group_by(year, eu_rnpa, vessel_rnpa) %>%
  group_split() %>% 
  future_map_dfr(.f = get_extensive)
toc()
beepr::beep(2)
plan(sequential)

extensive <- vessel_info %>% 
  select(-vessel_rnpa) %>% 
  group_by(year, eu_rnpa) %>%
  summarize_all(sum)

# plan(multisession, workers = 9)
# extensive <- shrimp_tracks %>% 
#   head(1) %>% 
#   mutate(data = future_map(data, get_extensive)) %>% 
#   unnest(data) %>% 
#   group_by(year, eu_rnpa) %>%
#   summarize(fg_area_km = sum(fg_area_km),
#             fg_hours = sum(fg_hours))
# beppr::beep(2)
# plan(sequential)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------

saveRDS(object = extensive,
        file = here("data", "processed", "extensive_margin.rds"))



# Define area calculations -----------------------------------------------------
# area_concave_hull <- function(x) {
#   # Find clusters
#   clusters <- x %>%
#     st_distance() %>%
#     dbscan(eps = 50000, # Distance in meters
#            minPts = 6 # Minimum points per cluster
#     )
#   
#   # Calculate convex hull and area of each cluster
#   fishing_grounds <- x %>%
#     mutate(cluster = clusters$cluster) %>%
#     filter(cluster > 0) %>%
#     group_by(cluster) %>%
#     summarize(ground_hours = sum(hours, na.rm = T)) %>%
#     st_convex_hull() %>%
#     mutate(area = st_area(.),
#            area = units::set_units(area, km^2))
#   
#   return(tibble(fg_area_km = sum(fishing_grounds$area, na.rm = T),
#                 fg_hours = sum(fishing_grounds$ground_hours, na.rm = T),
#                 fg_n = length(unique(fishing_grounds$cluster))))
# }

# X ----------------------------------------------------------------------------
# get_extensive <- function(data) {
#   # browser()
#   # Find vessels for which we don't have enough points'
#   # not_enough <- data %>%
#   #   count(vessel_rnpa) %>%
#   #   filter(n < 5) %>% 
#   #   pull(vessel_rnpa)
#   
#   # Convert to points
#   cch <- data %>% 
#     # filter(!vessel_rnpa %in% not_enough) %>%
#     # group_by(vessel_rnpa) %>% 
#     # nest() %>% 
#     st_as_sf(coords = c("lon", "lat"),
#              crs = 4326) %>% 
#     # mutate(data = map(data, st_as_sf,
#                       # coords = c("lon", "lat"),
#                       # crs = 4326)) %>% 
#     st_transform(crs = "+proj=lcc +lat_0=12 +lon_0=-102 +lat_1=17.5 +lat_2=29.5 +x_0=2500000 +y_0=0") %>%  # https://epsg.io/6361
#     area_concave_hull()
#   
#   # browser()
#   # Calculate area of fishing grounds
#   # cch <- pts %>%
#   #   mutate(data = map(data, area_concave_hull)) %>%
#   #   unnest(data)
# 
#   
#   return(cch)
# }