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

get_utm_zone <- function(spat) {
  # determine projection to use based on lon/lat distribution
  coords <- sf::st_coordinates(spat)    # X=lon, Y=lat
  lons <- coords[, "X"]
  lats <- coords[, "Y"]
  
  # helper: compute UTM zone for a longitude
  lon_to_utm_zone <- function(lon) {
    zone <- floor((lon + 180) / 6) + 1L
    return(as.integer(zone))
  }
  
  zones <- sort(unique(lon_to_utm_zone(lons)))
  
  # centroid lon/lat for robust single-zone selection
  centroid_lon <- mean(lons, na.rm = TRUE)
  centroid_lat <- mean(lats, na.rm = TRUE)
  centroid_zone <- lon_to_utm_zone(centroid_lon)
  
  # choose EPSG depending on hemisphere
  if(length(zones) == 1) {
    # single UTM zone
    if(centroid_lat >= 0) {
      utm_epsg <- 32600 + zones[1]   # northern hemisphere
    } else {
      utm_epsg <- 32700 + zones[1]   # southern hemisphere
    }
    crs <- sf::st_crs(utm_epsg)
  } else if(length(zones) == 2 && abs(zones[2] - zones[1]) == 1) {
    # two adjacent zones -> pick the zone containing the centroid longitude
    if(centroid_lat >= 0) {
      utm_epsg <- 32600 + centroid_zone
    } else {
      utm_epsg <- 32700 + centroid_zone
    }
    crs <- sf::st_crs(utm_epsg)
  } else {
    # fallback: single Azimuthal Equidistant centered on the Gulf of Mexico
    center_lon <- -90
    center_lat <- 25
    aeqd_proj <- sprintf(
      "+proj=aeqd +lat_0=%f +lon_0=%f +datum=WGS84 +units=m +no_defs", center_lat, center_lon)
    crs <- sf::st_crs(aeqd_proj)
  }
  
  return(crs)
}

get_extensive <- function(data) {
  
  npts <- dim(data)[1]
  
  results <- data %>% 
    select(year, eu_rnpa) %>% 
    distinct() %>% 
    mutate(fg_area_km = 0,
           fg_hours = 0,
           fg_n = 0,
           n_pts = npts)
  
  # Only proceed if there are more than 5 observations
  if(npts >= 150) {
    
    # Build spatial object
    spat <- data %>%
      st_as_sf(coords = c("lon", "lat"),
               crs = 4326)
    
    utm_crs <- get_utm_zone(spat)
    
    spat <- st_transform(spat,
                         crs = utm_crs)
    
    
    # Find clusters
    clusters <- spat %>%
      st_coordinates() %>%
      dbscan(eps = 25e3, # Distance in meters
             minPts = 50 # Minimum points per cluster
      )
    
    n_clust <- max(clusters$cluster)
    
    # Only proceed if there is at least one cluster (cluster # 0 is "noise" so it doesn't count)
    if(n_clust > 0) {
      # Calculate convex hull and area of each cluster
      results <- spat %>%
        mutate(cluster = clusters$cluster) %>%
        filter(!cluster == 0) %>% # Remove points not part of a cluster
        group_by(year, eu_rnpa, cluster) %>%
        summarize(ground_hours = sum(hours, na.rm = T),
                  .groups = "drop") %>%
        st_convex_hull() %>% 
        mutate(area = st_area(.),
               area = units::set_units(area, km^2)) %>% 
        st_drop_geometry() %>% 
        group_by(year, eu_rnpa) %>% 
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
extensive <- shrimp_tracks %>% 
  pull(data) %>% 
  bind_rows() %>%
  select(year, eu_rnpa, lon, lat, hours) %>% 
  group_by(year, eu_rnpa) %>%
  group_split() %>% 
  future_map_dfr(.f = get_extensive)
plan(sequential)
beepr::beep(2)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------

saveRDS(object = extensive,
        file = here("data", "processed", "extensive_margin.rds"))


