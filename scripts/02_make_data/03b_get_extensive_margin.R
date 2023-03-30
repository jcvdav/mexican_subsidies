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
library(here)
library(concaveman)
library(sf)
library(tidyverse)

sf_use_s2(FALSE)

# Load data --------------------------------------------------------------------

shrimp_tracks <- tibble(file = list.files(path = here("data"),
                                          pattern = "_shrimp_tracks.rds",
                                          full.names = T)) %>% 
  mutate(year = str_extract(file, pattern = "[:digit:]{4}"),
         data = map(file, readRDS)) %>% 
  select(-file)

mex <- rnaturalearth::ne_countries(country = "Mexico", returnclass = "sf")

## PROCESSING ##################################################################

# Define area calculations -----------------------------------------------------
area_concave_hull <- function(x) {
  concave_hull <- concaveman(x, concavity = 10)
  erased <- rmapshaper::ms_erase(concave_hull, mex) 
  area <- st_area(st_make_valid(erased))
  
  return(area)
}

# X ----------------------------------------------------------------------------
get_extensive <- function(data) {
  # Find vessels for which we don't have enough points'
  not_enough <- data %>%
    count(vessel_rnpa) %>%
    filter(n < 3) %>% 
    pull(vessel_rnpa)
  
  pts <- data %>% 
    filter(!vessel_rnpa %in% not_enough,
           between(speed, 1.5, 4)) %>%
    select(eu_rnpa, vessel_rnpa, lon, lat) %>% 
    group_by(eu_rnpa, vessel_rnpa) %>% 
    nest() %>% 
    mutate(data = map(data, st_as_sf, coords = c("lon", "lat"), crs = 4326))
  
  cch <- pts %>% 
    mutate(area = map_dbl(data, area_concave_hull)) %>% 
    select(-data)
}


extensive <- shrimp_tracks %>% 
  mutate(area = map(data, get_extensive)) %>% 
  select(year, area) %>% 
  unnest(area) %>% 
  group_by(year, eu_rnpa) %>%
  summarize(area = sum(area))



## EXPORT ######################################################################

# X ----------------------------------------------------------------------------

saveRDS(object = extensive,
        file = here("data", "extensive_margin.rds"))



