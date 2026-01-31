################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# jc_villasenor@miami.edu
# date
#
# Description
#
################################################################################
  
# SET UP #######################################################################

## Load packages ---------------------------------------------------------------
pacman::p_load(
  bigrquery,
  DBI,
  here,
  tidyverse,
  sf,
  rnaturalearth,
  ggspatial,
  dbscan
)

theme_set(
  theme_minimal(base_size = 10)
)

## Load data -------------------------------------------------------------------
tracks <- read_rds(file = here("data/processed/2019_shrimp_tracks.rds"))

bq_auth("juancarlos.villader@gmail.com")
con <- dbConnect(drv = bigquery(),
                 project = "mex-fisheries", 
                 dataset = "mex_vms",
                 billing = "mex-fisheries")

vms <- tbl(con, "mex_vms_processed_latest")

# PROCESSING ###################################################################

## Some step -------------------------------------------------------------------
track <- tracks |> 
  add_count(vessel_rnpa) |> 
  filter(n == max(n)) |> 
  st_as_sf(coords = c("lon", "lat"),
           crs = "EPSG:4326")

all_vms <- vms |> 
  filter(sql(paste0("vessel_rnpa = '", unique(track$vessel_rnpa), "'"))) |> 
  filter(year == 2019) |>
  collect() |> 
  st_as_sf(coords = c("lon", "lat"),
           crs = "EPSG:4326")

crop1 <- c("ymin" = 18,
           "ymax" = 27,
           "xmin" = -99,
           "xmax" = -90)

crop2 <- c("ymin" = 22,
           "ymax" = 26,
           "xmin" = -98,
           "xmax" = -95)

crop3 <- c("ymin" = 18,
           "ymax" = 20,
           "xmin" = -93.4,
           "xmax" = -92.4)

mex <- ne_countries(country = c("Mexico", "United States of America", "Guatemala"),
                    scale = "large") |> 
  st_crop(y = crop1)

mex_zoom <- mex |> 
  st_crop(y = crop2)

mex_zoom2 <- mex |> 
  st_crop(y = crop3)

## GET CLUSTERS ################################################################
# Build spatial object
# Find clusters
clusters <- track |> 
  st_transform(crs = "+proj=lcc +lat_0=12 +lon_0=-102 +lat_1=17.5 +lat_2=29.5 +x_0=2500000 +y_0=0") |> 
  st_coordinates() |> 
  dbscan(eps = 25e3, # Distance in meters
         minPts = 50) # Minimum points per cluster)

# Calculate convex hull and area of each cluster
results <- track |> 
  mutate(cluster = clusters$cluster) |> 
  filter(!cluster == 0) |> 
  st_transform(crs = "EPSG:4326")

fg <- results |> 
  group_by(year, eu_rnpa, vessel_rnpa, cluster) %>%
  summarize(ground_hours = sum(hours, na.rm = T)) %>%
  st_convex_hull() %>% 
  mutate(area = st_area(.),
         area = units::set_units(area, km^2))

# VISUALIZE ####################################################################

## Another step ----------------------------------------------------------------
p1 <- ggplot() + 
  geom_sf(data = mex) +
  geom_sf(data = all_vms,
          pch = ".") +
  labs(title = "1) Raw VMS positions",
       x = "",
       y = "") +
  annotation_scale(location = "tr") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

p2 <- ggplot() + 
  geom_sf(data = mex) +
  geom_sf(data = track,
          pch = ".") +
  geom_rect(aes(ymin = 18.1,
                ymax = 19.5,
                xmin = -93.4,
                xmax = -92.4),
            fill = NA,
            color = "red",
            linewidth = 1) +
  labs(title = "2) VMS after filtering for speed and depth",
       x = "",
       y = "") +
  annotation_scale(location = "tr") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

p3 <- ggplot() + 
  geom_sf(data = mex_zoom2) +
  geom_sf(data = results |> st_crop(crop3), pch = ".") +
  labs(title = "3) Zoom in on filtered VMS positions",
       x = "",
       y = "") +
  annotation_scale(location = "tl") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

p4 <- ggplot() + 
  geom_sf(data = mex_zoom2) +
  geom_sf(data = results |> st_crop(crop3),
          pch = ".",
          aes(color = factor(cluster)), show.legend = F) +
  labs(title = "4) Clustering to identify fishing grounds",
       x = "",
       y = "") +
  annotation_scale(location = "tl") +
  scale_color_brewer(palette = "Paired") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

p5 <- ggplot() + 
  geom_sf(data = mex_zoom2) +
  geom_sf(data = results |> st_crop(crop3),
          pch = ".",
          aes(color = factor(cluster)),
          show.legend = F) +
  geom_sf(data = fg |> st_crop(crop3),
          aes(color = factor(cluster)),
          show.legend = F,
          fill = "transparent") +
  labs(title = "5) MCP around cluster members",
       x = "",
       y = "") +
  annotation_scale(location = "tl") +
  scale_color_brewer(palette = "Paired") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

p6 <- ggplot() + 
  geom_sf(data = mex_zoom2) +
  geom_sf(data = fg |> st_crop(crop3),
          aes(fill = factor(cluster)),
          show.legend = F) +
  labs(title = "6) Calculate extent of MCP",
       x = "",
       y = "") +
  annotation_scale(location = "tl") +
  scale_fill_brewer(palette = "Paired") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

P <- cowplot::plot_grid(p1, p2, p3,
                        p4, p5, p6,
                        ncol = 3)

# EXPORT #######################################################################

## The final step --------------------------------------------------------------
out_dir <- here("content/figures/")

W <- 4
H <- 4
  
# Export individual plots
ggsave(plot = p1,
       filename = here(out_dir, "fig_vms_pipeline_example_1.pdf"),
       width = W,
       height = H)
ggsave(plot = p2,
       filename = here(out_dir, "fig_vms_pipeline_example_2.pdf"),
       width = W,
       height = H)
ggsave(plot = p3,
       filename = here(out_dir, "fig_vms_pipeline_example_3.pdf"),
       width = W,
       height = H)
ggsave(plot = p4,
       filename = here(out_dir, "fig_vms_pipeline_example_4.pdf"),
       width = W,
       height = H)
ggsave(plot = p5,
       filename = here(out_dir, "fig_vms_pipeline_example_5.pdf"),
       width = W,
       height = H)
ggsave(plot = p6,
       filename = here(out_dir, "fig_vms_pipeline_example_6.pdf"),
       width = W,
       height = H)

ggsave(plot = P,
       filename = here(out_dir, "fig_vms_pipeline_example.pdf"),
       width = W * 3,
       height = H * 2)

