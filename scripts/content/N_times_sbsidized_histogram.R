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
library(cowplot)
library(tidyverse)

# Load data --------------------------------------------------------------------

shrimp_panel <- readRDS(
  file = file.path(
    project_path,
    "data",
    "processed_data",
    "shrimp_estimation_panel.rds"
  )
)

## PROCESSING ##################################################################

# Build data -------------------------------------------------------------------
data <-  shrimp_panel %>%
  mutate(treated = treated == 1) %>% 
  filter(treated) %>% 
  count(eu)

## VISUALIZE ###################################################################

# Build plot -------------------------------------------------------0-----------

p <- ggplot(data = data,
       mapping = aes(x = n)) + 
  geom_histogram(bins = 8) +
  labs(x = "Number of times subsidized",
       y = "Number of vessels")

## EXPORT ######################################################################

# export plot ------------------------------------------------------------------
ggsave(
  plot = p,
  filename = here("results", "img", "N_times_subsidized_histogram.pdf"),
  width = 6,
  height = 4.5
)
