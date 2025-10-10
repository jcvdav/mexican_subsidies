################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# jc_villasenor@miami.edu
# date
#
# Build a figure that a histogram of the number of times a vessel is subsidized
#
################################################################################
  
# SET UP #######################################################################

## Load packages ---------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse
)

## Load data -------------------------------------------------------------------
shrimp_panel <- readRDS(here("data", "estimation_panels", "shrimp_estimation_panel.rds")) |> 
  filter(year <= 2019)

# PROCESSING ###################################################################

## Get unique data -------------------------------------------------------------
data <- shrimp_panel |> 
  select(eu, n_times_sub) |> 
  distinct()

# VISUALIZE ####################################################################

## Build plot ------------------------------------------------------------------
n_times_sub <- ggplot(data = data,
                      mapping = aes(x = n_times_sub)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(labels = c(0:9), breaks = c(0:9)) +
  labs(x = "Number of times subsidized (2011-2019)",
       y = "Number of  economic units") 

# EXPORT #######################################################################
## Save figure -----------------------------------------------------------------
ggsave(plot = n_times_sub,
       filename = here("content", "figures", "fig_N_times_subsidized_histogram.pdf"),
       width = 6,
       height = 4,
       units = "in")

