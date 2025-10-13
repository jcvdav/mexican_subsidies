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
  here,
  fixest,
  tidyverse
)

## Load data -------------------------------------------------------------------
shrimp_panel <- readRDS(here("data", "estimation_panels", "shrimp_estimation_panel.rds")) |> 
  mutate(post = ifelse(year >= 2020, 1, 0))

# PROCESSING ###################################################################

# Define some defaults ---------------------------------------------------------
setFixest_dict(
  # Outcomes of interest
  c(# Log outcomes
    "log(hours)" = "Fishing time",
    "log(fg_area_km)" = "Fishing area",
    "log(live_weight)" = "Landings",
    # Levels
    "hours" = "Fishing time",
    "fg_area_km" = "Fishing area",
    "live_weight" = "Landings",
    # Extensive
    "hours > 0" = "Fishing time",
    "fg_area_km > 0" = "Fishing area",
    "live_weight > 0" = "Landings",
    # Variables
    "log(ph)" = "log(fuel price)",
    "treated" = "Subsidized",
    "n_vessels" = "vessels",
    # Fixed effects
    "eu" = "Economic Unit",
    "year^region" = "Region-by-year"))

# Model names so that modelsummary represents them
model_names <- c("Fishing time", "Fishing area", "Landings")

setFixest_fml(..ext_outcomes = ~c(hours == 0, fg_area_km == 0, live_weight == 0),
              ..level_outcomes = ~c(hours, fg_area_km, live_weight),
              ..es_self = ~i(year, "2019") | eu,
              ..post = ~post | eu)

# Standard errors clustered by economic unit by default
setFixest_vcov(all = "cluster", no_FE = "iid")
# ESTIMATION ###################################################################

## Estimate models -------------------------------------------------------------
# Extensvie outcomes
# Event study
event_study_self_ext <- feols(..ext_outcomes ~ ..es_self,
                              data = shrimp_panel,
                              panel.id = ~eu + year,
                              subset = ~n_times_sub == 9) |> 
  set_names(model_names)

# Pre-post
prepost_self_ext <- feols(..ext_outcomes ~ ..post,
                          data = shrimp_panel,
                          panel.id = ~eu + year,
                          subset = ~n_times_sub == 9) |> 
  set_names(model_names)

# Intensive outcomes
# Event study
event_study_self_levels <- feols(..level_outcomes ~ ..es_self,
                                 data = shrimp_panel,
                                 panel.id = ~eu + year,
                                 subset = ~n_times_sub == 9) |> 
  set_names(model_names)

# Pre-post
prepost_self_levels <- feols(..level_outcomes ~ ..post,
                             data = shrimp_panel,
                             panel.id = ~eu + year,
                             subset = ~n_times_sub == 9) |> 
  set_names(model_names)

# EXPORT #######################################################################

## Export models ---------------------------------------------------------------

write_rds(x = event_study_self_ext,
          file = here("data/output/es_self_reform_model_ext.rds"))
write_rds(x = event_study_self_levels,
          file = here("data/output/es_self_reform_model_levels.rds"))


write_rds(x = prepost_self_ext,
          file = here("data/output/prepost_reform_model_ext.rds"))
write_rds(x = prepost_self_levels,
          file = here("data/output/prepost_reform_model_levels.rds"))






  