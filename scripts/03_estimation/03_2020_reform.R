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
              ..log_outcomes = ~c(log(hours), log(fg_area_km), log(live_weight)),
              ..es = ~i(year, "2019") | eu,
              ..post = ~post | eu)

# Standard errors clustered by economic unit by default
setFixest_vcov(all = "cluster", no_FE = "iid")

## PROCESSING ##################################################################
## Descriptive stats for the text ----------------------------------------------
# There are three types of economic units:
# Type 1 - Ones that left the fishery altogether
# Type 2 - Ones that left and came out
# Type 3 - Ones that never left

# First time out
first_time <- shrimp_panel |> 
  filter(n_times_sub == 9,
         year >= 2020,
         hours == 0) |> 
  group_by(eu) |>
  summarize(first_year_out = min(year)) |> 
  ungroup()

# Last time still in
last_time <- shrimp_panel |> 
  filter(n_times_sub == 9,
         year >= 2019,
         hours > 0) |> 
  group_by(eu) |>
  summarize(last_year_in = max(year)) |> 
  ungroup()

# Type 1 EUs
left <- shrimp_panel |> 
  filter(n_times_sub == 9) |> 
  select(eu) |> 
  distinct() |> 
  left_join(first_time, by = "eu") |> 
  left_join(last_time, by = "eu") |> 
  filter(first_year_out > last_year_in) |> 
  pull(eu)

# Type 2 EUs
left_and_came <- shrimp_panel |> 
  filter(n_times_sub == 9) |> 
  select(eu) |> 
  distinct() |> 
  left_join(first_time, by = "eu") |> 
  left_join(last_time, by = "eu") |> 
  filter(!first_year_out > last_year_in) |> 
  pull(eu)

# Type 3 EUs
never_left <- shrimp_panel |> 
  filter(n_times_sub == 9,
         year >= 2020,
         hours > 0) |> 
  group_by(eu) |> 
  filter(n_distinct(year) == 5) |> 
  select(eu) |> 
  distinct() |> 
  pull(eu)

# Take a look at the numbers
length(left)
length(left_and_came)
length(never_left)

(length(left) + length(left_and_came) + length(never_left)) == shrimp_panel |> filter(n_times_sub == 9) |> pull(eu) |> n_distinct()

# Exiting fishery altogether
exit_panel <- shrimp_panel |> 
  left_join(first_time) |> 
  mutate(exited = eu %in% left) |> 
  mutate(exit = 0,
         exit = ifelse((exited & year >= first_year_out), 1, 0))

# ESTIMATION ###################################################################
## Estimate models -------------------------------------------------------------

# 1) Probability of exiting the fishery altogether
# Event study
event_study_exit <- feols(exit ~ ..es,
                          data = exit_panel,
                          panel.id = ~eu + year,
                          subset = ~n_times_sub == 9)

# Pre-post
prepost_exit <- feols(exit ~ ..post,
                      data = exit_panel,
                      panel.id = ~eu + year,
                      subset = ~n_times_sub == 9)


# 2) Extensive outcomes
prepost_ext <- feols(..ext_outcomes ~ ..post,
                     data = exit_panel,
                     panel.id = ~eu + year,
                     subset = ~n_times_sub == 9 & !exited) |> 
  set_names(model_names)

# 3) Intensive outcomes, levels
# Event study
event_study_levels <- feols(..level_outcomes ~ ..es,
                            data = exit_panel,
                            panel.id = ~eu + year,
                            subset = ~n_times_sub == 9 & !exited) |> 
  set_names(model_names)

# Pre-post
prepost_levels <- feols(..level_outcomes ~ ..post,
                        data = exit_panel,
                        panel.id = ~eu + year,
                        subset = ~n_times_sub == 9 & !exited) |> 
  set_names(model_names)

# 4) Intensive outcomes, logs
# Pre-post
prepost_semi_elasticity <- feols(..log_outcomes ~ ..post,
                                 data = exit_panel,
                                 panel.id = ~eu + year,
                                 subset = ~n_times_sub == 9 & !exited) |> 
  set_names(model_names)

# EXPORT #######################################################################

## Export models ---------------------------------------------------------------

# For figure
write_rds(x = event_study_exit,
          file = here("data/output/es_self_reform_model_p_exit.rds"))
write_rds(x = event_study_levels,
          file = here("data/output/es_self_reform_levels.rds"))

# For tables
write_rds(x = prepost_exit,
          file = here("data/output/prepost_reform_model_p_exit.rds"))
write_rds(x = prepost_ext,
          file = here("data/output/prepost_reform_model_ext.rds"))
write_rds(x = prepost_levels,
          file = here("data/output/prepost_reform_model_levels.rds"))
write_rds(x = prepost_semi_elasticity,
          file = here("data/output/prepost_reform_model_semi_elasticity.rds"))



  