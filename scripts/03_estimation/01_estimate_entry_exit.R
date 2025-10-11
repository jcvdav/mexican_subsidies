################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Estimates changes in fishing time, fished area, and landings in response to 
# changes in subsidy status.
#
# The unit of observation is an economic unit by year
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  fixest,
  broom,
  tidyverse
)

# Load data --------------------------------------------------------------------
shrimp_panel <- readRDS(here("data", "estimation_panels", "shrimp_estimation_panel.rds")) |> 
  filter(year <= 2019)

# Set some defaults ------------------------------------------------------------
# Model names to use by fixest
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

setFixest_fml(..outcomes = ~c(log(hours), log(fg_area_km), log(live_weight)),
              ..level_outcomes = ~c(hours, fg_area_km, live_weight),
              ..ext_outcomes = ~c(hours > 0, fg_area_km > 0, live_weight > 0),
              ..twfe = ~treated | eu + year ^ region,
              ..covs = ~treated + n_vessels + total_hp + log(mean_diesel_price_mxn_l) +
                nino34_m:region)

setFixest_vcov(panel = "cluster")
## ESTIMATION ##################################################################

# 1) Log-linear specification --------------------------------------------------
# 1a) TWFE with economic units whose subsidy status changes only
semi_elasticity_twfe <-
  feols(..outcomes ~  ..twfe,
        data = shrimp_panel,
        panel.id = ~eu + year,
        subset = ~sometimes == 1) |> 
  set_names(model_names)

etable(semi_elasticity_twfe)

# 1b) Add covariates instead of fixed effects
semi_elasticity_cov <- 
  feols(..outcomes ~ ..covs,
        data = shrimp_panel,
        panel.id = ~eu + year,
        subset = ~sometimes == 1) %>% 
  set_names(model_names)

etable(semi_elasticity_cov)

# 1c) Repeat main estimation but include all economic units
semi_elasticity_twfe_fs <-
  feols(..outcomes  ~  ..twfe,
        data = shrimp_panel,
        panel.id = ~eu + year) %>% 
  set_names(model_names)

etable(semi_elasticity_twfe_fs)

# 1d) Repeat main estimation but exclude economic units who received new vessels
semi_elasticity_twfe_modern <-
  feols(..outcomes ~  ..twfe,
        data = shrimp_panel,
        panel.id = ~eu + year,
        subset = ~sometimes == 1 & modernized == 0) |> 
  set_names(model_names)

etable(semi_elasticity_twfe_modern)

# 2) Estimate in levels --------------------------------------------------------
# 2a) Main specification
levels_twfe <-
  feols(..level_outcomes ~  ..twfe,
        data = shrimp_panel,
        panel.id = ~eu + year,
        subset = ~sometimes == 1) |> 
  set_names(model_names)

etable(levels_twfe)

# 2b) Covariates
levels_cov <-
  feols(..level_outcomes ~  ..covs,
        data = shrimp_panel,
        panel.id = ~eu + year,
        subset = ~sometimes == 1) |> 
  set_names(model_names)

etable(levels_cov)

# 2c) 
levels_twfe_fs <-
  feols(..level_outcomes  ~  ..twfe,
        data = shrimp_panel,
        panel.id = ~eu + year) %>% 
  set_names(model_names)

etable(levels_twfe_fs)

# 2d) Without modernized eus
levels_twfe_modern <-
  feols(..level_outcomes ~  ..twfe,
        data = shrimp_panel,
        panel.id = ~eu + year,
        subset = ~sometimes == 1 & modernized == 0) |> 
  set_names(model_names)

etable(levels_twfe_modern)

# 3) Fishing / not fishing -----------------------------------------------------
# 3a) Main specification
ext_twfe <-
  feols(..ext_outcomes ~  ..twfe,
        data = shrimp_panel,
        panel.id = ~eu + year,
        subset = ~sometimes == 1) |> 
  set_names(model_names)

etable(ext_twfe)

# 3b) Covariates
ext_cov <-
  feols(..ext_outcomes ~  ..covs,
        data = shrimp_panel,
        panel.id = ~eu + year,
        subset = ~sometimes == 1) |> 
  set_names(model_names)

etable(ext_cov)

# 3c) 
ext_twfe_fs <-
  feols(..ext_outcomes  ~  ..twfe,
        data = shrimp_panel,
        panel.id = ~eu + year) %>% 
  set_names(model_names) |> 
  set_names(model_names)

etable(ext_twfe_fs)

# 3d) Without modern
ext_twfe_modern <-
  feols(..ext_outcomes ~  ..twfe,
        data = shrimp_panel,
        panel.id = ~eu + year,
        subset = ~sometimes == 1 & modernized == 0) |> 
  set_names(model_names)

etable(ext_twfe_modern)

# 4) Restrict sample to EUs subsidized at most n_times -------------------------
n_eus <- function(model){
  tibble(neus = length(unique(fixef(model, notes = F)$eu)))
}

restrict_n_times <- function(n_times = 8, model_type = "elasticity"){
  inside_data <- shrimp_panel |> 
    filter(sometimes == 1) |> 
    filter(n_times_sub <= n_times)
  
  # Define which type of model we are fitting
  if(model_type == "elasticity") {
    models <- feols(..outcomes ~ ..twfe,
                    data = inside_data,
                    panel.id = ~eu + year) %>% 
      set_names(model_names)
  } else if (model_type == "levels") {
    models <- feols(..level_outcomes ~ ..twfe,
                    data = inside_data,
                    panel.id = ~eu + year) %>% 
      set_names(model_names)
  } else if (model_type == "ext") {
    models <- feols(..ext_outcomes ~ ..twfe,
                    data = inside_data,
                    panel.id = ~eu + year) %>% 
      set_names(model_names)
  }
  
  coefs <- 
    map_dfr(models, tidy, conf.int = T, .id = "var") |> 
    filter(term == "treated")
  
  nobs <- map_dfr(models, glance, .id = "var") |> 
    select(var, nobs)
  
  neus <- map_dfr(models, n_eus, .id = "var")
  
  results <- coefs |> 
    left_join(nobs, by = "var") |> 
    left_join(neus, by = "var") |> 
    mutate(n_times = n_times)
  
  return(results)
}

subsidized_n_times_semi_elasticity_models <- map_dfr(1:8, restrict_n_times, model_type = "elasticity")
subsidized_n_times_levels_models <- map_dfr(1:8, restrict_n_times, model_type = "levels")
subsidized_n_times_ext_models <- map_dfr(1:8, restrict_n_times, model_type = "ext")

## COLLECT ALL MODELS ##########################################################
all_semi_elasticity_models <- list("TWFE sometimes sub." = semi_elasticity_twfe,
                                   "Cov sometimes sub." = semi_elasticity_cov,
                                   "TWFE all" = semi_elasticity_twfe_fs,
                                   "TWFE modernized" = semi_elasticity_twfe_modern)

all_level_models <- list("TWFE sometimes sub." = levels_twfe,
                         "Cov sometimes sub." = levels_cov,
                         "TWFE all" = levels_twfe_fs,
                         "TWFE modernized" = levels_twfe_modern)

all_ext_models <- list("TWFE sometimes sub." = ext_twfe,
                       "Cov sometimes sub." = ext_cov,
                       "TWFE all" = ext_twfe_fs,
                       "TWFE modernized" = ext_twfe_modern)

# EXPORT #######################################################################
output_dir <- "data/output"

# Export main specifications of each model group
saveRDS(object = semi_elasticity_twfe,
        file = here(output_dir, "semi_elasticity_twfe_model.rds"))
saveRDS(object = levels_twfe,
        file = here(output_dir, "levels_model.rds"))
saveRDS(object = ext_twfe,
        file = here(output_dir, "ext_model.rds"))

# Export groups of models
saveRDS(object = all_semi_elasticity_models,
        file = here(output_dir, "all_semi_elasticity_models.rds"))
saveRDS(object = all_level_models,
        file = here(output_dir, "all_level_models.rds"))
saveRDS(object = all_ext_models,
        file = here(output_dir, "all_ext_models.rds"))

# Models by n-times subsidized
saveRDS(object = subsidized_n_times_semi_elasticity_models,
        file = here(output_dir, "subsidized_n_times_semi_elasticity_models.rds"))
saveRDS(object = subsidized_n_times_levels_models,
        file = here(output_dir, "subsidized_n_times_levels_models.rds"))
saveRDS(object = subsidized_n_times_ext_models,
        file = here(output_dir, "subsidized_n_times_ext_models.rds"))

