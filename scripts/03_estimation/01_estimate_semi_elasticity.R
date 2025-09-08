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
shrimp_panel <- readRDS(here("data", "estimation_panels", "shrimp_estimation_panel.rds"))

# Set some defaults ------------------------------------------------------------
# Model names to use by fixest
setFixest_dict(
  # Outcomes of interest
  c("log(hours)" = "Fishing time",
    "log(fg_area_km)" = "Fishing area",
    "log(landed_weight)" = "Landings",
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
              ..twfe = ~treated | eu + year ^ region,
              ..covs = ~treated + n_vessels + total_hp + log(mean_diesel_price_mxn_l) +
                nino34_m:region)

## ESTIMATION ##################################################################
# 1) Main specification --------------------------------------------------------
# TWFE with economic units whose subsidy status changes only
semi_elasticity_twfe <-
  feols(..outcomes ~  ..twfe,
        data = shrimp_panel,
        panel.id = ~eu + year,
        vcov = "NW",
        subset = ~sometimes == 1) |> 
  set_names(model_names)

etable(semi_elasticity_twfe)

# 2) Alternative specifications ----------------------------------------------------
# 2a) Add covariates instead of fixed effects
semi_elasticity_cov <- 
  feols(..outcomes ~ ..covs,
        data = shrimp_panel,
        panel.id = ~eu + year,
        vcov = "NW",
        subset = ~sometimes == 1) %>% 
  set_names(model_names)

etable(semi_elasticity_cov)

# 2b) Repeat main estimation but include all economic units
semi_elasticity_twfe_fs <-
  feols(..outcomes  ~  ..twfe,
        data = shrimp_panel,
        panel.id = ~eu + year,
        vcov = "NW") %>% 
  set_names(model_names)

etable(semi_elasticity_twfe_fs)

all_models <- list("TWFE sometimes sub." = semi_elasticity_twfe,
                   "Cov sometimes sub." = semi_elasticity_cov,
                   "TWFE all" = semi_elasticity_twfe_fs)

# 3) Restrict sample to EUs subsidized at most n_times -------------------------
n_eus <- function(model){
  tibble(neus = length(unique(fixef(model, notes = F)$eu)))
}

restrict_n_times <- function(n_times = 8){
  inside_data <- shrimp_panel |> 
    filter(sometimes == 1) |> 
    filter(n_times_sub <= n_times)
  
  models <- feols(..outcomes ~ ..twfe,
                  data = inside_data,
                  panel.id = ~eu + year,
                  vcov = "NW") %>% 
    set_names(model_names)
  
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

subsidized_n_times_models <- map_dfr(1:8, restrict_n_times)

# EXPORT #######################################################################
output_dir <- "data/output"

saveRDS(object = semi_elasticity_twfe,
        file = here(output_dir, "semi_elasticity_twfe_model.rds"))

saveRDS(object = all_models,
        file = here(output_dir, "all_semi_elasticity_models.rds"))

saveRDS(object = subsidized_n_times_models,
        file = here(output_dir, "subsidized_n_times_semi_elasticity_models.rds"))

