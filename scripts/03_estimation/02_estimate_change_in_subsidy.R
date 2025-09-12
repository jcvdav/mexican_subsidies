################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
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
shrimp_panel_raw <- readRDS(here("data", "estimation_panels", "shrimp_estimation_panel.rds")) |> 
  filter(year <= 2019)

## PROCESSING ##################################################################
shrimp_panel <- shrimp_panel_raw %>% 
  filter(treated == 1,
         n_times_sub >= 2)

## ESTIMATION ##################################################################
# Define some defaults ---------------------------------------------------------
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
              ..twfe = ~log(subsidy_pesos) | eu + year^region,
              ..covs = ~log(subsidy_pesos) + n_vessels + total_hp + nino34_m:region)


# Main specification -----------------------------------------------------------
# 1) Main specification --------------------------------------------------------
# TWFE with economic units subsidized at least twice and that are subsidized
elasticity_twfe <- feols(fml = ..outcomes ~ ..twfe,
                         data = shrimp_panel,
                         panel.id = ~eu + year,
                         vcov = "NW") %>% 
  set_names(model_names)

etable(elasticity_twfe)


# 2) Alternative specifications ------------------------------------------------
# 2a) Same as main specification but only vessels always subsidized
elasticity_twfe_always <- feols(fml = ..outcomes ~ ..twfe,
                                data = shrimp_panel,
                                panel.id = ~eu + year,
                                subset = ~always == 1,
                                vcov = "NW") %>% 
  set_names(model_names)

etable(elasticity_twfe_always)

# 2b) Same as main specification but only vessels sometimes subsidized
elasticity_twfe_sometimes <- feols(fml = ..outcomes ~ ..twfe,
                                data = shrimp_panel,
                                panel.id = ~eu + year,
                                subset = ~sometimes == 1,
                                vcov = "NW") %>% 
  set_names(model_names)

etable(elasticity_twfe_sometimes)

# 2c) Add covariates instead of fixed effects
elasticity_cov <- twfe <- feols(fml = ..outcomes ~ ..covs,
                                data = shrimp_panel,
                                panel.id = ~eu + year,
                                vcov = "NW") %>% 
  set_names(model_names)

etable(elasticity_cov)

all_models <- list("TWFE" = elasticity_twfe,
                   "TWFE Always" = elasticity_twfe_always,
                   "TWFE Sometimes" = elasticity_twfe_sometimes,
                   "Cov" = elasticity_cov)

# 3) Restrict sample to EUs subsidized at least n_times -------------------------
n_eus <- function(model){
  tibble(neus = length(unique(fixef(model, notes = F)$eu)))
}

restrict_n_times <- function(n_times = 8){
  # browser()
  inside_data <- shrimp_panel |> 
    filter(n_times_sub >= n_times,
           treated == 1)
  
  models <- feols(..outcomes ~ ..twfe,
                  data = inside_data,
                  panel.id = ~eu + year,
                  vcov = "NW") %>% 
    set_names(model_names)
  
  coefs <- map_dfr(models, tidy, conf.int = T, .id = "var") |> 
    filter(term == "log(subsidy_pesos)")
  
  nobs <- map_dfr(models, glance, .id = "var") |> 
    select(var, nobs)
  
  neus <- map_dfr(models, n_eus, .id = "var")
  
  results <- coefs |> 
    left_join(nobs, by = "var") |> 
    left_join(neus, by = "var") |> 
    mutate(n_times = n_times)
  
  return(results)
}


subsidized_n_times_models <- map_dfr(2:9, restrict_n_times)

## EXPORT ######################################################################
output_dir <- "data/output"

saveRDS(object = elasticity_twfe,
        file = here(output_dir, "elasticity_twfe_model.rds"))

saveRDS(object = all_models,
        file = here(output_dir, "all_elasticity_models.rds"))

saveRDS(object = subsidized_n_times_models,
        file = here(output_dir, "subsidized_n_times_elasticity_models.rds"))

## BUILD FIGURE ################################################################

