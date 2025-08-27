################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# jc_villasenor@miami.edu
# date
#
# This script build the regression table for the semi-elasticity estimates
# The models are estimated in scripts/03_estimation/01_estimate_semi_elasticity.R
#
################################################################################
  
# SET UP #######################################################################

## Load packages ---------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse,
  fixest,
  modelsummary
)

## Load data -------------------------------------------------------------------
semi_elasticity_twfe <- readRDS(here("data", "output", "semi_elasticity_twfe_model.rds"))

all_models <- readRDS(file = here("data", "output", "all_semi_elasticity_models.rds")) |> 
  set_names(c("A) Main text specification",
              "B) Covariates but no fixed effects",
              "C) Main text specification with all economic units"))

# PROCESSING ###################################################################
# Function to extract number of observations in each model
n_eus <- function(model){
  as.character(length(unique(fixef(model, notes = F)$eu)))
}

# Function to convert coefficients to %
coef_to_pct <- function(model){
  scales::percent((exp(coefficients(model)[1])-1), accuracy = 0.01, suffix = "\\%")
}


## Definbe modelsummary rpesets ------------------------------------------------
# Information to omit from the regression tables to make the more tidy
omit <- "(Intercept)|RMSE|With|IC|n_vessels|total_hp|nino|diesel"

# Change the appearance of what will appear in the regression table
gm <- tribble(~raw, ~clean, ~fmt,
              "nobs", "N", 0,
              "adj.r.squared", "$R^2$ Adj", 3
)

coefs <- c("treated" = "Subsidized")

# Calculate percent changes to add to the table --------------------------------
extra <- bind_rows(map_dfc(semi_elasticity_twfe, coef_to_pct),
                   map_dfc(semi_elasticity_twfe, n_eus)) |> 
  mutate(var = c("\\%Change",
                 "$N_{eu}$")) |> 
  select(var, everything())


attr(extra, 'position') <- c(3, 4)

# VISUALIZE ####################################################################
# Build and export main table --------------------------------------------------
msummary(models = semi_elasticity_twfe,
         stars = panelsummary:::econ_stars(),
         coef_omit = omit,
         coef_rename = coefs,
         gof_map = gm,
         add_rows = extra,
         output = here("content", "tables", "tab_semi_elasticity.tex"),
         title = "\\label{tab:semi_elasticity}Effect of receiving a fuel subsidy on time fishing (hours), fishing area ($\\text{km}^2$), and landings (kg).",
         notes = ("The unit of observation is an economic unit by year.
                  All models include fixed effects by economic unit and by region-year.
                  Numbers in parentheses are panel-robust standard errors (Newey-West with a 1yr lag).
                  Differences in sample size across columns are due to missing coordinates on some
                  VMS messages-fishing area can not be estimated- or because landings data were not available.
                  The number of economic units used in each column is shown by $N_{eu}$."),
         escape = F)

## The final step --------------------------------------------------------------

msummary(models = all_models,
         shape = "rbind",
         stars = panelsummary:::econ_stars(),
         coef_omit = omit,
         coef_rename = coefs,
         gof_map = gm,
         output = here("content", "tables", "tab_semi_elasticity_all_estimates.tex"),
         title = "\\label{tab:supp_semi_elasticity}Effect of receiving a fuel subsidy on time fishing (hours), fishing area ($\\text{km}^2$), and landings (kg).",
         notes = ("The unit of observation is an economic unit by year.
         Numbers in parentheses are panel-robust standard errors (Newey-West with a 1yr lag).
                  Panel A) shows the same information as in \\autoref{tab:semi_elasticity}.
                  Panel B) uses the same sample of vessels subsidized at least once, but
                  removes all fixed effects and adds covariates for number of vessels, total engine power,
                  log-price of diesel fuel, and nino3.4 index interacted by region.
                  Panel C) uses the same two-way fixed effects estimation as in A), but
                  includes all vessels in our sample, regardless of number of times subsidized."),
         escape = F)









  