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

# Load packages ---------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse,
  fixest,
  panelsummary,
  modelsummary
)

source(here("scripts", "00_Setup.R"))

# Standard errors clustered by economic unit by default
setFixest_vcov(all = "cluster", no_FE = "iid")

## Load data -------------------------------------------------------------------
shrimp_panel <- readRDS(here("data", "estimation_panels", "shrimp_estimation_panel.rds")) |> 
  filter(year <= 2019)

# Models
elasticity_twfe <- readRDS(here("data", "output", "elasticity_twfe_model.rds"))

all_models <- readRDS(file = here("data", "output", "all_elasticity_models.rds")) |> 
  set_names(c("A) Main text specification",
              "B) Always subsidized",
              "C) Sometimes subsidized",
              "D) Removing modernized",
              "D) Covariates but no fixed effects"))

# Set up user defined functions ------------------------------------------------
# Function to extract number of observations in each model
n_eus <- function(model){
  as.character(length(unique(fixef(model, notes = F)$eu)))
}

# Function to convert coefficients to %
coef_to_pct <- function(model){
  scales::percent((((1 + 0.01)^coefficients(model)[1])-1), accuracy = 0.01, suffix = "\\%")
}

## Define modelsummary presets ------------------------------------------------
# Information to omit from the regression tables to make the more tidy
omit <- "(Intercept)|RMSE|With|IC|n_vessels|total_hp|nino|diesel"

# Change the appearance of what will appear in the regression table
gm <- tribble(~raw, ~clean, ~fmt,
              "nobs", "N", 0,
              "adj.r.squared", "$R^2$ Adj", 3
)

coefs <- c("log(subsidy_pesos)" = "log(subsidy amount[MXP])")

# PROCESSING ###################################################################

# Calculate percent changes to add to the table --------------------------------
extra <- bind_rows(map_dfc(elasticity_twfe, coef_to_pct),
                   map_dfc(elasticity_twfe, n_eus)) |> 
  mutate(var = c("\\%Change",
                 "$N_{eu}$")) |> 
  select(var, everything())

attr(extra, 'position') <- c(2, 3)

# VISUALIZE ####################################################################
# Build table ------------------------------------------------------------------
msummary(models = elasticity_twfe,
         estimate="{estimate} ({std.error}){stars}",
         statistic = NULL,
         stars = panelsummary:::econ_stars(),
         coef_omit = omit,
         coef_rename = coefs,
         gof_map = gm,
         add_rows = extra,
         output = here("content", "tables", "tab_elasticity.tex"),
         title = "\\label{tab:elasticity}Elasticity estimates for time fishing (hours),
         fishing area ($\\text{km}^2$), and landings (kg) with respect to changes in subsidy amount.",
         notes = c("\\footnotesize $* p < 0.1, ** p < 0.05, *** p < 0.01$",
                   "\\footnotesize The unit of observation is an economic unit by year.
                  All models include fixed effects by economic unit and by region-year.
                  Numbers in parentheses are cluster-robust standard errors, clustered at the economic-unit level.
                  The sample contains economic units subsidized at least twice and with subsidy amount $>$ 0.
                  The number of economic units used in each column is shown by $N_{eu}$."),
         escape = F)

## Now a table for all other models --------------------------------------------
msummary(models = all_models,
         shape = "rbind",
         estimate="{estimate} ({std.error}){stars}",
         statistic = NULL,
         stars = panelsummary:::econ_stars(),
         coef_omit = omit,
         coef_rename = coefs,
         gof_map = gm,
         output = here("content", "tables", "tab_elasticity_all_estimates.tex"),
         title = "\\label{tab:supp_elasticity}Elasticity estimates for time fishing (hours),
         fishing area ($\\text{km}^2$), and landings (kg) with respect to changes in subsidy amount.",
         notes = c("\\footnotesize $* p < 0.1, ** p < 0.05, *** p < 0.01$",
                   "\\footnotesize The unit of observation is an economic unit by year.
                   Numbers in parentheses are cluster-robust standard errors, clustered at the economic-unit level.
                  Panel A) shows the same information as in \\autoref{tab:elasticity}.
                  Panel B) restricts the sample to economic units always subsidized.
                  Panel C) restricts the sample to economic units sometimes subsidized.
                  Panel D) removes economic units that received fleet modernization subsidies.
                  Panel E) uses the same sample of vessels, but removes all fixed effects and adds
                  covariates for number of vessels, total engine power, and nino3.4 index interacted by region."),
         escape = F)

add_panel_spacing(here("content", "tables", "tab_elasticity_all_estimates.tex"))

