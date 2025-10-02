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
shrimp_panel <- readRDS(here("data", "estimation_panels", "shrimp_estimation_panel.rds")) |> 
  filter(year <= 2019)

model_names <- c("A) Main text specification",
                 "B) Covariates but no fixed effects",
                 "C) Main text specification with all units",
                 "D) Main text specification without modernized units")

## Load models -----------------------------------------------------------------
ext_twfe <- readRDS(here("data", "output", "ext_model.rds"))
all_ext_models <- readRDS(file = here("data", "output", "all_ext_models.rds")) |> 
  set_names(model_names)

levels_twfe <- readRDS(here("data", "output", "levels_model.rds"))
all_level_models <- readRDS(file = here("data", "output", "all_level_models.rds")) |> 
  set_names(set_names(model_names))

semi_elasticity_twfe <- readRDS(here("data", "output", "semi_elasticity_twfe_model.rds"))
all_semi_elasticity_models <- readRDS(file = here("data", "output", "all_semi_elasticity_models.rds")) |> 
  set_names(set_names(model_names))


# Set up user defined functions ------------------------------------------------
# Function to extract number of observations in each model
n_eus <- function(model){
  as.character(length(unique(names(fixef(model, notes = F)$eu))))
}

# Function to convert coefficients to %
coef_to_pct <- function(model){
  scales::percent((exp(coefficients(model)[1])-1), accuracy = 0.01, suffix = "\\%")
}

# PROCESSING ###################################################################

## Define modelsummary presets ------------------------------------------------
# Information to omit from the regression tables to make the more tidy
omit <- "(Intercept)|RMSE|With|IC|n_vessels|total_hp|nino|diesel"

# Change the appearance of what will appear in the regression table
gm <- tribble(~raw, ~clean, ~fmt,
              "nobs", "N", 0,
              "adj.r.squared", "$R^2$ Adj", 3
)

coefs <- c("treated" = "Subsidized")


## Calculate extra rows --------------------------------------------------------
# Number of economic units
N_eus_ext <- map_dfc(ext_twfe, n_eus) |> 
  mutate(var = "$N_{eu}$") |> 
  select(var, everything())

N_eus_int <- map_dfc(levels_twfe, n_eus) |> 
  mutate(var = "$N_{eu}$") |> 
  select(var, everything())

N_eus_semi <- map_dfc(semi_elasticity_twfe, n_eus) |> 
  mutate(var = "$N_{eu}$") |> 
  select(var, everything())

# Mean of Y for untreated units
mean_of_Y <- shrimp_panel |> 
  filter(treated == 0,
         sometimes == 1) |> 
  mutate(var = "$\\bar{Y}_{\\text{Subsidized} = 0}$") |> 
  group_by(var) |> 
  summarize(`Fishing time` = as.character(round(mean(hours, na.rm = T))),
            `Fishing area` = as.character(round(mean(fg_area_km, na.rm = T))),
            Landings = as.character(round(mean(live_weight, na.rm = T))))

# Convert log-log to %change
pct_change <- map_dfc(semi_elasticity_twfe, coef_to_pct) |> 
  mutate(var = "\\%Change") |> 
  select(var, everything())

# Put together
extra <- bind_rows(N_eus_ext, mean_of_Y, N_eus_int, N_eus_semi)

# Assign rows where they should appear in modelsummary table
attr(extra, "position") <- c(2, 6, 7, 11)

# VISUALIZE ####################################################################

# 1) Main-text table  ----------------------------------------------------------

msummary(list("A) Extensive margin" = ext_twfe,
              "B) Intensive margin (levels)" = levels_twfe,
              "C) Semi-elasticities" = semi_elasticity_twfe),
         shape = "rbind",
         estimate="{estimate} ({std.error}){stars}",
         statistic = NULL,
         stars = panelsummary:::econ_stars(),
         coef_omit = omit,
         coef_rename = coefs,
         gof_map = gm,
         add_rows = extra,
         output = here("content", "tables", "tab_main_entry_exit.tex"),
         title = "\\label{tab:main_entry_exit}Effect of receiving a fuel subsidy on fishing behavior and fisheries production.",
         notes = c("\\footnotesize $* p < 0.1, ** p < 0.05, *** p < 0.01$",
                   "\\footnotesize The unit of observation is an economic unit by year.
                  Numbers in parentheses are panel-robust standard errors (Newey-West with a 1yr lag).
                  Panel A) shows estimates for the extensive margin, where the outcome variables indicate whether a vessel spent time fishing, had fishing grounds, or reported landings.
                  Panel B) shows estimates for the intensive margin, where the outcome variables are time fishing (hours), fishing area ($\\text{km}^2$), and landings (kg).
                  Panel C) shows semi-elasticity estimates for log-transformed time fishing (hours), fishing area ($\\text{km}^2$), and landings (kg).
                  This last panel excludes vessels whose fishing activity or landings were exactly zero, mostly capturing the intensive margin."),
         escape = F)

# 2) Supplementary tables ------------------------------------------------------
## Now a table for all levels models
msummary(models = all_ext_models,
         shape = "rbind",
         estimate="{estimate} ({std.error}){stars}",
         statistic = NULL,
         stars = panelsummary:::econ_stars(),
         coef_omit = omit,
         coef_rename = coefs,
         gof_map = gm,
         output = here("content", "tables", "tab_ext_all_estimates.tex"),
         title = "\\label{tab:supp_ext}Effect of receiving a fuel subsidy on time fishing (hours) \\textgreater 0,
                   fishing area ($\\text{km}^2$) \\textgreater 0, and landings (kg) \\textgreater 0.",
         notes = c("\\footnotesize $* p < 0.1, ** p < 0.05, *** p < 0.01$",
                   "\\footnotesize The unit of observation is an economic unit by year.
                  Numbers in parentheses are panel-robust standard errors (Newey-West with a 1yr lag).
                  Panel A) shows the same information as in \\autoref{tab:main_entry_exit}A.
                  Panel B) uses the same sample of vessels subsidized at least once, but
                  removes all fixed effects and adds covariates for number of vessels, total engine power,
                  log-price of diesel fuel, and nino3.4 index interacted by region.
                  Panel C) uses the same two-way fixed effects estimation as in A), but
                  includes all vessels in our sample, regardless of number of times subsidized.
                  Panel D) uses the same two-way fixed effects estimation as in A), but
                  removes vessels that received a fleet modernization subsidy."),
         escape = F)

## Now a table for all levels models
msummary(models = all_level_models,
         shape = "rbind",
         estimate="{estimate} ({std.error}){stars}",
         statistic = NULL,
         stars = panelsummary:::econ_stars(),
         coef_omit = omit,
         coef_rename = coefs,
         gof_map = gm,
         output = here("content", "tables", "tab_levels_all_estimates.tex"),
         title = "\\label{tab:supp_levels}Effect of receiving a fuel subsidy on time fishing (hours), fishing area ($\\text{km}^2$), and landings (kg).",
         notes = c("\\footnotesize $* p < 0.1, ** p < 0.05, *** p < 0.01$",
                   "\\footnotesize The unit of observation is an economic unit by year.
                  Numbers in parentheses are panel-robust standard errors (Newey-West with a 1yr lag).
                  Panel A) shows the same information as in \\autoref{tab:main_entry_exit}B.
                  Panel B) uses the same sample of vessels subsidized at least once, but
                  removes all fixed effects and adds covariates for number of vessels, total engine power,
                  log-price of diesel fuel, and nino3.4 index interacted by region.
                  Panel C) uses the same two-way fixed effects estimation as in A), but
                  includes all vessels in our sample, regardless of number of times subsidized.
                  Panel D) uses the same two-way fixed effects estimation as in A), but
                  removes vessels that received a fleet modernization subsidy."),
         escape = F)

## Now a table for all semi-elasticity models
msummary(models = all_semi_elasticity_models,
         shape = "rbind",
         estimate="{estimate} ({std.error}){stars}",
         statistic = NULL,
         stars = panelsummary:::econ_stars(),
         coef_omit = omit,
         coef_rename = coefs,
         gof_map = gm,
         output = here("content", "tables", "tab_semi_elasticity_all_estimates.tex"),
         title = "\\label{tab:supp_semi_elasticity}Effect of receiving a fuel subsidy on time fishing (hours), fishing area ($\\text{km}^2$), and landings (kg).",
         notes = c("\\footnotesize $* p < 0.1, ** p < 0.05, *** p < 0.01$",
                   "\\footnotesize The unit of observation is an economic unit by year.
                  Numbers in parentheses are panel-robust standard errors (Newey-West with a 1yr lag).
                  Panel A) shows the same information as in \\autoref{tab:main_entry_exit}C.
                  Panel B) uses the same sample of vessels subsidized at least once, but
                  removes all fixed effects and adds covariates for number of vessels, total engine power,
                  log-price of diesel fuel, and nino3.4 index interacted by region.
                  Panel C) uses the same two-way fixed effects estimation as in A), but
                  includes all vessels in our sample, regardless of number of times subsidized.
                  Panel D) uses the same two-way fixed effects estimation as in A), but
                  removes vessels that received a fleet modernization subsidy."),
         escape = F)

