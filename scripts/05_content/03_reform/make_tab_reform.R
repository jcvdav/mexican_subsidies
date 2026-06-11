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
  tidyverse,
  fixest,
  modelsummary
)

source(here("scripts", "00_Setup.R"))

# Standard errors clustered by economic unit by default
setFixest_vcov(all = "cluster", no_FE = "iid")

## Load data -------------------------------------------------------------------
shrimp_panel <- readRDS(here("data", "estimation_panels", "shrimp_estimation_panel.rds")) |> 
  mutate(post = 1 * (year >= 2020))

exit_panel <- read_rds(file = here("data/estimation_panels/exit_panel.rds"))

# Models
p_exit <- read_rds(here("data/output/prepost_reform_model_p_exit.rds"))
models <- c(here("data/output/prepost_reform_model_ext.rds"),
            here("data/output/prepost_reform_model_levels.rds")) |> 
  map(read_rds) |> 
  set_names("A) Extensive Margin",
            "B) Intensive Margin (levels)")

# PROCESSING ###################################################################

## Define modelsummary presets ------------------------------------------------
# Information to omit from the regression tables to make the more tidy
omit <- "(Intercept)|RMSE|With|IC|n_vessels|total_hp|nino|diesel"

# Change the appearance of what will appear in the regression table
gm <- tribble(~raw, ~clean, ~fmt,
              "nobs", "N", 0,
              "adj.r.squared", "$R^2$ Adj", 3
)

coefs <- c("post" = "Post")

# Get values for extra rows ----------------------------------------------------
# Mean of Y for pre period
mean_of_Y <- shrimp_panel |> 
  filter(always == 1) |> 
  mutate(var = "$\\bar{Y}_{\\text{Post} = 0}$") |> 
  group_by(var) |> 
  summarize(`Fishing time` = as.character(round(mean(hours, na.rm = T))),
            `Fishing area` = as.character(round(mean(fg_area_km, na.rm = T))),
            Landings = as.character(round(mean(live_weight, na.rm = T))))

# Assign rows where they should appear in modelsummary table
attr(mean_of_Y, "position") <- c(5)

# BUILD TABLES #################################################################
# P of exit
msummary(p_exit,
         estimate="{estimate} ({std.error}){stars}",
         statistic = NULL,
         stars = panelsummary:::econ_stars(),
         coef_omit = omit,
         coef_rename = coefs,
         gof_map = gm,
         output = here("content", "tables", "tab_reform_p_exit.tex"),
         title = "\\label{tab:prepost_reform_p_exit}Effect of Mexico's \\textit{impromptu}
         fuel subsidy reform on probability of economic units exiting the fishery.",
         notes = c("\\footnotesize $* p < 0.1, ** p < 0.05, *** p < 0.01$",
                   "\\footnotesize The unit of observation is an economic unit by year.
                   Numbers in parentheses are cluster-robust standard errors, clustered at the economic-unit level.
                  "),
         escape = F)

##  For change sin outcomes ----------------------------------------------------
msummary(models,
         shape = "rbind",
         estimate="{estimate} ({std.error}){stars}",
         statistic = NULL,
         stars = panelsummary:::econ_stars(),
         coef_omit = omit,
         coef_rename = coefs,
         gof_map = gm,
         add_rows = mean_of_Y,
         output = here("content", "tables", "tab_reform.tex"),
         title = "\\label{tab:prepost_reform}Effect of Mexico's \\textit{impromptu}
         fuel subsidy reform on fishing behavior and fisheries production for economic units that did not exit the fishery.",
         notes = c("\\footnotesize $* p < 0.1, ** p < 0.05, *** p < 0.01$",
                   "\\footnotesize The unit of observation is an economic unit by year.
                   Numbers in parentheses are cluster-robust standard errors, clustered at the economic-unit level.
                   Panel A) shows estimates for the extensive margin, where the outcome variables indicate whether a vessel spent time fishing, had fishing grounds, or reported landings.
                   Panel B) shows estimates for the intensive margin, where the outcome variables are time fishing (hours), fishing area ($\\text{km}^2$), and landings (kg)."),
         escape = F)

add_panel_spacing(here("content", "tables", "tab_reform.tex"))

