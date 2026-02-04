################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse,
  modelsummary
)

# Load data --------------------------------------------------------------------
panel <- readRDS(file = here("data", "estimation_panels", "shrimp_estimation_panel.rds")) |> 
  filter(year <= 2019)

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
datasummary(formula = (subsidy_pesos + hours + fg_area_km + landed_weight) * treated ~ 
              (Mean + SD + Min + Max),
            data = panel %>% mutate(treated = ifelse(treated == 1, "Subsidized", "Not subsidized")),
            format = "kableExtra",
            output = here("content", "tables", "tab_summary_stats.tex"))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------