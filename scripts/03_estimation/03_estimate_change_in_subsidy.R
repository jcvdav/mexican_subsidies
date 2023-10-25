################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
# How many times does a vessel enter / exit the roster?
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  fixest,
  modelsummary,
  tidyverse
)

# Load data --------------------------------------------------------------------
shrimp_panel <- readRDS(here("data", "estimation_panels", "shrimp_estimation_panel.rds"))


## PROCESSING ##################################################################

########### identification 2

omit <- "nino|hp|n_vess|(Intercept)|RMSE|With|Std.|FE|IC"


# Identification 3
models2 <- feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~ 
                   log(subsidy_pesos) + log(ph) + nino34_m + n_vessels + total_hp | eu,
                 data = shrimp_panel %>% group_by(eu) %>% add_count() %>% filter(n >= 2) %>% ungroup(),
                 panel.id = ~eu + year,
                 vcov = "NW",
                 subset = ~treated == 1)

extra2 <- tibble(V1 = "% Change",
                 V2 = scales::percent((((1 + 0.01)^coefficients(models2[[1]])[1])-1), 0.01),
                 V3 = scales::percent((((1 + 0.01)^coefficients(models2[[2]])[1])-1), 0.01),
                 V4 = scales::percent((((1 + 0.01)^coefficients(models2[[3]])[1])-1), 0.01))
attr(extra1, 'position') <- c(6, 3)

modelsummary(models = models2[1:3],
             stars = T,
             coef_omit = omit,
             gof_omit = omit,
             add_rows = extra2,
             title = "Elasticity of fishing activity with regards to subsidy amount (mexican pesos) and price of fuel (pesos per liter) for vessels that are subsidized at least once. Identification comes from exogenous variations in the adjustment factor or subsidized price",
             coef_rename = c("log(subsidy_pesos)" = "log(subsidy amount)",
                             "log(ph)" = "log(fuel price)"),
             notes = c("Control variables are total horsepower, number of vessels, and mean Nino3.4 anomaly.",
                       "All estimations include economic-unit-fixed effects.",
                       "Numbers in parentheses are panel-robust standard errors.",
                       "% Change is calcualted as (((1 + 0.01) ^ coefficient)-1) * 100",
                       "Difference is sample size is due to missing coordinates on some VMS messages."))



# What happens if we restrict to those always subsidized
feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~ 
        log(subsidy_pesos) + log(ph) + nino34_m + n_vessels + total_hp | eu,
      data = shrimp_panel %>% group_by(eu) %>% add_count() %>% filter(n >= 2) %>% ungroup(),
      panel.id = ~eu + year,
      vcov = "NW",
      split = ~subsidy_frequency,
      subset = ~treated == 1) %>% 
  modelsummary(stars = T,
               coef_omit = omit,
               gof_omit = omit)
