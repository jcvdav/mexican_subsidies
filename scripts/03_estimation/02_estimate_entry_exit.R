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


ggplot(data = shrimp_panel %>% select(eu, n_times_sub) %>% distinct(),
       mapping = aes(x = n_times_sub)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(labels = c(0:9), breaks = c(0:9)) +
  labs(x = "N times subsidized",
       y = "N economic units",
       title = "Historgram of frequency with which economic units are subsidized (2011-2019)",
       subtitle = "N = 0 implies never subsidized, N = 9 implies always subsidized.") 

models1 <- feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~ 
                   treated + log(ph) + total_hp + n_vessels + nino34_m |
                   eu,
                 data = shrimp_panel,
                 panel.id = ~eu + year,
                 vcov = "NW",
                 # cluster = ~eu,
                 subset = ~sometimes == 1)

extra1 <- tibble(V1 = "% Change",
                 V2 = scales::percent((exp(coefficients(models1[[1]])[1])-1), 0.01),
                 V3 = scales::percent((exp(coefficients(models1[[2]])[1])-1), 0.01),
                 V4 = scales::percent((exp(coefficients(models1[[3]])[1])-1), 0.01))
attr(extra1, 'position') <- c(6, 3)

modelsummary(models = models1,
             stars = T,
             coef_omit = omit,
             gof_omit = omit,
             add_rows = extra1,
             title = "Effect of receiving a subsidy on intensive(log(hours)) and extensive (log(area)) margins. Identification comes fom quasi-random inclusion / exclusion from the roster",
             coef_rename = c("log(ph)" = "log(fuel price)",
                             "removed" = "Subsidy removed",
                             "treated" = "Enter"),
             notes = c("Control variables are total horsepower, number of vessels, and mean Nino3.4 index.",
                       "All estimations include economic-unit-fixed effects.",
                       "Numbers in parentheses are panel-robust standard errors.", 
                       "% Change is calculated as (exp(coefficient)-1) * 100",
                       "Difference is sample size is due to missing coordinates on some VMS messages."))


feols(c(log(hours), log(area_km), log(landed_weight)) ~ 
        treated + log(ph) + total_hp + n_vessels + nino34_m |
        eu,
      data = shrimp_panel,
      panel.id = ~eu + year,
      vcov = "NW",
      # cluster = ~eu,
      subset = ~sometimes == 1 & n_times_sub < 8) %>% 
  # set_names(c("log(hours)", "log(area)", "log(landed_weight)")) %>% 
  modelsummary(stars = T,
               coef_omit = omit,
               gof_omit = omit,
               title = "Sample restricted to vessels subsidized at most 7 times. Effect of receiving a subsidy on intensive(log(hours)) and extensive (log(area)) margins. Identification comes fom quasi-random inclusion / exclusion from the roster",
               coef_rename = c("log(ph)" = "log(fuel price)",
                               "treated" = "Subsidized"),
               notes = c("Control variables are total horsepower, number of vessels, and mean Nino3.4 index.",
                         "All estimations include economic-unit-fixed effects.",
                         "Numbers in parentheses are panel-robust standard errors.", 
                         "% Change is calculated as (exp(coefficient)-1) * 100",
                         "Difference is sample size is due to missing coordinates on some VMS messages."))