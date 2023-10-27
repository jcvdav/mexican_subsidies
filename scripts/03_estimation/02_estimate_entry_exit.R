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

omit <- "hp|n_vess|(Intercept)|RMSE|With|IC"

gm <- tribble(~raw, ~clean, ~fmt,
              "nobs", "N", 0,
              "adj.r.squared", "R2 Adj", 3,
              "vcov.type", "SE", 0,
              "FE: eu", "FE: Economic Unit", 0,
              "FE: year^region", "FE: Year-region", 0
)


shrimp_panel %>%
  select(eu, n_times_sub) %>%
  distinct() %>% 
  ggplot(mapping = aes(x = n_times_sub)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(labels = c(0:9), breaks = c(0:9)) +
  labs(x = "N times subsidized",
       y = "N economic units",
       title = "Historgram of frequency with which economic units are subsidized (2011-2019)",
       subtitle = "N = 0 implies never subsidized, N = 9 implies always subsidized.") 

models <- feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~ 
                        treated + total_hp + n_vessels |
                        eu + year ^ region,
                      data = shrimp_panel,
                      panel.id = ~eu + year,
                      vcov = "NW",
                      subset = ~sometimes == 1) %>% 
  set_names(c("Hours", "Area", "Landings"))

models_fs <- feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~ 
                  treated + total_hp + n_vessels |
                  eu + year ^ region,
                data = shrimp_panel,
                panel.id = ~eu + year,
                vcov = "NW") %>% 
  set_names(c("Hours", "Area", "Landings"))

models_owfe <- feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~ 
                       treated + log(ph) + total_hp + n_vessels + nino34_m |
                       eu,
                     data = shrimp_panel,
                     panel.id = ~eu + year,
                     vcov = "NW",
                     subset = ~sometimes == 1) %>% 
  set_names(c("Hours", "Area", "Landings"))

extra1 <- tibble(V1 = "% Change",
                 V2 = scales::percent((exp(coefficients(models[[1]])[1])-1), 0.01),
                 V3 = scales::percent((exp(coefficients(models[[2]])[1])-1), 0.01),
                 V4 = scales::percent((exp(coefficients(models[[3]])[1])-1), 0.01))
attr(extra1, 'position') <- c(3, 3)

modelsummary(models = models,
             stars = T,
             coef_omit = omit,
             gof_map = gm,
             add_rows = extra1,
             title = "Effect of receiving a subsidy on intensive and extensive behavioral margins, and fisheries production. Identification comes fom quasi-random inclusion / exclusion from the roster.",
             coef_rename = c("log(ph)" = "log(fuel price)",
                             "removed" = "Subsidy removed",
                             "treated" = "Enter"),
             notes = c("Control variables are total horsepower and number of vessels.",
                       "Numbers in parentheses are panel-robust standard errors.", 
                       "% Change is calculated as (exp(coefficient)-1) * 100",
                       "Difference is sample size is due to missing coordinates on some VMS messages
                       or missing landings data."))


map_dfr(c("TWFE" = models,
          "Controls" = models_owfe,
          "Full" = models_fs),
        tidy, conf.int = T,
        .id = "model") %>% 
  filter(term == "treated") %>% 
  mutate(var = str_extract(model, "Hours|Area|Landings"),
         var = fct_relevel(var, "Hours", "Area", "Landings"),
         model = str_extract(model, "Controls|TWFE|Full")) %>% 
  ggplot(aes(x = var, y = estimate, fill = var, color = var, shape = model)) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_pointrange(aes(ymin = conf.low,
                      ymax = conf.high),
                  position = position_dodge(width = 0.5),
                  fatten = 6) +
  scale_shape_manual(values = c(21, 22, 24)) +
  scale_colour_brewer(palette = 'Set2') +
  scale_fill_brewer(palette = 'Set2') +
  guides(fill = "none",
         color = "none",
         shape = guide_legend(override.aes = list(fill = "black"))) +
  labs(x = "",
       y = "Estimate and 95% Conf.Int.",
       shape = "Model",
       title = "Effect of receiving a subsidy on fishing hours, extent of fishing grounds, and landings",
       subtitle = "All years (2011-2019)",
       caption = "Treatment is receiving a subsidy. Main sample excludes economic units never (N = 35) and always (N = 169) subsidized.") +
  theme(legend.position = "bottom")

# Sensitivity test to show effect for at most 7 (re-do for "at post 6" and "at most 5" and so on...)
feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~ 
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


restrict_n_times <- function(n_times = 9){
  feols(log(hours) ~ 
          treated + total_hp + n_vessels |
          eu + year ^ region,
        data = shrimp_panel %>% 
          filter(n_times_sub < n_times),
        panel.id = ~eu + year,
        vcov = "NW",
        subset = ~sometimes == 1) %>% 
    tidy(conf.int = T)
}

map_dfr(2:9,
        restrict_n_times,
        .id = "n_times") %>% 
  mutate(n_times = as.numeric(n_times) + 1) %>% 
  filter(term == "treated") %>% 
  ggplot(aes(x = n_times, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_pointrange() +
  labs(x = "Subsidized at least # times",
       y = "Estimate and 95% CI")
