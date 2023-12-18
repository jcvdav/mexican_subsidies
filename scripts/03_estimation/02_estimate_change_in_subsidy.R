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
elasticity_twfe <- feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~ 
                           log(subsidy_pesos) + n_vessels + total_hp |
                           eu + year^region,
                         data = shrimp_panel %>% group_by(eu) %>% add_count() %>% filter(n >= 2) %>% ungroup(),
                         panel.id = ~eu + year,
                         vcov = "NW",
                         subset = ~treated == 1) %>% 
  set_names(c("Hours", "Area", "Landings"))

elasticity_twfe_split <- feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~ 
                                 log(subsidy_pesos) + n_vessels + total_hp |
                                 eu + year^region,
                               data = shrimp_panel %>% group_by(eu) %>% add_count() %>% filter(n >= 2) %>% ungroup(),
                               panel.id = ~eu + year,
                               vcov = "NW",
                               split = ~subsidy_frequency,
                               subset = ~treated == 1) %>% 
  set_names(c("S Hours", "S Area", "S Landings", "A Hours", "A Area", "A Landings"))

elasticity_owfe <- feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~ 
                      log(subsidy_pesos) + log(mean_diesel_price_mxn_l) + nino34_m + n_vessels + total_hp | eu,
                    data = shrimp_panel %>% group_by(eu) %>% add_count() %>% filter(n >= 2) %>% ungroup(),
                    panel.id = ~eu + year,
                    vcov = "NW",
                    subset = ~treated == 1) %>% 
  set_names(c("Hours", "Area", "Landings"))

elasticity_owfe_split <- feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~ 
                                 log(subsidy_pesos) + log(mean_diesel_price_mxn_l) + nino34_m + n_vessels + total_hp | eu,
                               data = shrimp_panel %>% group_by(eu) %>% add_count() %>% filter(n >= 2) %>% ungroup(),
                               panel.id = ~eu + year,
                               vcov = "NW",
                               split = ~subsidy_frequency,
                               subset = ~treated == 1) %>% 
  set_names(c("S Hours", "S Area", "S Landings", "A Hours", "A Area", "A Landings"))

extra2 <- tibble(V1 = "% Change",
                 V2 = scales::percent((((1 + 0.01)^coefficients(elasticity_twfe[[1]])[1])-1), 0.01),
                 V3 = scales::percent((((1 + 0.01)^coefficients(elasticity_twfe[[2]])[1])-1), 0.01),
                 V4 = scales::percent((((1 + 0.01)^coefficients(elasticity_twfe[[3]])[1])-1), 0.01))
attr(extra2, 'position') <- c(3, 3)

modelsummary(models = elasticity_twfe,
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

map_dfr(c("TWFE Main" = elasticity_twfe,
          "Controls Main" = elasticity_owfe,
          "TWFE Split" = elasticity_twfe_split,
          "Controls Split" = elasticity_owfe_split),
        tidy, conf.int = T,
        .id = "model") %>% 
  filter(term == "log(subsidy_pesos)") %>% 
  mutate(var = str_extract(model, "Hours|Area|Landings|log(hours)"),
         var = fct_relevel(var, "Hours", "Area", "Landings"),
         split = str_extract(model, "Split\\..{1}"),
         split = ifelse(is.na(split), "Main", str_remove(split, "Split\\.")),
         model = str_extract(model, "Controls|TWFE"),
         group = paste(model, split),
         group = fct_relevel(group, "TWFE Main", "TWFE S", "TWFE A", "Controls Main", "Controls S", "Controls A")) %>% 
  ggplot(aes(x = var, y = estimate, fill = var, color = var, shape = group)) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_pointrange(aes(ymin = conf.low,
                      ymax = conf.high),
                  position = position_dodge(width = 0.5),
                  fatten = 6) +
  scale_shape_manual(values = c(21, 1, 10, 22, 0, 7)) +
  scale_colour_brewer(palette = 'Set2') +
  scale_fill_brewer(palette = 'Set2') +
  guides(fill = "none",
         color = "none",
         shape = guide_legend(override.aes = list(fill = "black"))) +
  labs(x = "",
       y = "Estimate and 95% Conf.Int.",
       shape = "Model",
       title = "Elasticity of fishing hours, extent of fishing grounds, and landings w.r.t. subsidy amount.",
       subtitle = "All years (2011-2019)",
       caption = "Treatment is log(subsidy amount) in 2019 MXN. Sample restricted only to subsidized vessels.") +
  theme(legend.position = "bottom")



# What happens if we restrict to those always subsidized
feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~ 
        log(subsidy_pesos) + n_vessels + total_hp |
        eu + year^region,
      data = shrimp_panel %>% group_by(eu) %>% add_count() %>% filter(n >= 2) %>% ungroup(),
      panel.id = ~eu + year,
      vcov = "NW",
      fsplit = ~subsidy_frequency,
      subset = ~treated == 1) %>% 
  map_df(tidy, conf.int = T, .id = "model") %>% 
  filter(term == "log(subsidy_pesos)") %>% 
  mutate(var = str_extract(model, "hours|area|landed_weight"),
         model = str_extract(model, "sometimes|always|Full sample")) %>% 
  ggplot(aes(x = var, y = estimate, fill = var, color = var, shape = model)) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_pointrange(aes(ymin = conf.low,
                      ymax = conf.high),
                  position = position_dodge(width = 0.5),
                  fatten = 6) +
  # scale_shape_manual(values = c(21, 22, 24)) +
  scale_colour_brewer(palette = 'Set2') +
  scale_fill_brewer(palette = 'Set2') +
  guides(fill = "none",
         color = "none",
         shape = guide_legend(override.aes = list(fill = "black"))) +
  labs(x = "",
       y = "Estimate and 95% Conf.Int.",
       shape = "Model",
       title = "Elasticity of fishing hours, extent of fishing grounds, and landings w.r.t. subsidy amount.",
       subtitle = "All years (2011-2019)",
       caption = "Treatment is log(subsidy amount) in 2019 MXN. Sample restricted only to subsidized vessels.") +
  theme(legend.position = "bottom")
