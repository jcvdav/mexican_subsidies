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
  broom,
  tidyverse
)

# Load data --------------------------------------------------------------------
shrimp_panel_raw <- readRDS(here("data", "estimation_panels", "shrimp_estimation_panel.rds"))

shrimp_panel <- shrimp_panel_raw %>% 
  filter(treated == 1,
         n_times_sub >= 2) %>% 
  mutate(y2014 = year == 2014)

## PROCESSING ##################################################################

omit <- "hp|n_vess|(Intercept)|RMSE|With|IC"

gm <- tribble(~raw, ~clean, ~fmt,
              "nobs", "N", 0,
              "adj.r.squared", "R2 Adj", 3,
              "vcov.type", "SE", 0,
              "FE: eu", "FE: Economic Unit", 0,
              "FE: year^region", "FE: Year-region", 0
)

coefs <- c("log(ph)" = "log(fuel price)",
           "log(subsidy_pesos)" = "log(subsidy amount[MXP])")


# Identification 3
elasticity_twfe <- feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~ 
                           log(subsidy_pesos) + n_vessels + total_hp|
                           eu + year^region,
                         data = shrimp_panel,
                         panel.id = ~eu + year,
                         vcov = "NW") %>% 
  set_names(c("Fishing time", "Fishing area", "Landings"))

elasticity_twfe_split <- feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~ 
                                 log(subsidy_pesos) + n_vessels + total_hp|
                                 eu + year^region,
                               data = shrimp_panel,
                               panel.id = ~eu + year,
                               vcov = "NW",
                               split = ~subsidy_frequency) %>% 
  set_names(c("S Fishing time", "S Fishing area", "S Landings", "A Fishing time", "A Fishing area", "A Landings"))

elasticity_owfe <- feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~ 
                      log(subsidy_pesos) + log(mean_diesel_price_mxn_l) + nino34_m + n_vessels + total_hp + y2014 | eu,
                    data = shrimp_panel,
                    panel.id = ~eu + year,
                    vcov = "NW") %>% 
  set_names(c("Fishing time", "Fishing area", "Landings"))


elasticity_owfe_split <- feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~ 
                                 log(subsidy_pesos) + log(mean_diesel_price_mxn_l) + nino34_m + n_vessels + total_hp + y2014| eu,
                               data = shrimp_panel,
                               panel.id = ~eu + year,
                               vcov = "NW",
                               split = ~subsidy_frequency) %>% 
  set_names(c("S Fishing time", "S Fishing area", "S Landings", "A Fishing time", "A Fishing area", "A Landings"))

## BUILD TABLE #################################################################
extra <- tibble(V1 = "% Change",
                V2 = scales::percent((((1 + 0.01)^coefficients(elasticity_twfe[[1]])[1])-1), 0.01),
                V3 = scales::percent((((1 + 0.01)^coefficients(elasticity_twfe[[2]])[1])-1), 0.01),
                V4 = scales::percent((((1 + 0.01)^coefficients(elasticity_twfe[[3]])[1])-1), 0.01))
attr(extra, 'position') <- c(3, 3)

modelsummary(models = elasticity_twfe,
             stars = T,
             coef_omit = omit,
             gof_map = gm,
             add_rows = extra,
             output = here("results", "tab", "table_elasticity.tex"),
             title = "label{tab:elasticity}Effect of increasing subsidy amounts on intensive and extensive behavioral margins, and fisheries production. Identification comes from exogenous variations in the amount of subsidy allocated toe ach economic unit.",
             coef_rename = coefs,
             notes = "All models include control variables (total horsepower, number of vessels). Numbers in parentheses are standard errors. \\\\% Change is calcualted as (((1 + 0.01) \\\\^ coefficient)-1) * 100. Difference is sample size across columns is due to missing coordinates on some VMS messages or missing landings data",
             threeparttable = T,
             escape = F)

## BUILD FIGURE ################################################################
p1 <- map_dfr(c("TWFE Main" = elasticity_twfe,
                "OWFE Main" = elasticity_owfe,
                "TWFE Split" = elasticity_twfe_split,
                "OWFE Split" = elasticity_owfe_split),
              tidy,
              conf.int = T,
              .id = "model") %>% 
  filter(term == "log(subsidy_pesos)") %>% 
  mutate(var = str_extract(model, "Fishing time|Fishing area|Landings"),
         var = fct_relevel(var, "Fishing time", "Fishing area", "Landings"),
         split = str_extract(model, "Split\\..{1}"),
         split = ifelse(is.na(split), "Main", str_remove(split, "Split\\.")),
         model = str_extract(model, "OWFE|TWFE"),
         group = paste(model, split),
         group = fct_relevel(group, "TWFE Main", "TWFE S", "TWFE A", "OWFE Main", "OWFE S", "OWFE A")) %>% 
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
         shape = guide_legend(ncol = 3,
                              override.aes = list(fill = "black"))) +
  labs(x = "",
       y = "Estimate and 95% Conf.Int.",
       shape = "Specification and sample")+
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1))


ggsave(plot = p1,
       filename = here("results", "img", "fig_elasticity.pdf"),
       width = 7,
       height = 3.5,
       units = "in")

## EXPORT ######################################################################
saveRDS(object = elasticity_twfe,
        file = here("results", "models", "elasticity_twfe.rds"))
