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

# Define some defaults ---------------------------------------------------------
# Model names to use
model_names <- c("Fishing time", "Fishing area", "Landings")
split_model_names <- c("S Fishing time", "S Fishing area", "S Landings", "A Fishing time", "A Fishing area", "A Landings")

# Information to omit from the regression tables to make the more tidy
omit <- "(Intercept)|RMSE|With|IC"

# Change the appearance of what will appear in the regression table
gm <- tribble(~raw, ~clean, ~fmt,
              "nobs", "N", 0,
              "adj.r.squared", "R2 Adj", 3
)

# Rename coefficients
coefs <- c("log(ph)" = "log(fuel price)",
           "log(subsidy_pesos)" = "log(subsidy amount[MXP])",
           "n_vessels" = "\\# Vessels",
           "norm_hp" = "Norm. power (hp / vessel)")

# Load data --------------------------------------------------------------------
shrimp_panel_raw <- readRDS(here("data", "estimation_panels", "shrimp_estimation_panel.rds"))

## PROCESSING ##################################################################
shrimp_panel <- shrimp_panel_raw %>% 
  filter(treated == 1,
         n_times_sub >= 2) %>% 
  mutate(y2014 = year == 2014)

## ESTIMATION ##################################################################
# Main specification -----------------------------------------------------------
# TWFE and time-varying covariates
elasticity_twfe <- feols(fml = c(log(hours), log(fg_area_km), log(landed_weight)) ~ 
                           log(subsidy_pesos) + n_vessels + norm_hp |
                           eu + year^region,
                         data = shrimp_panel,
                         panel.id = ~eu + year,
                         vcov = "NW") %>% 
  set_names(model_names)


# Calculate percent changes to add to the table --------------------------------
extra <- tibble(V1 = "\\% Change",
                V2 = scales::percent((((1 + 0.01)^coefficients(elasticity_twfe[[1]])[1])-1), accuracy = 0.01, suffix = "\\%"),
                V3 = scales::percent((((1 + 0.01)^coefficients(elasticity_twfe[[2]])[1])-1), accuracy = 0.01, suffix = "\\%"),
                V4 = scales::percent((((1 + 0.01)^coefficients(elasticity_twfe[[3]])[1])-1), accuracy = 0.01, suffix = "\\%")) %>% 
  set_names(c("V1", model_names))
attr(extra, 'position') <- c(7, 7)

# Build table ------------------------------------------------------------------
modelsummary(models = elasticity_twfe,
             stars = panelsummary:::econ_stars(),
             coef_omit = omit,
             gof_map = gm,
             add_rows = extra,
             output = here("results", "tab", "table_elasticity.tex"),
             title = "\\label{tab:elasticity}Effect of increasing subsidy amounts on intensive and extensive behavioral margins, and fisheries production. Identification comes from exogenous variations in the amount of subsidy allocated toe ach economic unit.",
             coef_rename = coefs,
             notes = "\\tiny The unit of observation is an economic unit by year. All models include fixed effects by economic unit and by region-year. Numbers in parentheses are panel-robust standard errors (Newey-West with a 1yr lag). Differences in sample size across columns are due to missing coordinates on some VMS messages or missing landings data.",
             escape = F)

## ROBUSTNESS TESTS ############################################################
feols(fml = c(log(hours), log(fg_area_km), log(landed_weight)) ~ 
        log(subsidy_pesos) + total_hp |
        eu + year^region,
      data = shrimp_panel |> group_by(eu) |> mutate(n = max(n_vessels)) |> ungroup() |> filter(n == 1),
      panel.id = ~eu + year,
      vcov = "NW")


## ALTERNATIVE SPECIFICATIONS ##################################################
# Two-way fixed-effects estimation, splitting sample by "always" and "sometimes" subsidized
elasticity_twfe_split <- feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~ 
                                 log(subsidy_pesos) + n_vessels + total_hp |
                                 eu + year^region,
                               data = shrimp_panel,
                               panel.id = ~eu + year,
                               vcov = "NW",
                               split = ~subsidy_frequency) %>% 
  set_names(split_model_names)

# Drop year-by-region fixed-effects, add fuel price and NINO (quadratic), as well year (quadratic)
elasticity_owfe <- feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~ 
                           log(subsidy_pesos) + log(mean_diesel_price_mxn_l) +
                           total_hp + n_vessels +
                           nino34_m + I(nino34_m^2) + year + I(year ^ 2) 
                         | eu,
                         data = shrimp_panel,
                         panel.id = ~eu + year,
                         vcov = "NW") %>% 
  set_names(model_names)

# Drop year-fixed effects, add fuel price and NINO, as well as a quadratic term 
# or year, and split the sample by "always" and "sometimes" subsidized
elasticity_owfe_split <- feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~ 
                                 log(subsidy_pesos) + log(mean_diesel_price_mxn_l) +
                                 total_hp + n_vessels +
                                 nino34_m + I(nino34_m^2) + year + I(year ^ 2) 
                               | eu,
                               data = shrimp_panel,
                               panel.id = ~eu + year,
                               vcov = "NW",
                               split = ~subsidy_frequency) %>% 
  set_names(split_model_names)

## BUILD FIGURE ################################################################
all_models <- c("TWFE Main" = elasticity_twfe,
                "OWFE Main" = elasticity_owfe,
                "TWFE Split" = elasticity_twfe_split,
                "OWFE Split" = elasticity_owfe_split)

p1 <- map_dfr(all_models,
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
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high),
                 color = "black",
                 position = position_dodge(width = 0.5),
                 linewidth = 0.1)+
  geom_pointrange(aes(ymin = estimate - std.error,
                      ymax = estimate + std.error),
                  position = position_dodge(width = 0.5),
                  fatten = 6,
                  linewidth = 1.5) +
  scale_shape_manual(values = c(21, 1, 10, 22, 0, 7)) +
  scale_colour_brewer(palette = 'Set2') +
  scale_fill_brewer(palette = 'Set2') +
  guides(fill = "none",
         color = "none",
         shape = guide_legend(ncol = 3,
                              override.aes = list(fill = "black",
                                                  size = 1))) +
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
