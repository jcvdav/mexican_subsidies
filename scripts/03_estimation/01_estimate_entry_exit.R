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

# Set some defaults ------------------------------------------------------------
# Model names to use
model_names <- c("Fishing time", "Fishing area", "Landings")

# Information to omit from the regression tables to make the more tidy
omit <- "hp|n_vess|(Intercept)|RMSE|With|IC"

# Change the appearance of what will appear in the regression table
gm <- tribble(~raw, ~clean, ~fmt,
              "nobs", "N", 0,
              "adj.r.squared", "R2 Adj", 3
              # "vcov.type", "SE", 0,
              # "FE: eu", "FE: Economic Unit", 0,
              # "FE: year^region", "FE: Year-region", 0
)


coefs <- c("log(ph)" = "log(fuel price)",
           "removed" = "Subsidy removed",
           "treated" = "Subsidized")

# QUICK FIGURE 
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

## ESTIMATION ##################################################################
# Main specification -----------------------------------------------------------
# TWFE and time-varying covariates
semi_elasticity_twfe <-
  feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~ 
          treated + total_hp + n_vessels |
          eu + year ^ region,
        data = shrimp_panel,
        panel.id = ~eu + year,
        vcov = "NW",
        subset = ~sometimes == 1) %>% 
  set_names(model_names)

# Calculate percent changes to add to the table --------------------------------
extra <- tibble(V1 = "\\% Change",
                V2 = scales::percent((exp(coefficients(semi_elasticity_twfe[[1]])[1])-1), accuracy = 0.01, suffix = "\\%"),
                V3 = scales::percent((exp(coefficients(semi_elasticity_twfe[[2]])[1])-1), accuracy = 0.01, suffix = "\\%"),
                V4 = scales::percent((exp(coefficients(semi_elasticity_twfe[[3]])[1])-1), accuracy = 0.01, suffix = "\\%"),)
attr(extra, 'position') <- c(3, 3)

# Build table ------------------------------------------------------------------
modelsummary(models = semi_elasticity_twfe,
             stars = panelsummary:::econ_stars(),
             coef_omit = omit,
             coef_rename = coefs,
             gof_map = gm,
             add_rows = extra,
             output = here("results", "tab", "table_semi_elasticity.tex"),
             title = "\\label{tab:semi_elasticity}Effect of receiving a subsidy on intensive and extensive behavioral margins, and fisheries production. Identification comes from quasi-random inclusions / exclusions from the roster.",
             notes = c("\\small The unit of observation is an economic unit by year. All models include control variables (total horsepower and number of vessels), fixed effects by economic unit and by region-year. Numbers in parentheses are panel-robust standard errors (Newey-West with a 1yr lag). Differences in sample size across columns are due to missing coordinates on some VMS messages or missing landings data."),
             escape = F)

# BUILD FIGURE #################################################################
# Repeat amin estiamtion but include all vessels
semi_elasticity_twfe_fs <-
  feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~ 
          treated + total_hp + n_vessels |
          eu + year ^ region,
        data = shrimp_panel,
        panel.id = ~eu + year,
        vcov = "NW") %>% 
  set_names(model_names)

# Drop year-by-region fixed-effects, add fuel price and NINO (quadratic), as well year (quadratic)
semi_elasticity_owfe <-
  feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~ 
          treated + log(mean_diesel_price_mxn_l) +
          total_hp + n_vessels +
          nino34_m + I(nino34_m^2) + year + I(year ^ 2) |
          eu,
        data = shrimp_panel,
        panel.id = ~eu + year,
        vcov = "NW",
        subset = ~sometimes == 1) %>% 
  set_names(model_names)

# Full sample, and drop year-by-region fixed-effects, add fuel price and NINO (quadratic), as well year (quadratic)
semi_elasticity_owfe_fs <-
  feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~ 
          treated + log(mean_diesel_price_mxn_l) +
          total_hp + n_vessels +
          nino34_m + I(nino34_m^2) + year + I(year ^ 2) |
          eu,
        data = shrimp_panel,
        panel.id = ~eu + year,
        vcov = "NW") %>% 
  set_names(model_names)

all_models <- c("TWFE Main" = semi_elasticity_twfe,
                "OWFE Main" = semi_elasticity_owfe,
                "TWFE Full" = semi_elasticity_twfe_fs,
                "OWFE Full" = semi_elasticity_owfe_fs)

p1 <- map_dfr(all_models,
              tidy,
              conf.int = T,
              .id = "model") %>% 
  filter(term == "treated") %>% 
  mutate(var = str_extract(model, "Fishing time|Fishing area|Landings"),
         var = fct_relevel(var, "Fishing time", "Fishing area", "Landings"),
         sample = str_extract(model, "Main|Full"),
         model = str_extract(model, "OWFE|TWFE"),
         group = paste(model, sample),
         group = fct_relevel(group, "TWFE Main", "TWFE Full", "OWFE Main", "OWFE Full")) %>% 
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
  scale_shape_manual(values = c(21, 22, 1, 0)) +
  scale_colour_brewer(palette = 'Set2') +
  scale_fill_brewer(palette = 'Set2') +
  guides(fill = "none",
         color = "none",
         shape = guide_legend(ncol = 2,
                              override.aes = list(fill = "black",
                                                  size = 1))) +
  labs(x = "",
       y = "Estimate and 95% Conf.Int.",
       shape = "Specification and sample") +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1))

ggsave(plot = p1,
       filename = here("results", "img", "fig_semi_elasticity.pdf"),
       width = 7,
       height = 3.5,
       units = "in")

# EXPORT #######################################################################
saveRDS(object = semi_elasticity_twfe,
        file = here("results", "models", "semi_elasticity_twfe.rds"))


# DELETE BELOW? --------- Tue Dec 19 13:19:24 2023 ------------------------------

restrict_n_times <- function(n_times = 9){
  feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~ 
          treated + total_hp + n_vessels |
          eu + year ^ region,
        data = shrimp_panel %>% 
          filter(n_times_sub <= n_times),
        panel.id = ~eu + year,
        vcov = "NW",
        subset = ~sometimes == 1) %>% 
    set_names(model_names) %>% 
    map_dfr(tidy, conf.int = T, .id = "var")
}

rob2 <- map_dfr(2:9,
                restrict_n_times,
                .id = "n_times") %>% 
  mutate(n_times = as.numeric(n_times)) %>% 
  filter(term == "treated")

p2 <- ggplot(data = rob2,
       aes(x = n_times, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(aes(ymin = estimate - std.error,
                      ymax = estimate + std.error,
                      color = var),
                  fatten = 6,
                  linewidth = 1.5) +
  scale_colour_brewer(palette = 'Set2') +
  guides(color = "none") +
  labs(x = "Subsidized at most # times",
       y = "Estimate and 95% CI") +
  facet_wrap(~var, ncol = 3)

ggsave(plot = p2,
       filename = here("results", "img", "fig_semi_elasticity_by_frequency.pdf"),
       width = 7,
       height = 3.5,
       units = "in")
