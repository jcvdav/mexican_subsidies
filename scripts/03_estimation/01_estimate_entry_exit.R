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

# Set table defaults
omit <- "hp|n_vess|(Intercept)|RMSE|With|IC"

gm <- tribble(~raw, ~clean, ~fmt,
              "nobs", "N", 0,
              "adj.r.squared", "R2 Adj", 3,
              "vcov.type", "SE", 0,
              "FE: eu", "FE: Economic Unit", 0,
              "FE: year^region", "FE: Year-region", 0
)

coefs <- c("log(ph)" = "log(fuel price)",
           "removed" = "Subsidy removed",
           "treated" = "Subsidized")

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

semi_elasticity_twfe <- feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~ 
                        treated + total_hp + n_vessels |
                        eu + year ^ region,
                      data = shrimp_panel,# %>% mutate(treated = (1 - treated)),
                      panel.id = ~eu + year,
                      vcov = "NW",
                      subset = ~sometimes == 1) %>% 
  set_names(c("Fishing time", "Fishing area", "Landings"))

semi_elasticity_twfe2 <- feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~ 
                  treated + total_hp + n_vessels |
                  eu + year ^ region,
                data = shrimp_panel %>% mutate(treated = (1 - treated)),
                panel.id = ~eu + year,
                vcov = "NW",
                subset = ~sometimes == 1) %>% 
  set_names(c("Fishing time", "Fishing area", "Landings"))

semi_elasticity_twfe_fs <- feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~ 
                  treated + total_hp + n_vessels |
                  eu + year ^ region,
                data = shrimp_panel,
                panel.id = ~eu + year,
                vcov = "NW") %>% 
  set_names(c("Fishing time", "Fishing area", "Landings"))

semi_elasticity_owfe <- feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~ 
                       treated + log(mean_diesel_price_mxn_l) + total_hp + n_vessels + nino34_m |
                       eu,
                     data = shrimp_panel,
                     panel.id = ~eu + year,
                     vcov = "NW",
                     subset = ~sometimes == 1) %>% 
  set_names(c("Fishing time", "Fishing area", "Landings"))

semi_elasticity_owfe_fs <- feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~ 
                          treated + log(mean_diesel_price_mxn_l) + total_hp + n_vessels + nino34_m |
                          eu,
                        data = shrimp_panel,
                        panel.id = ~eu + year,
                        vcov = "NW") %>% 
  set_names(c("Fishing time", "Fishing area", "Landings"))


# BUILD TABLES #################################################################
extra <- tibble(V1 = "% Change",
                V2 = scales::percent((exp(coefficients(semi_elasticity_twfe[[1]])[1])-1), 0.01),
                V3 = scales::percent((exp(coefficients(semi_elasticity_twfe[[2]])[1])-1), 0.01),
                V4 = scales::percent((exp(coefficients(semi_elasticity_twfe[[3]])[1])-1), 0.01))
attr(extra, 'position') <- c(3, 3)

modelsummary(models = semi_elasticity_twfe,
             stars = T,
             coef_omit = omit,
             gof_map = gm,
             add_rows = extra,
             output = here("results", "tab", "table_semi_elasticity.tex"),
             title = "\\label{tab:semi_elasticity}Effect of receiving a subsidy on intensive and extensive behavioral margins, and fisheries production. Identification comes fom quasi-random inclusions / exclusions from the roster.",
             coef_rename = coefs,
             notes = c("All models include control variables (total horsepower and number of vessels). Numbers in parentheses are standard errors. The \\\\% Change is calculated as (exp(coefficient)-1) x 100. Difference is sample size across columns is due to missing coordinates on some VMS messages or missing landings data."),
             threeparttable = T,
             escape = F)

# BUILD FIGURE #################################################################
p1 <- map_dfr(c("TWFE Main" = semi_elasticity_twfe,
                "OWFE Main" = semi_elasticity_owfe,
                "TWFE Full" = semi_elasticity_twfe_fs,
                "OWFE Full" = semi_elasticity_owfe_fs),
              tidy, conf.int = T,
              .id = "model") %>% 
  filter(term == "treated") %>% 
  mutate(var = str_extract(model, "Fishing time|Fishing area|Landings"),
         var = fct_relevel(var, "Fishing time", "Fishing area", "Landings"),
         sample = str_extract(model, "Main|Full"),
         # sample = fct_relevel(sample, "Main", "Full"),
         model = str_extract(model, "OWFE|TWFE"),
         # model = fct_relevel(model, "TWFE", "Controls"),
         group = paste(model, sample),
         group = fct_relevel(group, "TWFE Main", "TWFE Full", "OWFE Main", "OWFE Full")) %>% 
  ggplot(aes(x = var, y = estimate, fill = var, color = var, shape = group)) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_pointrange(aes(ymin = conf.low,
                      ymax = conf.high),
                  position = position_dodge(width = 0.5),
                  fatten = 6) +
  scale_shape_manual(values = c(21, 22, 1, 0)) +
  scale_colour_brewer(palette = 'Set2') +
  scale_fill_brewer(palette = 'Set2') +
  guides(fill = "none",
         color = "none",
         shape = guide_legend(ncol = 2,
                              override.aes = list(fill = "black"))) +
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
          filter(n_times_sub <= n_times),
        panel.id = ~eu + year,
        vcov = "NW",
        subset = ~sometimes == 1) %>% 
    tidy(conf.int = T)
}

map_dfr(2:8,
        restrict_n_times,
        .id = "n_times") %>% 
  mutate(n_times = as.numeric(n_times) + 1) %>% 
  filter(term == "treated") %>% 
  ggplot(aes(x = n_times, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_pointrange() +
  labs(x = "Subsidized at most # times",
       y = "Estimate and 95% CI")
