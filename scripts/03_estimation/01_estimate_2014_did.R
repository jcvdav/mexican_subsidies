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
  fixest,
  modelsummary,
  tidyverse
)

# Load data --------------------------------------------------------------------
shrimp_panel <- readRDS(here("data", "estimation_panels", "shrimp_estimation_panel.rds")) %>% 
  mutate(subsidy_frequency = fct_relevel(subsidy_frequency, "never", "sometimes", "always"),
         removed = -1 * treated)

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
## DiD 
always_bef <- shrimp_panel %>% 
  filter(year <= 2013,
         always == 0,
         treated == 1) %>% 
  group_by(eu) %>% 
  add_count() %>% 
  ungroup() %>%
  filter(n == n_distinct(year)) %>% 
  pull(eu) %>% 
  unique()

removed_2014 <- shrimp_panel %>% 
  filter(eu %in% always_bef,
         year == 2014,
         treated == 0) %>% 
  pull(eu)

did_removed_vs_reduced <- shrimp_panel %>% 
  filter(year <= 2014,
         eu %in% always_bef) %>% 
  select(eu, year, treated, subsidy_pesos, hours, fishing_hours, fg_area_km, fuel_consumption_l, landed_weight, total_hp, n_vessels, nino34_m, ph) %>% 
  mutate(years_since = year - 2014,
         removed = 1 * (eu %in% removed_2014)) %>% 
  group_by(eu) %>% 
  add_count() %>% 
  filter(n == 4) %>% 
  ungroup()

did_removed_vs_reduced2 <- shrimp_panel %>% 
  filter(year <= 2017,
         eu %in% always_bef) %>% 
  select(eu, year, treated, subsidy_pesos, hours, fishing_hours, fg_area_km, fuel_consumption_l, landed_weight, total_hp, n_vessels, nino34_m, ph) %>% 
  mutate(years_since = year - 2014,
         removed = 1 * (eu %in% removed_2014)) %>% 
  group_by(eu) %>% 
  add_count() %>% 
  filter(n == 7) %>% 
  ungroup()

ggplot(did_removed_vs_reduced %>% filter(subsidy_pesos > 0),
       aes(x = year, y = log(subsidy_pesos), color = factor(removed))) +
  geom_line(aes(group = eu), linewidth = 0.1, alpha = 0.5) +
  stat_summary(geom = "line", fun = mean) +
  stat_summary(geom = "pointrange", fun.data = mean_se, fatten = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Year",
       y = expression("log(subsidy"~MXP[2019]~")"),
       color = "Subsidy removed in 2014") +
  theme(legend.position = c(0, 0),
        legend.justification = c(0, 0))

ggplot(did_removed_vs_reduced,
       aes(x = year, y = log(hours), color = factor(removed))) +
  geom_line(aes(group = eu), linewidth = 0.1, alpha = 0.5) +
  stat_summary(geom = "line", fun = mean) +
  stat_summary(geom = "pointrange", fun.data = mean_se) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Year",
       y = "log(hours)",
       color = "Subsidy removed in 2014") +
  theme(legend.position = c(0, 0),
        legend.justification = c(0, 0))

feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~ post * removed + total_hp + n_vessels, data = did_removed_vs_reduced %>% 
        filter(year %in% c(2013, 2014)) %>%
        mutate(post = 1 * (year == 2014)))

feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~ post * removed + total_hp + n_vessels, data = did_removed_vs_reduced %>% 
        filter(year %in% c(2013, 2014)) %>%
        mutate(post = 1 * (year == 2014)))


feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~  total_hp + n_vessels + i(years_since, removed, "-1") | eu + years_since,
      data = did_removed_vs_reduced2) %>% 
  iplot()


feols(log(hours) ~ total_hp + n_vessels + i(years_since, removed, ref = -1) |
        eu + year,
      panel.id = ~eu + years_since,
      cluster = ~eu,
      data = did_removed_vs_reduced) %>% 
  iplot(xlab = "Time to treatment",
        main = "Event study: log(Hours)",
        pt.col = "steelblue",
        pt.cex = 2)

feols(log(hours) ~ total_hp + n_vessels + i(years_since, removed, ref = -3) |
        eu + years_since,
      panel.id = ~eu + years_since,
      cluster = ~eu,
      data = did_removed_vs_reduced) %>% 
  iplot(xlab = "Time to treatment",
        main = "Event study: Hours",
        pt.col = "steelblue",
        pt.cex = 2)

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

## ESTIMATE ####################################################################

# X ----------------------------------------------------------------------------

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------