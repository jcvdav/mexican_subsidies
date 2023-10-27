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
shrimp_panel <- readRDS(here("data", "estimation_panels", "shrimp_estimation_panel.rds")) #%>% 
  # mutate(removed = -1 * treated)

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
## DiD 
always_bef <- shrimp_panel %>% 
  filter(year <= 2013,
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
  select(eu, year, region,treated, subsidy_pesos, hours, fishing_hours, fg_area_km, fuel_consumption_l, landed_weight, total_hp, n_vessels, nino34_m, ph) %>% 
  mutate(years_since = year - 2014,
         post = 1 * (year == 2014),
         removed = 1 * (eu %in% removed_2014)) %>% 
  group_by(eu) %>% 
  add_count() %>% 
  filter(n == 4) %>% # This ends up removing a single vessel that drops out in 2014 (3 obs correspodning to 2011-2013)
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
       aes(x = year, y = log(hours),
           color = factor(removed))) +
  # geom_line(aes(group = eu), linewidth = 0.1, alpha = 0.5) +
  stat_summary(geom = "line", fun = mean) +
  stat_summary(geom = "pointrange", fun.data = mean_se) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Year",
       y = "log(hours)",
       color = "Subsidy removed in 2014") +
  theme(legend.position = c(0, 0),
        legend.justification = c(0, 0))

## ESTIMATION ##################################################################
# Two-way fixed effects
event_study <- feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~
                       i(year, removed, 2013) + total_hp + n_vessels |
                       eu + year^region,
                     data = did_removed_vs_reduced,
                     panel.id = ~eu + year,
                     vcov = NW(lag = 1)) %>% 
  set_names(c("Hours", "Area", "Landings"))

# Including time-varying constants instead of year-region
event_study_owfe <- feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~
                       i(year, removed, 2013) + log(ph) + nino34_m + total_hp + n_vessels |
                       eu,
                     data = did_removed_vs_reduced,
                     panel.id = ~eu + year,
                     vcov = NW(lag = 1)) %>% 
  set_names(c("Hours", "Area", "Landings"))

attr(event_study, "tree") <- tibble(id = 1:3,
                                    lhs = factor(names(event_study),
                                                 levels = names(event_study)))

did <- feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~
               post:removed + total_hp + n_vessels + log(ph) + nino34_m | 
               eu,
             data = did_removed_vs_reduced,
             panel.id = ~eu + year,
             vcov = "NW")

did <- feols(c(log(hours), log(fg_area_km), log(landed_weight)) ~
               post:removed + total_hp + n_vessels + log(ph) + nino34_m | 
               eu + year ^ region,
             data = did_removed_vs_reduced,
             panel.id = ~eu + year,
             vcov = "NW")
## VISUALIZE ###################################################################

gof_stats <- map_dfr(event_study, glance, .id = "model") %>% 
  mutate(x = 2011.5,
         y = 0.5,
         adj.r.squared = round(adj.r.squared, 3))

# X ----------------------------------------------------------------------------
ggiplot(event_study,
        multi_style = "facet",
        pt.pch = 21,
        ref.line.par = list(col = "black"),
        theme = theme(legend.position = "None",
                      panel.border = element_rect(fill = "transparent")),
        main = "Effect of subsidy removals on fishing hours, extent of fishing grounds, and landings") +
  scale_x_continuous(breaks = c(2011:2014)) +
  scale_colour_brewer(palette = 'Set2') +
  scale_fill_brewer(palette = 'Set2') +
  labs(fill = "Outcome of interest",
       color = "Outcome of interest",
       shape = "Outcome of interest",
       x = "Year",
       subtitle = "Treatment is subsidy removal. Treated economic units (N = 67) had their subsidies removed in 2014.
       Control economic units (N = 194) had their subsidy reduced, but not removed.")

modelsummary(event_study,
             stars = panelsummary:::econ_stars())

## ESTIMATE ####################################################################

# X ----------------------------------------------------------------------------

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------