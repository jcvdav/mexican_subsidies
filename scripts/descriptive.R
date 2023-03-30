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
library(here)
library(fixest)
library(modelsummary)
library(tidyverse)

# Load data --------------------------------------------------------------------
shrimp_panel <- readRDS(here("data", "estimation_panels", "shrimp_estimation_panel.rds")) %>% 
  mutate(subsidy_frequency = fct_relevel(subsidy_frequency, "never", "sometimes", "always"))

## PROCESSING ##################################################################

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


did2 <- shrimp_panel %>% 
  filter(year <= 2014,
         eu %in% always_bef) %>% 
  select(eu, year, treated, hours, area, fuel_consumption_l, landed_weight, total_hp, n_vessels, nino34_m, ph) %>% 
  mutate(years_since = year - 2014,
         removed = eu %in% removed_2014) %>% 
  group_by(eu) %>% 
  mutate(norm_hours = (hours - mean(hours[year <= 2013])) / sd(hours[year <= 2013]),
         norm_area = ((area - mean(area[year <= 2013])) / sd(area[year <= 2013])),
         norm_landings = (landed_weight - mean(landed_weight[year <= 2013])) / sd(landed_weight[year <= 2013]),
         norm_fuel = (fuel_consumption_l - mean(fuel_consumption_l[year <= 2013])) / sd(fuel_consumption_l[year <= 2013])) %>% 
  ungroup() %>% 
  group_by(eu) %>% 
  add_count() %>% 
  filter(n == 4)

feols(hours ~ total_hp + n_vessels + i(years_since, removed, ref = -1) |
        eu + years_since,
      panel.id = ~eu + years_since,
      cluster = ~eu,
      data = did2) %>% 
  iplot(xlab = "Time to treatment",
        main = "Event study: Hours",
        pt.col = "steelblue",
        pt.cex = 2)


########### identification 2

omit <- "nino|hp|n_vess|(Intercept)|RMSE|With|Std.|FE"

models1 <- feols(c(log(hours), log(area), log(landed_weight), log(fuel_consumption_l)) ~ 
                   treated + log(ph) + total_hp + n_vessels + nino34_m | eu,
                 data = shrimp_panel,
                 panel.id = ~eu + year,
                 vcov = "NW",
                 # cluster = ~eu,
                 subset = ~sometimes == 1) %>% 
  set_names(c("log(hours)", "log(area)", "log(landings)", "log(fuel consumption)"))

extra1 <- tibble(V1 = "% Change", V2 = scales::percent((exp(coefficients(models1[[1]])[1])-1), 0.01))
attr(extra1, 'position') <- c(6, 3)

modelsummary(models = models1[1:2],
             stars = T,
             coef_omit = omit,
             gof_omit = omit,
             # add_rows = extra1,
             title = "Effect of receiving a subsidy on log(hours) of activity. Identification comes fom quasi-random inclusion / exclusion from the roster",
             coef_rename = c("log(ph)" = "log(fuel price)",
                             "treated" = "Subsidized"),
             notes = c("Control variables are total horsepower, number of vessels, and mean Nino3.4 anomaly.",
                       "All estimations include economic-unit-fixed effects.",
                       "Numbers in parentheses are heteroskedastic- and autocorrelation-robust standard errors.", 
                       "% Change is calculated as (exp(coefficient)-1) * 100"))

# Main takeaway: For a subset of X vessels that are sometimes subsidized,
# we see that they fish more when they receive a subsidy.

# effect is (exp(0.243)-1) * 100


# Identification 3
models2 <- feols(c(log(hours), log(area), log(landed_weight), log(fuel_consumption_l)) ~ 
        log(subsidy_pesos) + log(ph) + nino34_m + n_vessels + total_hp| eu,
      data = shrimp_panel %>% group_by(eu) %>% add_count() %>% filter(n >= 2) %>% ungroup(),
      panel.id = ~eu + year,
      vcov = "NW",
      subset = ~treated == 1) %>% 
  set_names(c("log(hours)", "log(area)", "log(landings)", "log(fuel consumption)"))

extra2 <- tibble(V1 = "% Change", V2 = scales::percent((exp(coefficients(models2[[1]])[1])-1), 0.01))
attr(extra1, 'position') <- c(6, 3)

modelsummary(models = models2[1],
             stars = T,
             coef_omit = omit,
             gof_omit = omit,
             add_rows = extra2,
             title = "Elasticity fishing activity with regards to subsidy amount (mexican pesos) and price of fuel (pesos per liter) for vessels that are subsidized at least once. Identification comes from exogenous variations in the adjustment factor or subsidized price",
             coef_rename = c("log(subsidy_pesos)" = "log(subsidy amount)",
                             "log(ph)" = "log(fuel price)"),
             notes = c("Control variables are total horsepower, number of vessels, and mean Nino3.4 anomaly.",
                       "All estimations include economic-unit-fixed effects.",
                       "Numbers in parentheses are heteroskedastic- and autocorrelation-robust standard errors.",
                       "% Change is calcualted as (((1 + 0.01) ^ coefficient)-1) * 100"))

# Effect is: (((1 - 0.5)^0.094)-1)
# A 1% increase in subsidies produces a (((1 + 0.01)^0.094)-1) * 100 = 0.093576 increase in hours


# Implications -----------
increase <- (exp(0.243)-1)
factor <- 1 - increase

total_outcomes <- shrimp_panel %>% 
  mutate(treated = ifelse(treated == 1, "Subsidized", "Not subsidized")) %>% 
  group_by(year, treated) %>% 
  summarize(hours = sum(hours) / 1e6)

alternative_outcomes <- shrimp_panel %>% 
  mutate(additional = treated * (hours - (factor * hours)),
         treated = ifelse(treated == 1, "Subsidized", "Not subsidized")) %>% 
  group_by(year, treated) %>% 
  summarize(hours = sum(hours) / 1e6,
            subsidy = sum(additional) / 1e6) %>% 
  mutate(baseline = hours - subsidy) %>% 
  select(year, treated, subsidy, baseline) %>% 
  pivot_longer(cols = c(subsidy, baseline),
               values_to = "hours",
               names_to = "source") %>% 
  mutate(treated = paste(treated, source, sep = "-")) %>% 
  filter(hours > 0)

ggplot(data = total_outcomes,
       mapping = aes(x = year, y = hours)) +
  stat_summary(geom = "area", fun = "sum") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(2011, 2019, by = 2),
                     limits = c(2011, 2019.5)) +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1)) +
  labs(x = "Year",
       y = "Total activity\n(Millions hours)")

ggplot(data = total_outcomes,
       mapping = aes(x = year, y = hours, fill = treated)) +
  stat_summary(geom = "area", fun = "sum", position = "stack") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(2011, 2019, by = 2),
                     limits = c(2011, 2019.5)) +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1)) +
  labs(x = "Year",
       y = "Total activity\n(Millions of hours)",
       fill = "Subsidy status") +
  scale_fill_brewer(palette = "Set1")

ggplot(data = alternative_outcomes,
       mapping = aes(x = year, y = hours, fill = treated)) +
  stat_summary(aes(x = year, y = hours), geom = "line", fun = "sum", position = "stack", inherit.aes = F) +
  stat_summary(geom = "area", fun = "sum", position = "stack") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(2011, 2019, by = 2),
                     limits = c(2011, 2019.5)) +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1)) +
  labs(x = "Year",
       y = "Total activity\n(Millions of hours)",
       fill = "Subsidy status") +
  scale_fill_brewer(palette = "Set1")



## SPatiall attribution

mex <- rnaturalearth::ne_countries(country = "Mexico", returnclas = "sf")

treated_in_2019 <- shrimp_panel %>% 
  filter(year == 2019,
         treated == 1) %>% 
  pull(eu)

shrimp_tracks <- readRDS(here("data", "2019_shrimp_tracks.rds"))

res <- 0.1

tracks_info <- shrimp_tracks %>% 
  mutate(lon = (floor(lon / res) * res) + (res / 2),
         lat = (floor(lat / res) * res) + (res / 2),
         treated = 1 * (eu_rnpa %in% treated_in_2019)) %>% 
  mutate(additional = treated * (hours - (factor * hours))) %>% 
  group_by(lat, lon) %>% 
  summarize(hours = sum(hours, na.rm = T),
            additional = sum(additional, na.rm = T),
            n_eus = n_distinct(eu_rnpa)) %>% 
  filter(lat < 35) %>% 
  mutate(difference = additional / hours)


ggplot() +
  geom_sf(data = mex) +
  geom_tile(data = tracks_info, aes(x = lon, y = lat, fill = log(hours))) +
  scale_fill_viridis_c() +
  guides(fill = guide_colorbar(title = "log(Total hours)",
                               frame.colour = "black",
                               ticks.colour = "black",
                               barwidth = 10)) +
  cowplot::theme_map() +
  theme(legend.position = "bottom")

ggplot() +
  geom_sf(data = mex) +
  geom_tile(data = tracks_info, aes(x = lon, y = lat, fill = log(additional))) +
  scale_fill_viridis_c() +
  guides(fill = guide_colorbar(title = "log(subsidized hours)",
                               frame.colour = "black",
                               ticks.colour = "black",
                               barwidth = 10)) +
  cowplot::theme_map() +
  theme(legend.position = "bottom")

ggplot() +
  geom_tile(data = tracks_info, aes(x = lon, y = lat, fill = difference)) +
  geom_sf(data = mex) +
  scale_fill_viridis_c(labels = scales::percent) +
  # scale_fill_gradientn(colors = colorRamps::matlab.like(10), label = scales::percent) +
  guides(fill = guide_colorbar(title = "% Atrributable\nto subsidy",
                               frame.colour = "black",
                               ticks.colour = "black",
                               barwidth = 10)) +
  cowplot::theme_map() +
  theme(legend.position = "bottom")






# Reduced form estimates -------------------------------------------------------
feols(c(log(fuel_consumption_l), log(hours), log(landed_weight), log(area)) ~ treated + nino34_m + total_hp,
      data = shrimp_panel, panel.id = ~eu + year,
      cluster = ~eu,
      subset = ~sometimes == 0) %>% 
  modelsummary(stars = T, coef_omit = omit, gof_omit = omit,
               title = "Reduced form estimates (vessels always and vessels never subsidized).")

feols(c(log(fuel_consumption_l), log(hours), log(landed_weight), log(area)) ~ treated + nino34_m + total_hp,
      data = shrimp_panel, panel.id = ~eu + year,
      cluster = ~eu,
      subset = ~always == 0) %>% 
  modelsummary(stars = T, coef_omit = omit, gof_omit = omit,
               title = "Reduced form estimates (vessels sometimes and vessels never subsidized).")


feols(c(log(fuel_consumption_l), log(hours), log(landed_weight), log(area)) ~ treated + nino34_m + total_hp,
      data = shrimp_panel, panel.id = ~eu + year,
      cluster = ~eu) %>% 
  modelsummary(stars = T, coef_omit = omit, gof_omit = omit,
               title = "Reduced form estimates (vessels sometimes and vessels never subsidized).")

# Main takeaways:
# 1) Suggestive evidence that vessels that receive a subsidy fish more than vessels that don't.
# 2) The difference is greater between always vs never than between sometimes vs never.
# But, vessels heterogeneity is correlated with treatment status, then these are biased.

# X ----------------------------------------------------------------------------
# But, we have more than just treatment status. We know how much each vessel
# received, so we can calculate the elasticity of fishing wrt subsidy. Let's 
# first put things into perspective by calculating the elasticity of fishing wrt
# price of fuel.

# We have annual prices so we have to give up year fixed effects, but we can
# bring in ENSO. From now on, anything that includes log(ph) excludes year fixed
# effects but includes ENSO.

# Never
never <- feols(c(log(fuel_consumption_l), log(hours), log(landed_weight), log(area)) ~ log(ph) + nino34_m + total_hp | eu,
               data = shrimp_panel, panel.id = ~eu + year,
               cluster = ~eu,
               subset = ~treated == 0) %>% 
  modelsummary(stars = T, coef_omit = omit, gof_omit = omit,
               title = "Elasticity wrt price for vessels that are never subsidized.",
               output = "data.frame")
# Sometimes 
sometimes <- feols(c(log(fuel_consumption_l), log(hours), log(landed_weight), log(area)) ~ log(ph) + nino34_m + total_hp | eu,
                   data = shrimp_panel, panel.id = ~eu + year,
                   cluster = ~eu,
                   subset = ~sometimes == 1) %>% 
  modelsummary(stars = T, coef_omit = omit, gof_omit = omit,
               title = "Elasticity wrt price for vessels that are sometimes subsidized.",
               output = "data.frame")
# Always
always <- feols(c(log(fuel_consumption_l), log(hours), log(landed_weight), log(area)) ~ log(ph) + nino34_m + total_hp | eu,
                data = shrimp_panel, panel.id = ~eu + year,
                cluster = ~eu,
                subset = ~always == 1) %>% 
  modelsummary(stars = T, coef_omit = omit, gof_omit = omit,
               title = "Elasticity wrt price for vessels that are always subsidized.",
               output = "data.frame")

bind_rows(never, sometimes, always, .id = "subset") %>% 
  filter(part == "estimates") %>% 
  mutate(subset = case_when(subset == 1 ~ "never",
                            subset == 2 ~ "sometimes",
                            subset == 3 ~ "always")) %>% 
  mutate(term = ifelse(statistic == "estimate", term, ""),
         subset = ifelse(statistic == "estimate", subset, "")) %>% 
  select(-c(part, statistic)) %>% 
  kableExtra::kable() %>% 
  kableExtra::kable_styling()

# Main takeaway: Price sensitivity (elasticity) decreases with increasing number
# of subsidies, which is in line with what we had seen before.

# Now we estimate elasticity of fishing WRT subsidy amount
# Price and subsidy
feols(c(log(fuel_consumption_l), log(hours), log(landed_weight), log(area)) ~ log(ph) + log(subsidy_pesos) + nino34_m + total_hp | eu,
      data = shrimp_panel, panel.id = ~eu + year,
      cluster = ~eu,
      subset = ~always == 1) %>% 
  modelsummary(stars = T, coef_omit = omit, gof_omit = omit,
               title = "Elasticity wrt price and subsidy for vessels that are always subsidized.")

feols(c(log(fuel_consumption_l), log(hours), log(landed_weight), log(area)) ~ log(ph) + log(subsidy_pesos) + nino34_m + total_hp | eu,
      data = shrimp_panel, panel.id = ~eu + year,
      cluster = ~eu,
      subset = ~sometimes == 1 & treated == 1) %>% 
  modelsummary(stars = T, coef_omit = omit, gof_omit = omit,
               title = "Elasticity wrt price and subsidy for vessels that are sometimes subsidized, conditional on being subsidized.")

# Main takeaways:
# 1) Positive elasticity of fishing WRT subsidy amount.
# 2) This value is greater for vessels that sometimes get a subsidy, conditional
# on being subsidized (and that are also more responsive to prices)


# How does expected subsidy affect extraction

expected <- shrimp_panel %>% 
  mutate(unit = 1) %>% 
  group_by(eu) %>% 
  arrange(year) %>% 
  mutate(exp = cumsum(treated) / cumsum(unit))

feols(c(log(fuel_consumption_l), log(hours), log(landed_weight)) ~
        log(ph) + i(treated, exp) + nino34_m + total_hp | eu,
      panel.id = ~eu + year,
      cluster = ~eu,
      data = expected) %>% 
  modelsummary(stars = T, coef_omit = omit, gof_omit = omit,
               title = "Insert title")


# How does entry affect fishing?

first_times_in <- shrimp_panel %>% 
  filter(sometimes == 1) %>% 
  group_by(eu) %>% 
  summarize(first_time_fishing = min(year),
            first_time_sub = min(c(year[treated == 1], 2050))) %>% 
  ungroup() %>% 
  filter(first_time_sub < 2050,
         first_time_fishing < first_time_sub) %>% 
  left_join(shrimp_panel, by = "eu", multiple = "all") %>% 
  filter(between(year, first_time_fishing, first_time_sub))

first_times_out <- shrimp_panel %>% 
  filter(sometimes == 1) %>% 
  group_by(eu) %>% 
  summarize(last_time_fishing = max(year),
            first_time_out = min(c(year[treated == 0], 2050))) %>% 
  ungroup() %>% 
  filter(first_time_out < last_time_fishing) %>% 
  left_join(shrimp_panel, by = "eu", multiple = "all") %>% 
  filter(between(year, first_time_out, last_time_fishing)) %>% 
  mutate(out = 1 * (treated == 0))


feols(c(log(fuel_consumption_l), log(hours), log(landed_weight)) ~
        log(ph) + treated + nino34_m + total_hp | eu,
      panel.id = ~eu + year,
      cluster = ~eu,
      data = first_times_in) %>% 
  modelsummary(stars = T, coef_omit = omit, gof_omit = omit,
               coef_rename = c("treated" = "entry"),
               title = "Effect of first entry into the subsidy raster on fishing fuel consumption, hours, and landings.")

feols(c(log(fuel_consumption_l), log(hours), log(landed_weight)) ~
        log(ph) + out + nino34_m + total_hp | eu,
      panel.id = ~eu + year,
      cluster = ~eu,
      data = first_times_out) %>% 
  modelsummary(stars = T, coef_omit = omit, gof_omit = omit,
               coef_rename = c("treated" = "entry"),
               title = "Effect of first entry into the subsidy raster on fishing fuel consumption, hours, and landings.")



# Example: the coefficient is 0.198. (exp(0.198) – 1) * 100 = 21.9.
# For every one-unit increase in the independent variable, our dependent variable increases by about 22%.

feols(log(hours) ~
        log(ph) + log(subsidy_pesos) + nino34_m + total_hp | eu,
      data = shrimp_panel, panel.id = ~eu + year,
      cluster = ~eu,
      subset = ~treated == 1) %>% 
  modelsummary(stars = T, coef_omit = omit, gof_omit = omit,
               title = "Reduced form estimates (vessels sometimes subsidized). Identification comes fom quasi-random inclusion / exclusion from `the roster`")



# How does frequency of subsidy affect behavior?


pred <- shrimp_panel %>%
  filter(treated == 1) %>%
  group_by(eu) %>%
  add_count() %>%
  ungroup() %>% 
  filter(n >= 2) %>% 
  mutate(nino34_m = mean(nino34_m),
         n_vessels = mean(n_vessels),
         total_hp = mean(total_hp)) %>% 
  group_by(eu) %>% 
  summarize(min = min(subsidy_pesos),
            max = max(subsidy_pesos),
            ph = mean(ph),
            nino34_m = mean(nino34_m),
            n_vessels = mean(n_vessels),
            total_hp = mean(total_hp)) %>% 
  ungroup() %>% 
  pivot_longer(cols = c(max, min), names_to = "range", values_to = "subsidy_pesos") %>% 
  mutate(new = predict(models2[[1]], newdata = .))

ggplot(data = shrimp_panel %>%
         filter(treated == 1) %>%
         group_by(eu) %>%
         add_count() %>%
         ungroup() %>% 
         filter(n >= 2) %>% 
         mutate(new = predict(models2[[1]], newdata = .)),
       aes(x = log(subsidy_pesos), y = log(hours))) +
  geom_smooth(method = "lm", se = F, color = "red", linetype = "dashed") +
  geom_smooth(aes(group = eu), method = "lm", se = F, linewidth = 0.1, color = "black") +
  geom_point(shape = ".", color = "steelblue") +
  geom_abline(intercept = 8.6, slope = 0.092, color = "red") +
  geom_line(aes(x = log(subsidy_pesos), y = new, group = eu), color = "steelblue")



