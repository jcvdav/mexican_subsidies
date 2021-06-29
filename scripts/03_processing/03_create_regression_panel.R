######################################################
#               create regression panel              #
######################################################
# 
# Combines the panel on subsidy allocations with the
# one on fuel consmption
#
######################################################

## SET UP ##########################################################################################################################################

# Load packages
library(here)
library(startR)
library(tidyverse)

## Read data
# Fuel consumptiom
fuel_consumption_raw <-
  # readRDS(here("data", "vms_daily_fuel_consumption.rds")) # This reads 2014 - 2016 VMS data
  readRDS(here("data", "all_vms_fuel_consumption.rds")) # This reads 2011 - 2021 VMS data

# Subsidy panel
subsidy_panel_raw <-
  tibble(readRDS(here("data", "economic_unit_subsidy_panel.rds")))

## PROCESSING ######################################################################################################################################
subsidy_panel <- subsidy_panel_raw %>% 
  select(year,
         eu_rnpa,
         contains("subsidy"),
         treated,
         applied)

fuel_consumption <- fuel_consumption_raw %>%
  filter(year < 2021) %>% 
  group_by(eu_rnpa, year) %>% 
  summarize(fuel_liters = sum(fuel_liters, na.rm = T),
            fuel_liters_max = sum(fuel_liters_max, na.rm = T)) %>% 
  ungroup()

panel <- fuel_consumption %>% 
  left_join(subsidy_panel, by = c("eu_rnpa", "year")) %>% 
  replace_na(replace = list(subsidy_liters = 0, subsidy_liters_pv = 0, treated = F, applied = F)) %>%
  mutate(p = 20,
         pp = p - 2,
         unsubsidized_liters = (fuel_liters - subsidy_liters) * (fuel_liters > subsidy_liters),
         tl = fuel_liters == (subsidy_liters + unsubsidized_liters),
         pa = ((subsidy_liters * pp) + (unsubsidized_liters * p)) / fuel_liters,
         marginal_p = case_when(treated & fuel_liters < subsidy_liters ~ pp,
                                treated & fuel_liters > subsidy_liters ~ p,
                                T ~ p),
         average_p = case_when(treated & fuel_liters < subsidy_liters ~ pp,
                               treated & fuel_liters > subsidy_liters ~ pa,
                               T ~ p))




panel %>% 
  # filter(treated) %>%
  mutate(a = fuel_liters > subsidy_liters,
         q = fuel_liters / subsidy_liters) %>% 
  ggplot(aes(x = q, y = average_p, color = a)) +
  geom_point()






c("fuel_liters ~ marginal_p + average_p",
  "fuel_liters ~ average_p",
  "fuel_liters ~ marginal_p") %>% 
  map(as.formula) %>% 
  map(lm, data = panel %>% filter(treated)) %>% 
  modelsummary()







panel %>% 
  mutate(ratio = fuel_liters / subsidy_liters,
         ratio_max = fuel_liters_max / subsidy_liters,
         dif = fuel_liters - subsidy_liters,
         dif_max = fuel_liters_max - subsidy_liters,
         p = ifelse(ratio <= 1, pp, p),
         side = ratio > 1) %>% 
  mutate(group = case_when(treated & side ~ "Cap > 0, Q > Cap",
                           treated & !side ~ "Cap > 0, Q < Cap",
                           !treated ~ "Unsubsidized")) %>% 
  count(year, group) %>% 
  ggplot(aes(x = year, y = n, fill = group)) +
  geom_col() +
  scale_fill_brewer(palette = "Set1", na.value = "gray") +
  labs(x = "Year", y = "Number of economic units")


treated_vessels <- panel %>% 
  filter(subsidy_pesos > 0)

ggplot(treated_vessels, aes(x = ratio)) +
  geom_histogram(bins = 100) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_x_continuous(limits = c(0, 5)) +
  labs(title = "Fuel used / fuel received")

ggplot(treated_vessels, aes(x = ratio_max)) +
  geom_histogram(bins = 100) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_x_continuous(limits = c(0, 5)) +
  labs(title = "Fuel used (high) / fuel received")

ggplot(treated_vessels, aes(x = dif)) +
  geom_histogram(bins = 100) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_x_continuous(limits = c(-1, 1) * 1e6) +
  labs(title = "Fuel used - fuel received")

ggplot(treated_vessels, aes(x = dif_max)) +
  geom_histogram(bins = 100) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_x_continuous(limits = c(-1, 1) * 1e6) +
  labs(title = "Fuel used (high) - fuel received")

ggplot(vessel_panel, aes(x = I, y =  fuel_liters, fill = treated)) +
  geom_point(alpha = 0.3, shape = 21, color = "black") +
  coord_equal() +
  geom_abline(slope =1 , intercept = 0, linetype = "dashed") +
  scale_fill_brewer(palette = "Set1", na.value = "gray") +
  labs(x = "I = (MLD x DPC x AF)",
       y = "Fuel consumption (L)")


p1 <- ggplot(panel, aes(x = subsidy_liters, y = fuel_liters, fill = treated)) +
  geom_point(alpha = 0.3, shape = 21, color = "black") +
  # coord_equal() +
  geom_abline(slope =1 , intercept = 0, linetype = "dashed") +
  scale_fill_brewer(palette = "Set1", na.value = "gray") +
  labs(x = "Subsidized liters (normalized on effort panel)",
       y = "Fuel consumption (L)")

cowplot::plot_grid(p1, p2, ncol = 1)

### Cool graph

fuel_prices <- tibble(year = c(2014, 2015, 2016),
                      p = c(13),
                      pp = p - 2)

fuel_prices_traced <- fuel_prices %>% 
  expand_grid(fuel_liters = seq(0, 100, by = 0.05)) %>% 
  mutate(P = ifelse(fuel_liters <= 0.4, pp, p)) %>% 
  group_by(year, p, pp) %>%
  mutate(P_mean = cumsum(P) / 1:length(P)) %>% 
  ungroup()
  

(p <- ggplot(data = panel, aes(x = fuel_liters / subsidy_liters, y = p)) +
    # geom_rug(alpha = 0.5, aes(color = treated)) +
    geom_hline(data = fuel_prices, aes(yintercept = p), linetype = "dashed", size = 1) +
    geom_step(data = fuel_prices_traced, aes(x = fuel_liters, y = P), color = "red", size = 1, direction = "vh") +
    geom_line(data = fuel_prices_traced, aes(x = fuel_liters, y = P_mean), linetype = "dotted", size = 1) +
    geom_jitter(aes(fill = treated), width = 0, height = 0.1, shape = 21, size = 1, alpha = 0.5) +
    labs(x = "Normalized liters [L used / (MDL x DPC)]",
         y = "Fuel price ($/L)") +
    geom_vline(xintercept = 1, linetype = "dashed") +
    # scale_y_continuous(labels = c(""), breaks = NULL, limits = c(10, 15)) +
    # scale_x_continuous() +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") +
    facet_wrap(~treated, ncol = 1))

lazy_ggsave(plot = p, filename = "trash_cool_graph")

c("fuel_liters ~ treated * side",
  "fuel_liters ~ treated * side + year") %>% 
  map(as.formula) %>% 
  map(feols, data = vessel_panel) %>% 
  modelsummary::modelsummary(statistic = "std.error", stars = T, gof_omit = "Pseudo|Within|Adj|IC|Log")

### EU-level



eu_full_panel <- all %>% 
  group_by(year, eu_rnpa) %>%
  summarize(fuel_liters = sum(fuel_liters, na.rm = T),
            I = sum(I, na.rm = T),
            n = n()) %>%
  ungroup() %>%
  inner_join(eu_panel, by = c("eu_rnpa" = "rnpa", "year" = "year")) %>% 
  replace_na(replace = list(subsidy_liters = 0)) %>%
  mutate(treated = subsidy_liters > 0,
         side = ifelse(fuel_liters > I, "right", "left")) %>% 
  mutate(group = case_when(treated & side == "right" ~ "Subsidized Rright",
                           treated & side == "left" ~ "Subsidized Left",
                           !treated & side == "right" ~ "Unsubsidized Right",
                           !treated & side == "left" ~ "Unsubsidized Left"))

eu_full_panel %>% 
  count(year, group) %>% 
  ggplot(aes(x = year, y = n, fill = group)) +
  geom_col(color = "black") +
  scale_fill_brewer(palette = "Set1", na.value = "gray") +
  labs(x = "Year", y = "Number of economic units")

p <- ggplot(eu_full_panel, aes(x = I, y = fuel_liters, fill = treated)) +
  geom_point(alpha = 0.3, shape = 21, color = "black") +
  geom_abline(slope =1 , intercept = 0, linetype = "dashed") +
  scale_fill_brewer(palette = "Set1", na.value = "gray") +
  labs(x = "I = (MLD x DPC x AF)",
       y = "Fuel consumption (L)")

lazy_ggsave(p, "cons_vs_theory")


ggplot(eu_full_panel, aes(x = subsidy_liters, y = fuel_liters, fill = treated)) +
  geom_point(alpha = 0.3, shape = 21, color = "black") +
  # coord_equal() +
  geom_abline(slope =1 , intercept = 0, linetype = "dashed") +
  scale_fill_brewer(palette = "Set1", na.value = "gray") +
  labs(x = "Subsidized liters",
       y = "Fuel consumption (L)")

p <- ggplot(eu_full_panel, aes(x = I, y = subsidy_liters, fill = treated)) +
  geom_point(alpha = 0.3, shape = 21, color = "black") +
  # coord_equal() +
  geom_abline(slope =1 , intercept = 0, linetype = "dashed") +
  scale_fill_brewer(palette = "Set1", na.value = "gray") +
  labs(y = "Subsidized liters",
       x = "I = (MLD x DPC x AF)",
       title = "Not to code")

lazy_ggsave(p, "not_to_law")

