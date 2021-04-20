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

# Read data
# Days per cycle, based on fishery
dpc_raw <-
  read_csv(file.path(project_path, "raw_data", "days_per_cycle.csv"))

# Maximum dayl liters for each engine size
mdl_raw <-
  read_csv(file.path(project_path, "raw_data", "maximum_daily_liters.csv"))

# Fuel consumptiom
fuel_consumption_raw <-
  readRDS(here("data", "vms_daily_fuel_consumption.rds"))

# Subsidy panel
subsidy_panel_raw <-
  tibble(readRDS(here("data", "economic_unit_subsidy_panel.rds")))

## PROCESSING ######################################################################################################################################
# Filter data (FOR NOW)
dpc <- dpc_raw %>% 
  filter(fuel_type == "diesel") %>%                           # Keep info on days per cycle for diesel only
  select(-c(year, fishery, fuel_type)) %>% 
  mutate(location = ifelse(location == "pacific", location, "atlantic")) %>% 
  select(-location) %>% 
  distinct()

mdl <- mdl_raw %>% 
  filter(fuel_type == "diesel") %>%                           # Keep maximum daily liters for diesel vessels only
  select(engine_power_bin_hp = engine_power_hp,
         maximum_daily_liters) %>% #, year)
  distinct()

# effort <- effort_raw 

subsidy_panel <- subsidy_panel_raw %>% 
  select(year,
         eu_rnpa,
         contains("subsidy")) %>% 
  mutate(treated = subsidy_pesos > 0)

fuel_consumption <- fuel_consumption_raw %>%
  group_by(vessel_rnpa, eu_rnpa, ocean, year, engine_power_hp, engine_power_bin_hp, species) %>% 
  summarize(fuel_liters = sum(fuel_liters, na.rm = T)) #%>% 
  # left_join(mdl, by = "engine_power_bin_hp") %>%
  # left_join(dpc, by = c("species", "ocean" = "location")) %>%
  # mutate(I = maximum_daily_liters * days_per_cycle * 0.4)

vessel_panel <- fuel_consumption %>% 
  inner_join(subsidy_panel, by = c("eu_rnpa", "year")) %>% 
  replace_na(replace = list(subsidy_liters = 0, subsidy_liters_pv = 0)) %>%
  mutate(p = 20,
         pp = p - 2,
         # ratio_t = fuel_liters / I,
         ratio_o = fuel_liters / subsidy_liters_pv,
         # p_t = ifelse(ratio_t <= 0.4, pp, p),
         p_o = ifelse(ratio_o <= 0.4, pp, p), 
         # side_t = ifelse(ratio_t > 0.4, "right", "left"),
         side_o = ifelse(ratio_o > 0.4, "right", "left")) %>% 
  mutate(
    # group_t = case_when(treated & side_t == "right" ~ "Subsidized Rright",
    #                          treated & side_t == "left" ~ "Subsidized Left",
    #                          !treated & side_t == "right" ~ "Unsubsidized Right",
    #                          !treated & side_t == "left" ~ "Unsubsidized Left"),
         group_o = case_when(treated & side_o == "right" ~ "Subsidized Rright",
                             treated & side_o == "left" ~ "Subsidized Left",
                             !treated & side_o == "right" ~ "Unsubsidized Right",
                             !treated & side_o == "left" ~ "Unsubsidized Left"))


# FROM HERE ################################################################
vessel_panel <- all %>% 
  inner_join(eu_panel, by = c("eu_rnpa" = "rnpa", "year" = "year"))  %>% 
  left_join(fuel_prices, by = "year") %>%
  mutate(ratio = fuel_liters / I,
         P = ifelse(ratio <= 0.4, pp, p),
         year = as.factor(year)) %>% 
  group_by(eu_rnpa) %>% 
  mutate(n_vessels = n_distinct(vessel_rnpa)) %>% 
  ungroup() %>% 
  replace_na(replace = list(subsidy_liters = 0,
                            subsidy_liters_pv = 0)) %>%
  mutate(treated = subsidy_liters > 0,
         side = ifelse(ratio > 0.4, "right", "left")) %>% 
  mutate(group = case_when(treated & side == "right" ~ "Subsidized Rright",
                           treated & side == "left" ~ "Subsidized Left",
                           !treated & side == "right" ~ "Unsubsidized Right",
                           !treated & side == "left" ~ "Unsubsidized Left"))

vessel_panel %>% 
  count(year, group_o) %>% 
  ggplot(aes(x = year, y = n, fill = group_o)) +
  geom_col() +
  scale_fill_brewer(palette = "Set1", na.value = "gray") +
  labs(x = "Year", y = "Number of vessels")




ggplot(vessel_panel, aes(x = I, y =  fuel_liters, fill = treated)) +
  geom_point(alpha = 0.3, shape = 21, color = "black") +
  coord_equal() +
  geom_abline(slope =1 , intercept = 0, linetype = "dashed") +
  scale_fill_brewer(palette = "Set1", na.value = "gray") +
  labs(x = "I = (MLD x DPC x AF)",
       y = "Fuel consumption (L)")


p1 <- ggplot(vessel_panel, aes(x = subsidy_liters / n_vessels, y = fuel_liters, fill = treated)) +
  geom_point(alpha = 0.3, shape = 21, color = "black") +
  # coord_equal() +
  geom_abline(slope =1 , intercept = 0, linetype = "dashed") +
  scale_fill_brewer(palette = "Set1", na.value = "gray") +
  labs(x = "Subsidized liters (normalized on effort panel)",
       y = "Fuel consumption (L)")


p2 <- ggplot(vessel_panel, aes(x = subsidy_liters_pv, y = fuel_liters, fill = treated)) +
  geom_point(alpha = 0.3, shape = 21, color = "black") +
  # coord_equal() +
  geom_abline(slope =1 , intercept = 0, linetype = "dashed") +
  scale_fill_brewer(palette = "Set1", na.value = "gray") +
  labs(x = "Subsidized liters (normalized on subsidy panel)",
       y = "Fuel consumption (L)")

cowplot::plot_grid(p1, p2, ncol = 1)

### Cool graph

fuel_prices <- tibble(year = c(2014, 2015, 2016),
                      p = c(13),
                      pp = p - 2)

fuel_prices_traced <- fuel_prices %>% 
  expand_grid(fuel_liters = seq(0, 2, by = 0.05)) %>% 
  mutate(P = ifelse(fuel_liters <= 0.4, pp, p)) %>% 
  group_by(year, p, pp) %>%
  mutate(P_mean = cumsum(P) / 1:length(P)) %>% 
  ungroup()
  

(p <- ggplot(data = vessel_panel, aes(x = fuel_liters / I, y = P)) +
    # geom_rug(alpha = 0.5, aes(color = treated)) +
    geom_hline(data = fuel_prices, aes(yintercept = p), linetype = "dashed", size = 1) +
    geom_step(data = fuel_prices_traced, aes(x = fuel_liters, y = P), color = "red", size = 1, direction = "vh") +
    geom_line(data = fuel_prices_traced, aes(x = fuel_liters, y = P_mean), linetype = "dotted", size = 1) +
    geom_jitter(aes(fill = treated), width = 0, height = 0.1, shape = 21, size = 1, alpha = 0.5) +
    # geom_jitter(aes(color = treated), width = 0, height = 0.1, pch = "l", size = 1, alpha = 0.8) +
    labs(x = "Normalized liters [L used / (MDL x DPC)]",
         y = "Fuel price ($/L)") +
    geom_vline(xintercept = 0.4, linetype = "dashed") +
    # geom_vline(xintercept = 1, linetype = "dashed") +
    scale_y_continuous(labels = c(""), breaks = NULL, limits = c(10, 15)) +
    scale_x_continuous(labels = c("I"), breaks = c(0.4)) +
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

# old stuff below


# TO HERE ################################################################

lazy_ggsave(p3, "trash3")


# TAKE ALL GFW AND ADD DPC AND MDL





p2 <- effort %>% 
  left_join(mdl, by = c("binned_engine_power_hp" = "engine_power_hp")) %>% 
  left_join(dpc, by = c("gfw_species" = "species")) %>% 
  mutate(I = maximum_daily_liters * days_per_cycle * 0.4) %>% 
  mutate(side = ifelse(active_liters > I, "right", "left")) %>% 
  count(year, side) %>% 
  ggplot(aes(x = year, y = n, fill = side)) +
  geom_col(color = "black") +
  scale_fill_brewer(palette = "Set1", na.value = "gray") +
  labs(x = "Year", y = "Number of vessels")


lazy_ggsave(p2, filename = "trash2")



############# ENGINE MATCHING THING
expand_grid(potencia = 1:4e3,
            binned_engine_power_hp = sort(unique(mdl$engine_power_hp))) %>% 
  drop_na() %>% 
  filter(binned_engine_power_hp <= potencia) %>% 
  mutate(dif = abs(potencia - binned_engine_power_hp)) %>% 
  group_by(potencia) %>% 
  top_n(n = -1, dif) %>% 
  select(-dif) %>% 
  ungroup() %>% 
  left_join(mdl, by = c("binned_engine_power_hp" = "engine_power_hp")) %>% 
  ggplot(aes(x = potencia, y = binned_engine_power_hp, color = maximum_daily_liters)) +
  geom_step(size = 1) +
  geom_segment(x = 2000, xend = 2000,
               y = 0, yend = 1350,
               color = "black",
               linetype = "dashed") +
  geom_segment(x = -150, xend = 2000,
               y = 1350, yend = 1350,
               color = "black",
               linetype = "dashed") +
  guides(color = guide_colorbar(title = "Maximum daily liters",
                               frame.colour = "black",
                               ticks.colour = "black")) +
  scale_color_viridis_c() +
  theme(legend.justification = c(1, 0),
        legend.position = c(1, 0)) +
  labs(x = "Vessel's engine size (HP)", y = "Discrete engine bin (HP)") +
  coord_equal()


