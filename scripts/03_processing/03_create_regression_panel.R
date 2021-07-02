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
  readRDS(here("data", "vms_monthly_fuel_consumption.rds"))

# Subsidy panel
subsidy_panel_raw <-
  tibble(readRDS(here("data", "economic_unit_subsidy_panel.rds")))

diesel_prices <- read.csv(file.path(project_path, "raw_data", "monthly_diesel_prices.csv")) %>% 
  select(month = Month, price = Price) %>% 
  mutate(date = lubridate::mdy(month),
         month = lubridate::month(date),
         year = lubridate::year(date)) %>% 
  filter(month == 06) %>% 
  select(year, ph = price)

## PROCESSING ######################################################################################################################################
subsidy_panel <- subsidy_panel_raw %>% 
  # filter(fuel_type == "Diesel") %>%
  select(year,
         eu_rnpa,
         contains("subsidy"),
         treated,
         applied,
         zone)

fuel_consumption <- fuel_consumption_raw %>%
  group_by(eu_rnpa, year) %>% 
  summarize(fuel_consumption_l = sum(fuel_consumption_l, na.rm = T),
            fuel_consumption_max_l = sum(fuel_consumption_max_l, na.rm = T),
            h = sum(h, na.rm = T)) %>% 
  ungroup()


panel <- fuel_consumption %>% 
  left_join(subsidy_panel, by = c("eu_rnpa", "year")) %>% 
  left_join(diesel_prices, by = "year") %>% 
  # replace_na(replace = list(subsidy_liters = 0, subsidy_liters_pv = 0, treated = F, applied = F)) %>%
  mutate(pl = ph - 2,
         phi = subsidy_cap_l / fuel_consumption_l,
         D = phi <= 1L,
         R = ph - pl,
         term1 = pl + (D * R),
         term2 = (phi * R  * D),
         o_term1 = (pl * (1 - D)) + (ph * D),
         o_term2 = D * phi * (pl - ph)) %>% 
  add_count(eu_rnpa) %>% 
  filter(n > 2,
         treated,
         !eu_rnpa == "2508008238")


model1 <- feols(fuel_consumption_l ~ term1 + term2 | eu_rnpa, data = panel)
model2 <- feols(fuel_consumption_l ~ term1 + term2 | eu_rnpa + zone, data = panel)
model3 <- feols(fuel_consumption_l ~ term1 + term2 | eu_rnpa + zone + year, data = panel)

rows <- tibble(a = "alpha", b = get_alpha(model1), c = get_alpha(model2),  d = get_alpha(model3))

models <- list(model1, model2, model3)

modelsummary(models, stars = T,
             statistic_vertical = F,
             add_rows = rows,
             gof_omit = "Adj|With|IC|Log")

ggplot(panel, aes(x = term1, y = fuel_consumption_l)) + 
  geom_path(aes(group = eu_rnpa))

ggplot(panel, aes(x = term2)) + 
  geom_histogram()


###
fuel_consumption %>% 
  left_join(subsidy_panel, by = c("eu_rnpa", "year")) %>% 
  left_join(diesel_prices, by = "year")
