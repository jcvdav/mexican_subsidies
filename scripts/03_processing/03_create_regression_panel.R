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
  read.csv(file.path(project_path, "data", "processed_data", "vms_annual_fuel_consumption.csv"),
           stringsAsFactors = F)

# Subsidy panel
subsidy_panel_raw <-
  read.csv(file.path(project_path, "data", "processed_data", "economic_unit_subsidy_panel.csv"),
           stringsAsFactors = F)

fuel_prices <- read.csv(file.path(project_path, "data", "raw_data", "monthly_diesel_prices.csv")) %>% 
  select(month = Month, price = Price) %>% 
  mutate(date = lubridate::mdy(month),
         month = lubridate::month(date),
         year = lubridate::year(date),
         fuel_type = "Diesel") %>% 
  group_by(year) %>% 
  mutate(mean_price = mean(price, na.rm = T)) %>% 
  ungroup() %>% 
  rename(july_price = price) %>% 
  filter(month == 06) %>% 
  select(-c(month, date))

## PROCESSING ######################################################################################################################################
# Fuel consumption
fuel_consumption <- fuel_consumption_raw %>%
  group_by(year, eu_rnpa, fleet) %>% 
  summarize(fuel_consumption_l = sum(fuel_consumption_l, na.rm = T),
            fuel_consumption_max_l = sum(fuel_consumption_max_l, na.rm = T),
            hours = sum(h, na.rm = T),
            n_vessels = n_distinct(vessel_rnpa)) %>% 
  ungroup()

# Fuel panel
subsidy_panel <- subsidy_panel_raw %>% 
  # filter(fuel_type == "Diesel") %>%
  select(year,
         eu_rnpa,
         contains("subsidy"),
         fuel_type,
         treated,
         applied) %>% 
  group_by(eu_rnpa) %>% 
  mutate(n = n_distinct(fuel_type)) %>% 
  ungroup() %>% 
  filter(n == 1)

panel <- fuel_consumption %>% 
  left_join(subsidy_panel, by = c("eu_rnpa", "year")) %>% 
  left_join(fuel_prices, by = c("year", "fuel_type")) %>% 
  replace_na(replace = list(subsidy_cap_l = 0, subsidy_cap_pv_l = 0, treated = F, applied = F)) %>%
  mutate(ph = mean_price,
         pl = ph - 2,
         phi = subsidy_cap_l / fuel_consumption_l,
         D = phi <= 1L,
         R = ph - pl,
         c_term1 = pl + (D * R),
         c_term2 = (phi * R  * D),
         o_term1 = (pl * (1 - D)) + (ph * D),
         o_term2 = D * phi * (pl - ph))

write.csv(x = panel,
          file = file.path(project_path, "data", "processed_data", "estimation_panel.csv"),
          row.names = F)
