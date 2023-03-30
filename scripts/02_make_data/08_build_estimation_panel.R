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
library(tidyverse)

fix_rnpa <- function(rnpa, length = 8){
  rnpa[is.na(rnpa)] <- "_"
  lengths <- stringr::str_length(rnpa)
  missing <- pmax(length - lengths, 0)
  zeroes <- purrr::map_chr(missing, ~paste(numeric(length = .x), collapse = ""))
  out <- paste0(zeroes, rnpa)
  return(out)
}

# Load data --------------------------------------------------------------------
cpi_t <- readRDS(
  file = here("data", "processed", "cpi_t_rates.rds"))

state_prices <- readRDS(
  file = here("data",
              "processed",
              "annual_state_diesel_prices_cre_2017_2020.rds")) %>%
  rename(p_stat = mean_diesel_price_mxn_l) %>%
  left_join(cpi_t, by = "year") %>%
  mutate(p_stat = p_stat * rate) %>%
  select(-rate)

subsidy_and_effort_panel <- readRDS(
  file = here(
    "data",
    "processed",
    "imputed_subsidy_economic_unit_annual_shrimp_panel.rds")) %>% 
  mutate(eu_rnpa = fix_rnpa(eu_rnpa, length = 10)) %>% 
  left_join(cpi_t, by = "year") %>%
  mutate(subsidy_pesos = subsidy_cap_l * 2,
         subsidy_pesos = subsidy_pesos * rate) %>%
  select(-rate) %>% 
  filter(hours > 24 * 30)

nino <- readRDS(
  file = here("data", "raw", "annual_nino34.rds"))

shrimp_landings <- readRDS(
  file = here("data", "processed", "shrimp_landings_panel.rds"))

extensive <- readRDS(
  file = here("data", "extensive_margin.rds")) %>% 
  mutate(eu_rnpa = fix_rnpa(eu_rnpa, length = 10),
         year = as.numeric(year)) %>% 
  filter(area > 0)

## PROCESSING ##################################################################

# Find vessels that are always subsidized
always <- subsidy_and_effort_panel %>% 
  filter(treated == 1) %>% 
  group_by(eu_rnpa) %>% 
  add_count() %>% 
  ungroup() %>%
  filter(n == n_distinct(year)) %>% 
  pull(eu_rnpa) %>% 
  unique()

# Find vessels that were never subsidized
never <- subsidy_and_effort_panel %>% 
  filter(treated == 0) %>% 
  group_by(eu_rnpa) %>% 
  add_count() %>% 
  ungroup() %>%
  filter(n == n_distinct(year)) %>% 
  pull(eu_rnpa) %>% 
  unique()

n_times_sub <- subsidy_and_effort_panel %>% 
  group_by(eu_rnpa) %>%
  summarize(n_times_sub = sum(treated)) %>% 
  arrange(n_times_sub) %>% 
  ungroup() %>% 
  rename(eu = eu_rnpa)

# X ----------------------------------------------------------------------------
shrimp <- subsidy_and_effort_panel %>%
  filter(year < 2020) %>% 
  # filter(between(year, 2012, 2019)) %>% # Used to be part of the PEW report
  rename(eu = eu_rnpa) %>%
  left_join(state_prices, by = c("year", "state")) %>%
  left_join(nino, by = "year") %>%
  left_join(shrimp_landings, by = c("year", "eu")) %>%
  left_join(n_times_sub, by = "eu") %>% 
  left_join(extensive, by = c("year", "eu" = "eu_rnpa")) %>% 
  mutate(
    subsidy_frequency = case_when(eu %in% always ~ "always",
                                  eu %in% never ~"never",
                                  T ~ "sometimes"),
    always = 1 * (eu %in% always),
    never = 1 * (eu %in% never),
    sometimes = 1 * (always == 0 & never == 0),
    treated = 1 * treated,
    R = 2,
    delta = -R * treated ,
    left = 1 * (fuel_consumption_l <= predicted_subsidy_cap_l),
    right = 1 - left,
    free_fuel_l = subsidy_pesos / ph,
    predicted_subsidy_pesos = predicted_subsidy_cap_l * 2,
    pp = pmax(0, (ph * (fuel_consumption_l - (subsidy_pesos / ph)) / fuel_consumption_l))
    #D = phi < 1L#,
    # term1 = pl + (D * R),
    # term2 = phi * R  * D
  ) %>% 
  select(year, region, state, eu, total_hp, n_vessels,
         tuna, sardine, shrimp, others,
         treated, left, right, subsidy_pesos, subsidy_cap_l,
         predicted_subsidy_pesos, predicted_subsidy_cap_l, free_fuel_l,
         n_times_sub, subsidy_frequency, always, sometimes, never,
         ph, pl, p, pp, delta, p_stat, nino34_m,
         fuel_consumption_l, hours, landed_weight, area)

## EXPORT ######################################################################

# RDS for local stuff ----------------------------------------------------------
saveRDS(
  object = shrimp,
  file = here(
    "data",
    "estimation_panels",
    "shrimp_estimation_panel.rds"
  )
)

# CSV for Olivie's STATA analysis ----------------------------------------------
# In emlab drive
write_csv(
  x = shrimp,
  file = file.path(
    project_path,
    "data",
    "processed_data",
    "shrimp_estimation_panel.csv"
  )
)
# Local file
write_csv(
  x = shrimp,
  file = here(
    "data",
    "estimation_panels",
    "shrimp_estimation_panel.csv"
  )
)
