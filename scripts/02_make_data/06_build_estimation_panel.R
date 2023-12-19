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
  tidyverse
)

# fix_rnpa <- function(rnpa, length = 8){
#   rnpa[is.na(rnpa)] <- "_"
#   lengths <- stringr::str_length(rnpa)
#   missing <- pmax(length - lengths, 0)
#   zeroes <- purrr::map_chr(missing, ~paste(numeric(length = .x), collapse = ""))
#   out <- paste0(zeroes, rnpa)
#   return(out)
# }

## LOAD DATA ###################################################################

# Treatment --------------------------------------------------------------------
eu_subsidy_panel <- readRDS(file = here("data", "processed", "economic_unit_subsidy_panel.rds"))

# Outcome variables ------------------------------------------------------------
# Intensive (Time)
intensive <- readRDS(file = here("data", "processed", "intensive_margin.rds"))
# Extensive (area)
extensive <- readRDS(file = here("data", "processed", "extensive_margin.rds"))
# Fisheries production
landings <- readRDS(file = here("data", "processed", "shrimp_landings_panel.rds"))

# Covariate data sets ----------------------------------------------------------
# Annual national fuel prices
fuel_prices <- readRDS(here("data", "processed", "annual_national_diesel_prices_2011_2020.rds"))
# Nino 3.4 index
nino <- readRDS(
  file = here("data", "raw", "annual_nino34.rds"))


## PROCESSING ##################################################################
# Build baseline panel of subsidy amounts and intensive margin
subsidy_and_effort_panel <- intensive %>% 
  left_join(eu_subsidy_panel, by = c("year", "eu_rnpa")) %>% 
  replace_na(replace = list(treated = 0, subsidy_pesos = 0))

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
  group_by(eu_rnpa) %>% 
  filter(all(treated == 0)) %>% 
  pull(eu_rnpa) %>% 
  unique()

n_times_sub <- subsidy_and_effort_panel %>% 
  group_by(eu_rnpa) %>%
  summarize(n_times_sub = sum(treated)) %>% 
  arrange(n_times_sub) %>% 
  ungroup()

# X ----------------------------------------------------------------------------
shrimp <- subsidy_and_effort_panel %>%
  left_join(extensive, by = c("year", "eu_rnpa")) %>% 
  left_join(landings, by = c("year", "eu_rnpa" = "eu")) %>%
  left_join(nino, by = "year") %>%
  left_join(fuel_prices, by = "year") %>%
  left_join(n_times_sub, by = "eu_rnpa") %>%
  rename(eu = eu_rnpa) %>% 
  mutate(
    subsidy_frequency = case_when(eu %in% always ~ "always",
                                  eu %in% never ~ "never",
                                  T ~ "sometimes"),
    subsidy_frequency = fct_relevel(subsidy_frequency,
                                    "never", "sometimes", "always"),
    always = 1 * (eu %in% always),
    never = 1 * (eu %in% never),
    sometimes = 1 * (always == 0 & never == 0)) %>% 
  select(year, region, state, eu, total_hp, n_vessels,
         tuna, sardine, shrimp, others,
         treated, subsidy_pesos, n_times_sub, subsidy_frequency, always, sometimes, never,
         mean_diesel_price_mxn_l, nino34_m,
         hours, fg_area_km, fg_hours, landed_weight)

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

