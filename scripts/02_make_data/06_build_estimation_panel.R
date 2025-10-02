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

## LOAD DATA ###################################################################

# Treatment --------------------------------------------------------------------
eu_subsidy_panel <- readRDS(file = here("data", "processed", "economic_unit_subsidy_panel.rds"))

# EUs to exclude ---------------------------------------------------------------
eu_modernized <- readRDS(file = here("data/processed/economic_unit_modernized_vessels.rds")) |> 
  rename(m_year = year,
         m_eu = rnpa)

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
# Build balanced panel foundation
first_year_by_eu <- intensive |> 
  filter(year <= 2019) |> 
  group_by(region, eu_rnpa, n_vessels, total_hp) |> 
  slice_min(year) |> 
  select(eu_rnpa, region, first_year = year)

foundation <- expand_grid(year = min(intensive$year):max(intensive$year),
                          eu_rnpa = unique(first_year_by_eu$eu_rnpa)) |> 
  left_join(first_year_by_eu, by = join_by(eu_rnpa)) |> 
  filter(year >= first_year) |> 
  select(-first_year)

# Build baseline panel of subsidy amounts and intensive margin
subsidy_and_effort_panel <- foundation |> 
  left_join(intensive |> 
              select(year, eu_rnpa, hours), by = join_by(eu_rnpa, year)) |> 
  left_join(eu_subsidy_panel, by = c("year", "eu_rnpa")) |> 
  replace_na(list(treated = 0))

# Find vessels that are always subsidized
always <- subsidy_and_effort_panel |> 
  filter(treated == 1,
         modernized == 0) |> 
  group_by(eu_rnpa) |> 
  add_count() |> 
  ungroup() |>
  filter(n == n_distinct(year)) |> 
  pull(eu_rnpa) |> 
  unique()

# Find vessels that were never subsidized
never <- subsidy_and_effort_panel |> 
  group_by(eu_rnpa) |> 
  filter(all(treated == 0)) |> 
  pull(eu_rnpa) |> 
  unique()

n_times_sub <- subsidy_and_effort_panel |> 
  group_by(eu_rnpa) |>
  summarize(n_times_sub = sum(treated)) |> 
  arrange(n_times_sub) |> 
  ungroup()

# Build the panel -------------------------------------------------------------
shrimp <- subsidy_and_effort_panel |>
  left_join(extensive, by = c("year", "eu_rnpa")) |> 
  left_join(landings, by = c("year", "eu_rnpa" = "eu")) |>
  left_join(nino, by = "year") |>
  left_join(fuel_prices, by = "year") |>
  left_join(n_times_sub, by = "eu_rnpa") |>
  left_join(eu_modernized, by = join_by(eu_rnpa == m_eu, year >= m_year)) |>
  rename(eu = eu_rnpa) |> 
  replace_na(replace = list(subsidy_pesos = 0, modernized = 0,
                            hours = 0, fg_area_km = 0, fg_hours = 0,
                            live_weight = 0, landed_weight = 0)) |> 
  mutate(
    subsidy_frequency = case_when(eu %in% always ~ "always",
                                  eu %in% never ~ "never",
                                  T ~ "sometimes"),
    subsidy_frequency = fct_relevel(subsidy_frequency,
                                    "never", "sometimes", "always"),
    always = 1 * (eu %in% always),
    never = 1 * (eu %in% never),
    sometimes = 1 * (always == 0 & never == 0)) |> 
  select(year, region, eu, total_hp, n_vessels,
         modernized, treated, subsidy_pesos, n_times_sub, subsidy_frequency, always, sometimes, never,
         mean_diesel_price_mxn_l, nino34_m,
         hours, fg_area_km, fg_hours, landed_weight, live_weight)

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

