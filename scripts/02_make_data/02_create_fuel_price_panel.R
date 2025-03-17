################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Combines fuel prices from SIE (2011 - 2016), CRE (2017 - 2020), and CPI data
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse
)

# Load data --------------------------------------------------------------------
cpi_t <-
  readRDS(here("data", "processed", "cpi_t_rates.rds"))

sie_2011_2016 <- readRDS(here(
  "data",
  "processed",
  "annual_national_diesel_prices_sie_2011_2016.rds"
))

cre_2017_2020 <- readRDS(
  here(
    "data",
    "processed",
    "annual_national_diesel_prices_cre_2017_2020.rds"
  )
)


## PROCESSING ##################################################################

# Build panel ------------------------------------------------------------------
annual_national_diesel_prices_2011_2020 <-
  rbind(sie_2011_2016,
        cre_2017_2020) %>%
  mutate(year = as.integer(year)) %>%
  left_join(cpi_t, by = "year") %>%
  mutate(mean_diesel_price_mxn_l = rate * mean_diesel_price_mxn_l) %>% 
  select(-rate)

## EXPORT ######################################################################
saveRDS(
  object = annual_national_diesel_prices_2011_2020,
  file = here(
    "data",
    "processed",
    "annual_national_diesel_prices_2011_2020.rds"
  )
)
