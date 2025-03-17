################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# CPI data from https://fred.stlouisfed.org/series/MEXCPIALLAINMEI#0
# Index 2015=100, Annual, not seasonally adjusted
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
library(here)
library(janitor)
library(tidyverse)

# Load data --------------------------------------------------------------------
# Load CPI
cpi_raw <-
  read_csv(here("data", "raw", "OECD_CPI_MEXCPIALLMINMEI.csv"))

## PROCESSING ##################################################################
# Clean ------------------------------------------------------------------------
cpi <- cpi_raw %>%
  clean_names() %>%
  rename(cpi = mexcpiallainmei)

# Extract new reference level --------------------------------------------------
cpi_2019 <- filter(cpi, date == "2019-01-01") %>%
  pull(cpi)

# %change = (baseline - unadjusted) / unadjusted * 100

cpi_t <- cpi %>%
  mutate(rate = 1 + ((cpi_2019 - cpi) / cpi),                                   # multipleir = 1 +  ((baseline - unadjusted) / unadjusted)
         year = lubridate::year(date)) %>%
  filter(year >= 2011) %>%
  select(year, rate)

## EXPORT ######################################################################
# Export CPI -------------------------------------------------------------------
saveRDS(
  object = cpi_t,
  file = here("data", "processed", "cpi_t_rates.rds")
)
