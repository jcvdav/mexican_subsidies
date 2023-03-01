################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Combines SIE data
# https://sie.energia.gob.mx/bdiController.do?action=cuadro&subAction=applyOptions
#
# There are two data sources. On both, use the "options" to ask for annual data
# The 2011 - 2015 data come from -----------------------------------------------
# Sector Energético >
#  HIDROCARBUROS >
#    Petrolíferos >
#      Precio al público de petrolíferos seleccionados, vigente a partir de 1995
# The only metadata aviualable there is:
# Sistema de Información Energética
# Petróleos Mexicanos
# Precio público ponderado de productos petrolíferos seleccionados
# Vigente a partir de 1995
# (pesos por metro cubico)
#
# 2016 data come from ----------------------------------------------------------
# Sector Energético >
#  HIDROCARBUROS >
#    Petrolíferos >
#      Precio al público de gasolinas y diésel, vigente a partir de 2016
#
# The metadata state:
# Sistema de Información Energética
# Secretaría de Energía
# Precio al público de gasolinas y diésel
# Vigente a partir de 2016
# (pesos por litro)
#
################################################################################

## SET UP ######################################################################
# Load packages ----------------------------------------------------------------
library(here)
library(readxl)
library(tidyverse)

# Load data --------------------------------------------------------------------
sie_2011_2015_raw <-
  read_excel(here(
    "data",
    "raw",
    "fuel_prices",
    "sie_anual_nacional_2011_2015.xls"
  ),
  skip = 8)

sie_2016_raw <-
  read_excel(here("data", "raw", "fuel_prices", "sie_anual_nacional_2016.xls"),
             skip = 8)
## PROCESSING ##################################################################

# Clean 2011 to 2015 -----------------------------------------------------------
sie_2011_2015 <- sie_2011_2015_raw %>%
  select(1:6) %>%
  magrittr::set_colnames(value = c("variable", 2011:2015)) %>%
  filter(variable == "Pemex Diesel") %>%
  select(-variable) %>%
  mutate_all(as.numeric) %>%
  pivot_longer(cols = c(1:5),
               names_to = "year",
               values_to = "mean_diesel_price_mxn_l") %>%
  mutate(mean_diesel_price_mxn_l = mean_diesel_price_mxn_l / 1e3)               # convert from cubic meters to liters

# Clean 2016 ------------------------------------------------------------------
sie_2016 <- sie_2016_raw %>%
  select(1:2) %>%
  magrittr::set_colnames(value = c("variable", 2016)) %>%
  filter(variable == "Diésel") %>%
  select(-variable) %>%
  pivot_longer(cols = 1,
               names_to = "year",
               values_to = "mean_diesel_price_mxn_l")


# Combine ----------------------------------------------------------------------
sie_2011_2016 <-
  rbind(sie_2011_2015,
        sie_2016)

## EXPORT ######################################################################

# Export a single sie file -----------------------------------------------------
saveRDS(
  object = sie_2011_2016,
  file = here(
    "data",
    "processed",
    "annual_national_diesel_prices_sie_2011_2016.rds"
  )
)
