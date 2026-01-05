################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
# Go here: https://psl.noaa.gov/data/climateindices/list/
# Find:
# Niño 3.4*	East Central Tropical Pacific SST (5N-5S)(170-120W): From CPC
# CPC uses the NOAA ERSST V5 anomalies. Now uses https://www.cpc.ncep.noaa.gov/data/indices/ersst5.nino.mth.91-20.ascii. Mean values also available.
# Click on "Mean Values", which akes you here: https://psl.noaa.gov/data/correlation/nina4.data
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse
)

# Load data --------------------------------------------------------------------
nino_raw <- read_delim("https://psl.noaa.gov/data/correlation/nina34.data",
                       skip = 3,
                       col_names = F,
                       delim = "  ")

## PROCESSING ##################################################################

# Get annual means -------------------------------------------------------------
nino <- nino_raw %>% 
  magrittr::set_colnames(c("year", 1:12)) %>% 
  mutate_all(as.numeric) %>% 
  filter(year >= 2011, year <= 2024) %>% 
  pivot_longer(cols = c(2:13), names_to = "month", values_to = "nino34") %>% 
  group_by(year) %>% 
  summarize(nino34_m = max(nino34),
            nino34_sd = sd(nino34))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = nino, 
        file = here("data", "raw", "annual_nino34.rds"))
