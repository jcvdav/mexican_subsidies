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
library(tidyverse)

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
  filter(year >= 2011, year <= 2020) %>% 
  pivot_longer(cols = c(2:13), names_to = "month", values_to = "nino34") %>% 
  group_by(year) %>% 
  summarize(nino34_m = max(nino34),
            nino34_sd = sd(nino34))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = nino, 
        file = here("data", "raw", "annual_nino34.rds"))
