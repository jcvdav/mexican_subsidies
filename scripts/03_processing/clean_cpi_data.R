######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(janitor)
library(tidyverse)

# Load CPI #https://fred.stlouisfed.org/series/MEXCPIALLAINMEI#0 Index 2015=100, Annual, not seasonally adjusted
cpi <- read_csv(file.path(project_path, "data", "raw_data", "OECD_CPI_MEXCPIALLMINMEI.csv")) %>% 
  clean_names() %>% 
  rename(cpi = mexcpiallainmei)

cpi_2019 <- filter(cpi, date == "2019-01-01") %>% 
  pull(cpi)

# %change = (baseline - unadjusted) / unadjusted * 100
# mjltipleir = 1 +  ((baseline - unadjusted) / unadjusted)

cpi_t <- cpi %>% 
  mutate(rate = 1 + ((cpi_2019 - cpi) / cpi),
         year = lubridate::year(date)) %>% 
  filter(year >= 2011) %>% 
  select(year, rate)

saveRDS(object = cpi_t,
        file = file.path(project_path, "data", "processed_data", "cpi_t_rates.rds"))
