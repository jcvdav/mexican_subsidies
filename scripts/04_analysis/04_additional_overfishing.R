
library(tidyverse)

data <- readRDS(file.path(project_path, "data", "processed_data", "shrimp_estimation_panel.rds")) %>% 
  filter(year == 2019)

# How many percentage points does price change with a 1peso?
peso_as_percent <- (1 * 100) / unique(data$ph)
pct_mkt_price_change <- exp((log((100 - peso_as_percent)/100)) * -3.553)
pct_sub_price_change <- exp(0.810) - 1 #exp((log((100 - peso_as_percent)/100)) * (-0.810 / 2))
pct_sub_price_change <- exp(-0.101) + exp(-0.412)

data %>% 
  select(eu, fuel_consumption_l, ph, delta, total_hp, nino34_m, left) %>% 
  mutate(mkt = fuel_consumption_l * pct_mkt_price_change,
         sub = ifelse(left == 1, fuel_consumption_l * pct_sub_price_change, 1 * fuel_consumption_l)) %>% 
  summarise_at(vars(fuel_consumption_l, mkt, sub), sum) %>% 
  mutate(mkt_dif = mkt - fuel_consumption_l,
         sub_dif = sub - fuel_consumption_l,
         pct_mkt_dif = mkt_dif / fuel_consumption_l,
         sub_mkt_dif = sub_dif / fuel_consumption_l)
  

