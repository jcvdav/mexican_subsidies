######################################################
#title#
######################################################
# 
# Download and save the NINO3.4 index. It's reported
# monthly, we take the mean.
#
######################################################

library(tidyverse)

nino <- read_delim("https://psl.noaa.gov/data/correlation/nina34.data",
                   skip = 3,
                   col_names = F,
                   delim = "  ") %>% 
  magrittr::set_colnames(c("year", 1:12)) %>% 
  mutate_all(as.numeric) %>% 
  filter(year >= 2011, year <= 2020) %>% 
  pivot_longer(cols = c(2:13), names_to = "month", values_to = "nino34") %>% 
  group_by(year) %>% 
  summarize(nino34_m = max(nino34),
            nino34_sd = sd(nino34))

saveRDS(object = nino, 
        file = file.path(project_path, "data", "processed_data", "annual_nino34.rds"))
