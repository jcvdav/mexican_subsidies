######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(here)
library(fixest)
library(tidyverse)


shrimp <- read_csv(
  file = file.path(
    project_path, "data", "processed_data", "imputed_subsidy_economic_unit_annual_shrimp_panel.csv")) %>% 
  filter(between(year, 2012, 2019))


get_dif <- function(data) {
  lm(formula = value ~ treated, data = data) %>%
    broom::tidy() %>% 
    filter(term == "treatedTRUE") %>% 
    mutate(stars = ifelse(p.value < 0.01, "***", ""),
           comb = paste0(round(estimate, 2), " (", round(std.error, 2), ")", stars)) %>% 
    pull(comb)
}

shrimp %>% 
  select(year, treated, total_hp, n_vessels, subsidy_cap_l, predicted_subsidy_cap_l) %>% 
  pivot_longer(cols = c(total_hp, n_vessels, subsidy_cap_l, predicted_subsidy_cap_l),
               names_to = "variable") %>% 
  group_by(year, variable) %>% 
  nest() %>% 
  mutate(dif = map_chr(data, get_dif)) %>% 
  select(-data) %>% 
  pivot_wider(names_from = variable, values_from = dif) %>% 
  knitr::kable(format = "latex",
               col.names = c("Year", "Total HP", "N Vessels", "Subsidy Cap (L)", "Predicted Subsidy Cap (L)"),
               booktabs = T,
               caption = "Summary statistics of vessel characteristics.
               The numbers show the difference in means between subsidized and unsubsidized vessels.
               Numbers in parentheses are standard errors.
               Total HP shows the differenc ein total engine capacity in horsepowers.
               N vessels shows the number of active vessels for each economic unit.
               Unsubsidized vessels have no subsidy cap, so the difference shows the average subsidy cap.") %>% 
  cat(file = here("results", "tab", "balance_in_time_table.tex"))
