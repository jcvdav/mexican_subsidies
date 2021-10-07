
# Take alpha values as given, calculate average and

library(fixest)
library(modelsummary)
library(tidyverse)

panel <- read.csv(file.path(project_path, "data", "processed_data", "estimation_panel.csv")) %>% 
  mutate(act_price = (fuel_consumption_l<=subsidy_cap_l)*(treated)*pl + 
           (fuel_consumption_l<=subsidy_cap_l)*(!treated)*ph +
           (fuel_consumption_l>subsidy_cap_l)*ph,
         eu_rnpa = factor(eu_rnpa),
         year = year) %>% 
  drop_na(phi) %>% 
  select(year, eu_rnpa, species, total_hp, fuel_consumption_l, subsidy_cap_l, act_price, ph, pl, phi)

get_pa <- function(ph, pl, phi) {
  pa <- ph - (ph - pl) * phi
  
  pa[phi >= 1] <- pl[phi >= 1]
  
  return(pa)
  
}


fit_fixed_alpha <- function(alpha, data) {
  d <- data %>% 
    mutate(pa = get_pa(ph = ph, pl = pl, phi = phi),
           pp = (alpha * act_price) + ((1 - alpha) * pa))
  
  broom::glance(feols(fuel_consumption_l ~ pp + total_hp | eu_rnpa + species, data = d))
}

tibble(alpha = seq(0, 1, by = 0.1)) %>% 
  mutate(model = map(alpha, fit_fixed_alpha, data = panel)) %>% 
  unnest(model) %>% 
  select(-c(pseudo.r.squared, nobs, adj.r.squared, within.r.squared)) %>% 
  pivot_longer(names_to = "measure", cols = -alpha) %>% 
  ggplot(aes(x = alpha, y = value)) + 
  geom_line() +
  geom_point() + 
  facet_wrap(~measure, scales = "free")

