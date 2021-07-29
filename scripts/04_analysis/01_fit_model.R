library(fixest)
library(modelsummary)
library(tidyverse)

panel <- read.csv(file.path(project_path, "data", "processed_data", "estimation_panel.csv")) %>% 
  mutate(act_price = (fuel_consumption_l<=subsidy_cap_l)*(treated)*pl + 
           (fuel_consumption_l<=subsidy_cap_l)*(!treated)*ph +
           (fuel_consumption_l>subsidy_cap_l)*ph,
         eu_rnpa = factor(eu_rnpa),
         year = year) %>% 
  drop_na(mean_price) %>% 
  filter(phi < 10)

est_panel <- panel  %>% 
  drop_na(mean_price) %>% 
  filter(!D | phi == 0)

model <- feols(fuel_consumption_l ~ act_price | eu_rnpa, data = est_panel)
model2 <- feols(fuel_consumption_l ~ act_price | eu_rnpa + year, data = est_panel)
model3 <- feols(fuel_consumption_l ~ act_price | eu_rnpa, data = panel)

modelsummary(model,
              stars = T,
              statistic_vertical = F,
              gof_omit = "Adj|With|Pse|Log|IC",
              coef_map = c("c_term1" = "Marginal price (MXN / L)",
                           "act_price" = "Marginal price (MXN / L)",
                           "treatedTRUE" = "Treated",
                           "subsidy_cap_l" = "Subsidy cap (L)",
                           "(Intercept)" = "Intercept"),
             output = file.path(project_path, "results", "tab", "regression_table.tex"))

saveRDS(object = model,
        file = file.path(project_path, "data", "output_data", "model.rds"))

