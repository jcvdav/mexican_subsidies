library(fixest)
library(modelsummary)
library(tidyverse)

panel <- read.csv(file.path(project_path, "data", "processed_data", "estimation_panel.csv")) %>% 
  mutate(act_price = (fuel_consumption_l<=subsidy_cap_l)*(treated)*pl + 
           (fuel_consumption_l<=subsidy_cap_l)*(!treated)*ph +
           (fuel_consumption_l>subsidy_cap_l)*ph,
         eu_rnpa = factor(eu_rnpa),
         year = year) %>% 
  drop_na(phi)

n_within_p <- function(p, data) {
  
  N <- panel %>% 
    pull(eu_rnpa) %>% 
    unique() %>% 
    n_distinct()
  
  n <- panel %>% 
    filter(between(phi, 1 - p, 1 + p)) %>% 
    pull(eu_rnpa) %>% 
    unique() %>% 
    n_distinct()
  
  n / N
}

pct_q <- seq(0, 1, by = 0.01)
pct_n <- map_dbl(pct_q, n_within_p, data = panel)
plot(pct_q, pct_n)


model1 <- feols(fuel_consumption_l ~ c_term1 + c_term2 + total_hp| eu_rnpa + species, data = panel)


modelsummary(list(model1, model2, model3),
             stars = T,
             statistic_vertical = F,
             gof_omit = "Adj|With|Pse|Log|IC",
             coef_map = c("c_term1" = "beta",
                          "c_term2" = "theta",
                          "total_hp" = "Total Engine Power (hp)",
                          "(Intercept)" = "Intercept"))

get_alpha <- function(model){
  beta <- coefficients(model)[1]
  theta <- coefficients(model)[2]
  
  alpha <- 1 + (theta/beta)
  return(alpha)
}

# saveRDS(object = model,
#         file = file.path(project_path, "data", "output_data", "model.rds"))


get_gamma <- function(model){
  beta <- coefficients(model)[1]
  theta <- coefficients(model)[2]
  
  gamma <- log((1 / (1 + (theta / beta))) - 1)
  return(gamma)
}

get_alpha(model)











