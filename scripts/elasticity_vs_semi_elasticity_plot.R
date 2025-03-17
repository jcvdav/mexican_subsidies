semi_mod <- readRDS(file = here("results", "models", "semi_elasticity_twfe.rds"))
elasticity_mod <- readRDS(here("results", "models", "elasticity_twfe.rds"))

entry_semi_elasticity <- (exp(coef(semi_mod$`Fishing time`)[1])-1) * 100
# exit_semi_elasticity <- (exp(coef(models2$`Hours`)[1])-1) * 100
elasticity <- coef(elasticity_mod$`Fishing time`)[1]

tibble(pct = seq(-0.99, 0.999, by = 0.001)) %>% 
  mutate(change = (((1 + pct)^elasticity)-1) * 100) %>% 
  ggplot(aes(x = pct * 100, y = change)) +
  geom_line() +
  geom_hline(yintercept = entry_semi_elasticity, color = "red") +
  labs(x = "% Change in subsidy",
       y = "% Change in hours")
