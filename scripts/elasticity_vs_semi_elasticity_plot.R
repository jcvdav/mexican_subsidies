

entry_semi_elasticity <- (exp(coef(models$`Hours`)[1])-1) * 100
exit_semi_elasticity <- (exp(coef(models2$`Hours`)[1])-1) * 100
elasticity <- coefficients(elasticity_twfe[[1]])[1]

tibble(pct = seq(-0.99, 0.99, by = 0.01)) %>% 
  mutate(change = (((1 + pct)^elasticity)-1) * 100) %>% 
  ggplot(aes(x = pct * 100, y = change)) +
  geom_line() +
  # geom_hline(yintercept = semi_elasticity) +
  geom_hline(yintercept = entry_semi_elasticity, color = "red") +
  geom_hline(yintercept = exit_semi_elasticity, color = "blue") +
  labs(x = "% Change in subsidy",
       y = "% Change in hours")
