
library(data.table)
library(fixest)
library(modelsummary)
library(tidyverse)

panel <- fread(file.path(project_path, "data", "processed_data", "estimation_panel.csv")) %>% 
  mutate(act_price = (fuel_consumption_l<=subsidy_cap_l)*(treated)*pl + 
           (fuel_consumption_l<=subsidy_cap_l)*(!treated)*ph +
           (fuel_consumption_l>subsidy_cap_l)*ph,
         eu_rnpa = factor(eu_rnpa))

model <- feols(fuel_consumption_l ~ act_price | factor(eu_rnpa), data = panel)

modelsummary(model,
             stars = T,
             statistic_vertical = F,
             gof_omit = "Adj|With|Pse|Log|IC",
             coef_map = c("c_term1" = "Marginal price (MXN / L)",
                          "act_price" = "Marginal price (MXN / L)",
                          "treatedTRUE" = "Treated",
                          "subsidy_cap_l" = "Subsidy cap (L)",
                          "(Intercept)" = "Intercept"),
             notes = "")






left <- panel %>% 
  filter(!D | phi == 0) %>% 
  group_by(eu_rnpa) %>% 
  mutate(n = n(),
         total_subsidy = sum(subsidy_cap_l, na.rm = T)) %>% 
  ungroup() %>% 
  filter(total_subsidy > 0,
         n >= 2)



# Plots

ggplot(data = left, aes(x = treated, y = fuel_consumption_l)) +
  geom_point()

ggplot(data = left, aes(x = subsidy_cap_l, y = fuel_consumption_l)) +
  geom_point()

ggplot(data = left, aes(x = fuel_consumption_l, y = c_term1)) +
  geom_point()

panel %>%
  group_by(eu_rnpa) %>% 
  mutate(fuel_mean = mean(fuel_consumption_l, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(f = fuel_consumption_l - fuel_mean) %>% 
  ggplot(aes(x = ph, y = f)) +
  geom_bin2d() +
  labs(x = "Mean fuel price", y = "Demeaned anual vessel-level fuel consmption") +
  lims(y = c(-0.4, 1) * 1e7)


left_models <- list(
  feols(fuel_consumption_l ~ c_term1 | eu_rnpa + year, data = left ),
  feols(fuel_consumption_l ~ treated, data = left),
  feols(fuel_consumption_l ~ treated | eu_rnpa + year, data = left),
  feols(fuel_consumption_l ~ subsidy_cap_l, data = left %>% filter(subsidy_cap_l > 0)),
  feols(fuel_consumption_l ~ subsidy_cap_l | eu_rnpa + year, data = left %>% filter(subsidy_cap_l > 0))
)


modelsummary(left_models,
             stars = T,
             statistic_vertical = F,
             gof_omit = "Adj|With|Pse|Log|IC",
             coef_map = c("c_term1" = "Marginal price (MXN / L)",
                          "treatedTRUE" = "Treated",
                          "subsidy_cap_l" = "Subsidy cap (L)",
                          "(Intercept)" = "Intercept"),
            notes = "")

# Right panel
right <- panel %>% 
  filter(D | !treated | phi == 0) %>%
  group_by(eu_rnpa) %>% 
  mutate(total_subsidy = sum(subsidy_cap_l, na.rm = T)) %>% 
  ungroup() %>% 
  filter(total_subsidy > 0)

# Plots

ggplot(data = right, aes(x = treated, y = fuel_consumption_l)) +
  geom_point()

ggplot(data = right, aes(x = subsidy_cap_l, y = fuel_consumption_l)) +
  geom_point()

ggplot(data = right, aes(x = fuel_consumption_l, y = c_term1)) +
  geom_point()


right_models <- list(
  feols(fuel_consumption_l ~ treated, data = right),
  feols(fuel_consumption_l ~ subsidy_cap_l, data = right),
  feols(fuel_consumption_l ~ subsidy_cap_l, data = right %>% filter(subsidy_cap_l > 0)),
  feols(fuel_consumption_l ~ c_term1 | eu_rnpa, data = right)
)

modelsummary(right_models,
             stars = T,
             statistic_vertical = F,
             gof_omit = "Adj|With|Pse|Log|IC",
             coef_map = c("treatedTRUE" = "Treated",
                          "subsidy_cap_l" = "Subsidy cap (L)",
                          "c_term1" = "Marginal price (MXN / L)",
                          "c_term2" = "Perceived price part",
                          "(Intercept)" = "Intercept"))


panel2 <- panel %>% 
  group_by(eu_rnpa) %>% 
  mutate(n = n(),
         total_subsidy = sum(subsidy_cap_l, na.rm = T)) %>% 
  ungroup() %>% 
  filter(total_subsidy > 0,
         n >= 2) %>% 
  filter(fuel_consumption_l > 0)


full_models <- list(
  feols(fuel_consumption_l ~ c_term1 + c_term2, data = panel2),
  feols(fuel_consumption_l ~ c_term1 + c_term2 | eu_rnpa , data = panel2),
  feols(fuel_consumption_l ~ c_term1 + c_term2 | eu_rnpa + year, data = panel2),
  feols(fuel_consumption_l ~ c_term1 + c_term2 + n_vessels | eu_rnpa + year, data = panel2),
  feols(fuel_consumption_l ~ c_term1 + c_term2 + total_hp | eu_rnpa + year, data = panel2)
)

modelsummary(full_models,
             stars = T,
             statistic_vertical = F,
             gof_omit = "Adj|With|Pse|Log|IC",
             coef_map = c("treatedTRUE" = "Treated",
                          "subsidy_cap_l" = "Subsidy cap (L)",
                          "c_term1" = "Marginal price (MXN / L)",
                          "c_term2" = "Perceived price part",
                          "n_vessels" = "# Vessels",
                          "total_hp" = "Total engine power (hp)",
                          "(Intercept)" = "Intercept"))

