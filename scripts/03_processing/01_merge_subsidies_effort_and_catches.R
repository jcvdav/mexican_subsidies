
# Load packages
library(tidyverse)

# Read-in the data
subsidies <- 
  readRDS(file.path(project_path, "processed_data", "diesel_clean.rds"))          # Subsidies data

effort <- readRDS(file.path(project_path, "processed_data", "effort.rds"))

landings <- readRDS(file.path(project_path, "processed_data", "")) 

a <- landings %>% 
  filter(year > 2010) %>% 
  left_join(subsidies, by = c("economic_unit", "year")) %>% 
  mutate(year = as.factor(year)) %>% 
  group_by(year, economic_unit, landed_weight) %>% 
  summarize(subsidy_amount = sum(subsidy_amount, na.rm = T)) %>% 
  replace_na(replace = list(subsidy_amount = 0))

ggplot(a, aes(x = diesel_liters, y = landed_weight, color = year)) +
  geom_point(alpha = 0.5) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  labs(x = "log10(Subsidies diesel) (L)",
       y = "log10(Total landings) (Kg)") +
  theme_bw() +
  facet_wrap(~main_group)


plm(formula = landed_weight ~ subsidy_amount,
    index = c("economic_unit", "year"), 
    model = "random",
    effect = "individual",
    data = a) %>% 
  modelsummary::modelsummary(gof_omit = "Pseudo|Adj|With|IC|Log", stars = TRUE, statistic_vertical = FALSE)


feols(landed_weight ~ subsidy_amount | economic_unit + year, data = a) %>% 
  modelsummary::modelsummary(gof_omit = "Pseudo|Adj|With|IC|Log", stars = TRUE, statistic_vertical = FALSE)

lm(landed_weight ~ subsidy_amount + economic_unit + year, data = a) %>% 
  modelsummary::modelsummary(gof_omit = "Pseudo|Adj|With|IC|Log", stars = TRUE, statistic_vertical = FALSE)
