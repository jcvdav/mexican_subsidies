
# Load packages
library(tidyverse)

# Read-in the data
subsidies <- 
  readRDS(file.path(project_path, "processed_data", "diesel_clean.rds"))          # Subsidies data

effort <- readRDS(file.path(project_path, "processed_data", "effort.rds"))

landings <- readRDS(file.path(.....)) 

a <- subsidies %>% 
  left_join(landings, by = c("economic_unit", "year")) %>% 
  filter(!diesel_liters<100)

ggplot(a, aes(x = diesel_liters, y = landed_weight, color = year)) +
  geom_point(alpha = 0.5) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  labs(x = "log10(Subsidies diesel) (L)",
       y = "log10(Total landings) (Kg)") +
  theme_bw() +
  facet_wrap(~main_group)


feols(log(landed_weight) ~ log(diesel_liters) | economic_unit + year + main_group, data = a) %>% 
  modelsummary::modelsummary(gof_omit = "Pseudo|Adj|With|IC|Log", stars = TRUE, statistic_vertical = FALSE)
