######################################################
#title#
######################################################
# 
# DID on the 2014 reduction
#
######################################################

library(here)
library(fixest)
library(tidyverse)


shrimp <- read_csv(
  file = file.path(
    project_path, "data", "processed_data", "imputed_subsidy_economic_unit_annual_shrimp_panel.csv")) %>% 
  filter(between(year, 2012, 2019))


before <- shrimp %>% 
  # mutate(left = fuel_consumption_l < predicted_subsidy_cap_l) %>%
  mutate(left = fuel_consumption_l < subsidy_cap_l) %>%
  filter(left,
         between(year, 2012, 2014)) %>% 
  group_by(eu_rnpa, left) %>% 
  mutate(n = n(),
         nt = sum(treated),
         nc = sum(!treated)) %>% 
  ungroup() %>% 
  filter(n == 3,
         nt == 3 | nc == 3)


did_eu <- shrimp %>% 
  # mutate(left = fuel_consumption_l < predicted_subsidy_cap_l) %>% 
  mutate(left = (treated & fuel_consumption_l < subsidy_cap_l) | (!treated & fuel_consumption_l < predicted_subsidy_cap_l)) %>%
  filter(left,
         year <= 2013) %>% 
  count(eu_rnpa) %>% 
  arrange(desc(n)) %>% 
  filter(n >= 2) %>% 
  pull(eu_rnpa) %>% 
  unique()

length(did_eu)

subs_bef <- shrimp %>% 
  # mutate(left = fuel_consumption_l < predicted_subsidy_cap_l) %>% 
  mutate(left = (treated & fuel_consumption_l < subsidy_cap_l) | (!treated & fuel_consumption_l < predicted_subsidy_cap_l)) %>%
  filter(year <= 2013,
         subsidy_cap_l > 0,
         left) %>% 
  pull(eu_rnpa) %>% 
  unique()

length(subs_bef)

aft <- shrimp %>% 
  filter(year == 2014) %>% 
  pull(eu_rnpa) %>% 
  unique()

length(aft)

three <- shrimp %>% 
  filter(year <= 2014,
         eu_rnpa %in% did_eu,
         eu_rnpa %in% aft) %>% 
  mutate(event_time = factor(year),
         after = year == 2014,
         treated = eu_rnpa %in% subs_bef) 


three %>%
  select(eu_rnpa, treated, after) %>%
  distinct() %>%
  count(after, treated) %>% 
  spread(after, n)

did_fuel <- ggplot(three, aes(x = event_time, y = log(fuel_consumption_l), fill = treated, color = treated, group = treated)) + 
  # geom_jitter(height = 0, width = 0.25, size = 1, alpha = 0.5) +
  stat_summary(geom = "line", fun = "mean") +
  stat_summary(geom = "point", fun = "mean", color = "black", shape = 21, size = 4) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")

did_hours <- ggplot(three, aes(x = event_time, y = log(hours), fill = treated, color = treated, group = treated)) + 
  # geom_jitter(height = 0, width = 0.25, size = 1, alpha = 0.5) +
  stat_summary(geom = "line", fun = "mean") +
  stat_summary(geom = "point", fun = "mean", color = "black", shape = 21, size = 4) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")


did_plot <- plot_grid(did_fuel, did_hours, ncol = 1)

ggsave(plot = did_plot,
       filename = here("results", "img", "did_setup.pdf"),
       width = 6,
       height = 6)

m <- feols(log(fuel_consumption_l) ~ i(event_time, treated, 2013) + total_hp | eu_rnpa + event_time, three)
m2 <- feols(log(hours) ~ i(event_time, treated, 2013) + total_hp | eu_rnpa + event_time, three)

list(m, m2) %>%
  coefplot(main = "Fuel (black), Hours (red)")

list(m, m2) %>% 
  map_dfr(broom::tidy, .id = "m") %>% 
  filter(str_detect(term, "[:digit:]")) %>% 
  mutate(term = str_extract(term, "[:digit:]+"),
         m = ifelse(m == 1, "fuel consumption", "hours")) %>% 
  ggplot(aes(x = term, y = estimate, ymin = estimate - std.error, ymax = estimate + std.error, fill = m)) +
  geom_pointrange(shape = 21, size = 1) +
  scale_fill_brewer(palette = "Set1")
