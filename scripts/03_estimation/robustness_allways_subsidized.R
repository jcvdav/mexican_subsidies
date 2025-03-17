######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(here)
library(fixest)
library(modelsummary)
library(tidyverse)


shrimp <- read_csv(
  file = file.path(
    project_path, "data", "processed_data", "imputed_subsidy_economic_unit_annual_shrimp_panel.csv")) %>% 
  filter(between(year, 2012, 2019))

## Histogram of number of times vessels are subsidized
## Identification comes from change in adjustment factor
n_sub_hist <- shrimp %>% 
  count(eu_rnpa, treated) %>% 
  ggplot(aes(x = n, fill = treated)) +
  geom_histogram(binwidth = 1, color = "black", position = "dodge") +
  facet_wrap(~treated, ncol = 1) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(labels = c(0:8), breaks = c(0:8)) +
  labs(x = "Number of times",
       y = "Number of economic units",
       fill = "Subsidized")

ggsave(plot = n_sub_hist,
       filename = here("results", "img", "N_times_subsidized_histogram.pdf"),
       width = 6,
       height = 4.5)

############ ROBUSTNESS
by_numbers <- shrimp %>% 
  filter(treated) %>%
  group_by(eu_rnpa, treated) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n == 8) %>% 
  mutate(dist = (fuel_consumption_l - predicted_subsidy_cap_l) / 1000,
         rel_dist = dist / predicted_subsidy_cap_l,
         left = fuel_consumption_l <= subsidy_cap_l)


cts <- by_numbers %>% 
  count(year, p)

ggplot(by_numbers, aes(x = year, y = p)) +
  geom_line(aes(group = eu_rnpa)) +
  geom_point(data = cts, aes(size = n))

bm1 <- feols(log(fuel_consumption_l) ~ log(p) + total_hp, data = by_numbers)
bm2 <- feols(log(fuel_consumption_l) ~ log(p) + total_hp | eu_rnpa, data = by_numbers)
bm3 <- feols(log(fuel_consumption_l) ~ log(p) + total_hp + subsidy_cap_l | eu_rnpa, data = by_numbers)

# Make table
bmods <- list(bm1, bm2, bm3, bm3)
names(bmods) <- rep("log(L)", length(bmods))

modelsummary(bmods,
             # output = here("results", "tab", "always_subsidized.tex"),
             title = "Demand elasticity of fuel using the 177 economic units that are always subsidized.",
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             statistic_vertical = T,
             coef_rename = c("log(p)" = "log(price)",
                             "subsidy_cap_l" = "Subsidy Cap (L)",
                             "total_hp" = "Total Capacity (HP)",
                             "dist" = "Distance from kink (L)"))

# How many vessels are always to the right?
always_sub_rl <- by_numbers %>% 
  filter(treated) %>% 
  count(eu_rnpa, left_of_kink) %>% 
  ggplot(aes(x = n, fill = left_of_kink)) +
  geom_histogram(binwidth = 1, color = "black") +
  facet_wrap(~left_of_kink, ncol = 1) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(labels = c(1:8), breaks = c(1:8)) +
  labs(x = "Number of times",
       y = "Number of economic units",
       fill = "Left of kink?")

ggsave(plot = always_sub_rl,
       filename = here("results", "img", "always_sub_rl.pdf"),
       width = 6,
       height = 4.5)


my_mod <- function(data) {
  feols(log(fuel_consumption_l) ~  log(p) + total_hp + subsidy_cap_l | eu_rnpa, data) %>% 
    broom::tidy() %>% 
    filter(term == "log(p)") %>% 
    mutate(nobs = n_distinct(data$eu_rnpa))
}

wraper <- function(data, n) {
  data %>% 
    filter(treated) %>%
    group_by(eu_rnpa, treated) %>% 
    mutate(nn = n()) %>% 
    filter(nn >= n) %>% 
    my_mod()
}

wraper2 <- function(data, n) {
  data %>% 
    filter(treated) %>%
    group_by(eu_rnpa, treated) %>% 
    mutate(nn = n()) %>% 
    filter(nn <= n) %>% 
    my_mod()
}


d1 <- shrimp %>% 
  filter(treated) %>%
  group_by(eu_rnpa, treated) %>% 
  mutate(n = n()) %>% 
  filter(n > 2) %>% 
  group_by(n) %>% 
  nest() %>% 
  arrange(n) %>% 
  mutate(m = map(data, my_mod)) %>% 
  unnest(m) %>% 
  mutate(source = "Exactly N")

d2 <- 3:8 %>% 
  map_dfr(wraper, data = shrimp, .id = "n") %>% 
  mutate(n = as.numeric(n) + 2.1) %>% 
  mutate(source = "At least N")

d3 <- 3:8 %>% 
  map_dfr(wraper2, data = shrimp, .id = "n") %>% 
  mutate(n = as.numeric(n) + 2.2) %>% 
  mutate(source = "At most N")


reg_by_times_sub <- rbind(d1, d2, d3) %>% 
  ggplot(mapping = aes(x = n, y = estimate, ymin = estimate-std.error, ymax = estimate + std.error, color = source)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange() +
  scale_x_continuous(labels = 3:8, breaks = 3:8) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Number of times subsidized",
       y = "Estimate",
       color= "Source") 

ggsave(plot = reg_by_times_sub,
       filename = here("results", "img", "reg_by_n_times_sub.pdf"),
       width = 6,
       height = 4)

# mean cap in time for the 8 vessels
shrimp %>% 
  filter(treated) %>%
  ggplot(aes(x = year, y = log(subsidy_cap_l + 1), group = eu_rnpa, color = left_of_kink)) +
  geom_line(size = 0.1)