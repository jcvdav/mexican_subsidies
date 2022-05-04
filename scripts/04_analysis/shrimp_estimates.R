
library(here)
library(fixest)
library(modelsummary)
library(ggridges)
library(cowplot)
library(tidyverse)


shrimp <- read_csv(
  file = file.path(
  project_path, "data", "processed_data", "imputed_subsidy_economic_unit_annual_shrimp_panel.csv")) %>% 
  filter(between(year, 2012, 2019)) %>% 
  mutate(extra_l = pmax(0, fuel_consumption_l - predicted_subsidy_cap_l) / 1e3)


# left of predicted
left_pred <- shrimp %>% 
  filter(fuel_consumption_l <= predicted_subsidy_cap_l) %>% 
  mutate(dist = fuel_consumption_l - predicted_subsidy_cap_l,
         rel_dist = fuel_consumption_l / predicted_subsidy_cap_l,
         ps = 2 * treated)

right_pred <- shrimp %>% 
  filter(fuel_consumption_l > predicted_subsidy_cap_l) %>% 
  mutate(dist = fuel_consumption_l - predicted_subsidy_cap_l,
         log_dist_dummy = round(log(dist)),
         log_dist_dummy = factor(ifelse(log_dist_dummy <= 10, "<=10", log_dist_dummy)),
         log_dist_dummy = fct_reorder(log_dist_dummy, dist, function(x){mean(round(log(x)))}),
         rel_dist = fuel_consumption_l / predicted_subsidy_cap_l,
         rel_dist_dummy = round(rel_dist),
         rel_dist_dummy = factor(ifelse(rel_dist_dummy >= 9, "9+", rel_dist_dummy)),
         dist_quant = factor(cut(rel_dist, quantile(rel_dist, probs = seq(0, 1, by = 0.1)), labels = F)),
         t = 1 * treated)



shrimp %>% count(year, treated) %>% spread(treated, n) %>% mutate(n = `TRUE` + `FALSE`)
left_pred %>% count(year, treated) %>% spread(treated, n) %>% mutate(n = `TRUE` + `FALSE`)
right_pred %>% count(year, treated) %>% spread(treated, n) %>% mutate(n = `TRUE` + `FALSE`)

shrimp %>%
  mutate(treated = ifelse(treated, "sub", "not_sub")) %>% 
  select(year, treated, total_hp, n_vessels, subsidy_cap_l, predicted_subsidy_cap_l, left_of_kink) %>% 
  group_by(year, treated) %>% 
  summarize_all(mean) %>% 
  pivot_longer(cols = c(total_hp, n_vessels, subsidy_cap_l, predicted_subsidy_cap_l, left_of_kink),
               names_to = "variable") %>% 
  pivot_wider(names_from = treated) %>% 
  mutate(difference = sub - not_sub) 
  
lm(total_hp ~ factor(year) : treated -1, shrimp) %>% 
  broom::tidy() %>% 
  mutate(term = str_remove_all(term, "[factor(year)]|[:treated]"),
         comb = paste0(round(estimate, 2), " (", round(std.error, 2), ")"),
         year = str_extract(term, "[:digit:]+"),
         treated = str_extract(term, "[:alpha:]+")) %>% 
  select(year, treated, comb) %>% 
  spread(treated, comb)

shrimp %>% filter(year == 2019) %>% t.test(formula = total_hp ~ treated, data = .) %>% broom::tidy()

get_dif <- function(data) {
  lm(formula = value ~ treated, data = data) %>%
    broom::tidy() %>% 
    filter(term == "treatedTRUE") %>% 
    mutate(stars = ifelse(p.value < 0.01, "***", ""),
           comb = paste0(round(estimate, 2), " (", round(std.error, 2), ")", stars)) %>% 
    pull(comb)
}

shrimp %>% filter(total_hp < 5000) %>% 
  select(year, treated, total_hp, n_vessels, subsidy_cap_l, predicted_subsidy_cap_l) %>% 
  pivot_longer(cols = c(total_hp, n_vessels, subsidy_cap_l, predicted_subsidy_cap_l),
               names_to = "variable") %>% 
  group_by(year, variable) %>% 
  nest() %>% 
  mutate(dif = map_chr(data, get_dif)) %>% 
  select(-data) %>% 
  pivot_wider(names_from = variable, values_from = dif) %>% 
  knitr::kable(format = "latex",
               col.names = c("Year", "Total HP", "N Vessels", "Subsidy Cap (L)", "Predicted Subsidy Cap (L)"),
               booktabs = T,
               caption = "Summary statistics of vessel characteristics.
               The numbers show the difference in means between subsidized and unsubsidized vessels.
               Numbers in parentheses are standard errors.
               Total HP shows the differenc ein total engine capacity in horsepowers.
               N vessels shows the number of active vessels for each economic unit.
               Unsubsidized vessels have no subsidy cap, so the difference shows the average subsidy cap.") %>% 
  cat(file = here("results", "tab", "balance_in_time.txt"))

eus <- shrimp %>% 
  select(-year) %>% 
  group_by(eu_rnpa) %>% 
  summarize_if(is.numeric, mean) %>% 
  filter(total_hp %in% range(total_hp) | n_vessels == max(n_vessels) | fuel_consumption_l %in% range(fuel_consumption_l) | hours %in% range(hours))

ihs <- shrimp %>% 
  mutate(ihs = log(subsidy_cap_l + sqrt(subsidy_cap_l ^ 2 + 1))) %>% 
  mutate(highlight = eu_rnpa %in% by_numbers$eu_rnpa) %>% 
  arrange(highlight) %>% 
  ggplot(mapping = aes(x = year, y = ihs, group = eu_rnpa, color = highlight)) +
  geom_line(size = 0.1, alpha = 0.3) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Year",
       y = "Subsidy cap (L)\n(Inverse hypoerbolic sine)",
       color = "Always subsidized") 

ggsave(plot = ihs,
       filename = here("results", "img", "ihs.pdf"),
       width = 6,
       height = 3)

ggplot(data = shrimp, aes(x = total_hp, fill = treated)) +
  geom_histogram(position = "dodge", bins = 10) +
  facet_wrap(~year) +
  scale_y_log10()


  
## FIGURES #####################################################################

# 2-peso subsidy as a percent of that year's price
a <- left_pred %>% 
  select(year, ph, pl) %>% 
  distinct() %>% 
  mutate(p_prop = ((ph - pl) / ph))

b <- shrimp %>% 
  group_by(year) %>% 
  summarize(tot = sum(subsidy_cap_l) / 1e3) %>% 
  ungroup()

price <- a %>% 
  select(year, ph, pl) %>% 
  pivot_longer(cols = c("ph", "pl"), names_to = "type", values_to = "p") %>% 
  ggplot(aes(x = year, y = p, color = type)) +
  geom_line() +
  scale_x_continuous(labels = seq(2011, 2019, by = 2), breaks = seq(2011, 2019, by = 2)) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Year",
       y = "Price subsidy\n(MXP / L)",
       color = "Price")

pct_price <- ggplot(a, aes(x = year, y = p_prop)) +
  geom_col() +
  scale_x_continuous(labels = seq(2011, 2019, by = 2), breaks = seq(2011, 2019, by = 2)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Year", y = "Price subsidy\n(% of market price)")

subsidy_amount <- ggplot(b, aes(x = year, y = tot)) +
  stat_summary(geom = "col", fun = sum, color = "black") +
  scale_x_continuous(labels = seq(2011, 2019, by = 2), breaks = seq(2011, 2019, by = 2)) +
  labs(x = "Year",
       y = "Total subsidy\n(L)")

subsidized_vessels <- shrimp %>%
  count(year, treated) %>% 
  ggplot(aes(x = year, y = n, fill = treated)) +
  geom_col(color = "black") +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Year",
       y = "Number of\neconomic units",
       fill = "Subsidized")

p1 <- plot_grid(price, pct_price, subsidized_vessels, subsidy_amount, labels = "AUTO", label_x = 0.9)

ggsave(plot = p1,
       filename = here("results", "img", "time_series.pdf"),
       width = 12,
       height = 6)


## ANALYSIS ####################################################################
before <- shrimp %>% 
  mutate(left = fuel_consumption_l < predicted_subsidy_cap_l) %>% 
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
  mutate(left = fuel_consumption_l < predicted_subsidy_cap_l) %>% 
  filter(left,
         year <= 2013) %>% 
  count(eu_rnpa) %>% 
  arrange(desc(n)) %>% 
  filter(n >= 2) %>% 
  pull(eu_rnpa) %>% 
  unique()

length(did_eu)

subs_bef <- shrimp %>% 
  mutate(left = fuel_consumption_l < predicted_subsidy_cap_l) %>% 
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
         treated = eu_rnpa %in% subs_bef) %>% 
  filter(!eu_rnpa %in% c("2005003518", "2508004906", "3010002446"))


three %>%
  select(eu_rnpa, treated, after) %>%
  distinct() %>%
  count(after, treated) %>% 
  spread(after, n)

three %>%
  select(eu_rnpa, treated, year) %>%
  spread(year, treated) %>%
  View()



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

ggplot(three, aes(x = event_time, y = log(fuel_consumption_l), fill = treated, color = treated, group = eu_rnpa)) +
  geom_line(size = 0.1)

m <- feols(log(fuel_consumption_l) ~ i(treated, event_time, 2013) + total_hp | eu_rnpa + event_time, three)
m2 <- feols(log(hours) ~ i(treated, event_time, 2013) + total_hp | eu_rnpa + event_time, three)

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


n_for_t <- function(start, t, data) {
  data %>% 
    filter(between(year, start, start + t)) %>% 
    count(eu_rnpa, left) %>% 
    filter(n == t + 1) %>% 
    pull(eu_rnpa) %>% 
    unique() %>% 
    length()
}

expand_grid(start = 2011:2013,
       t = 1:7) %>% 
  mutate(n = map2_dbl(start, t, n_for_t, data = three)) %>% 
  View()




# Naive regression on left vessels
agg_cons <- left_pred %>% 
  group_by(treated) %>% 
  summarize(fuel_consumption_l = mean(fuel_consumption_l)) %>% 
  ungroup()

right_agg_cons <- right_pred %>% 
  group_by(treated) %>% 
  summarize(fuel_consumption_l = mean(fuel_consumption_l)) %>% 
  ungroup()


agg_left_plot <- ggplot(left_pred, aes(x = fuel_consumption_l, fill = treated)) +
  geom_density(alpha = 0.5) +
  # geom_vline(data = agg_cons, aes(xintercept = fuel_consumption_l, color = treated)) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  lims(x = c(0, 1e6)) +
  labs(x = "Fuel consumption (L)",
       fill = "Subsidized",
       color = "Subsidized",
       subtitle = "Left of kink")

annual_left_plot <- ggplot(left_pred, aes(x = fuel_consumption_l, y = factor(year), fill = treated)) +
  geom_density_ridges(alpha = 0.5) +
  scale_fill_brewer(palette = "Set1") +
  lims(x = c(0, 1e6)) +
  labs(x = "Fuel consumption (L)",
       fill = "Subsidized",
       subtitle = "X-axis truncated at 1,000,000 L")

agg_right_plot <- ggplot(right_pred, aes(x = fuel_consumption_l, fill = treated)) +
  geom_density(alpha = 0.5) +
  # geom_vline(data = right_agg_cons, aes(xintercept = fuel_consumption_l, color = treated)) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  lims(x = c(0, 1e6)) +
  labs(x = "Fuel consumption (L)",
       fill = "Subsidized",
       color = "Subsidized",
       subtitle = "Right of kink")

annual_right_plot <- ggplot(right_pred, aes(x = fuel_consumption_l, y = factor(year), fill = treated)) +
  geom_density_ridges(alpha = 0.5) +
  scale_fill_brewer(palette = "Set1") +
  lims(x = c(0, 1e6)) +
  labs(x = "Fuel consumption (L)",
       fill = "Subsidized",
       subtitle = "X-axis truncated at 1,000,000 L")

p2 <- plot_grid(agg_left_plot, agg_right_plot, ncol = 1)


ggsave(plot = p2,
       filename = here("results", "img", "consumption_densities.pdf"),
       width = 8,
       height = 6)

##################################################################################################

## Histogram of number of times vessels are subsidized
## Identification comes from change in adjustment factor
n_sub_hist <- shrimp %>% filter(eu_rnpa == 305005159) %>% 
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


# For next week,s e need to run some new regressions. Specifically, re-run tables 1-4 by adding a new column that has only vessels subsidized at most 7 times.

# Left of kink
# Fuel consumption
lm1 <- feols(log(fuel_consumption_l) ~ treated + total_hp, left_pred)
lm2 <- feols(log(fuel_consumption_l) ~  treated + total_hp | eu_rnpa, left_pred)
lm3 <- feols(log(fuel_consumption_l) ~  treated + total_hp | eu_rnpa + year, left_pred)
lm4 <- feols(log(fuel_consumption_l) ~  treated + total_hp + predicted_subsidy_cap_l | eu_rnpa + year, left_pred)
lm5 <- feols(log(fuel_consumption_l) ~  treated + total_hp + dist | eu_rnpa + year, left_pred)
lm6 <- feols(log(fuel_consumption_l) ~  treated + total_hp | eu_rnpa + year, left_pred %>% filter(!eu_rnpa %in% unique(by_numbers$eu_rnpa)))

lmods <- list(lm1, lm2, lm3, lm4, lm5, lm6)
names(lmods) <- rep("log(L)", length(lmods))

modelsummary(lmods, 
             # output = here("results", "tab", "fuel_left.tex"),
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             statistic_vertical = T,
             title = "Left of kink, effect on fuel consumption. The last column excludes vessels that were always subsidized.",
             coef_rename = c("treatedTRUE" = "Subsidized",
                             "total_hp" = "Total Capacity (HP)",
                             "n_vessels" = "# Vessels",
                             "dist" = "Distance from kink",
                             "predicted_subsidy_cap_l" = "Predicted subsidy cap (L)"))

# Hours

h_lm1 <- feols(log(hours) ~ treated + total_hp, left_pred)
h_lm2 <- feols(log(hours) ~  treated + total_hp | eu_rnpa, left_pred)
h_lm3 <- feols(log(hours) ~  treated + total_hp | eu_rnpa + year, left_pred)
h_lm4 <- feols(log(hours) ~  treated + total_hp + predicted_subsidy_cap_l | eu_rnpa + year, left_pred)
h_lm5 <- feols(log(hours) ~  treated + total_hp + dist | eu_rnpa + year, left_pred)
h_lm6 <- feols(log(hours) ~  treated + total_hp | eu_rnpa + year, left_pred %>% filter(!eu_rnpa %in% unique(by_numbers$eu_rnpa)))


hlmods <- list(h_lm1, h_lm2, h_lm3, h_lm4, h_lm5, h_lm6)
names(hlmods) <- rep("log(H)", length(hlmods))

modelsummary(hlmods,
             output = here("results", "tab", "hours_left.tex"),
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             statistic_vertical = T,
             title = "Left of kink, effect on hours. The last column excludes vessels that were always subsidized.",
             coef_rename = c("treatedTRUE" = "Subsidized",
                             "total_hp" = "Total Capacity (HP)",
                             "n_vessels" = "# Vessels",
                             "dist" = "Distance from kink",
                             "predicted_subsidy_cap_l" = "Predicted subsidy cap (L)"))



# Models to the Right of the kink
rm1 <- feols(log(fuel_consumption_l) ~ treated + total_hp, right_pred)
rm2 <- feols(log(fuel_consumption_l) ~ treated + total_hp | eu_rnpa, right_pred)
rm3 <- feols(log(fuel_consumption_l) ~ treated + total_hp | eu_rnpa + year, right_pred)
rm4 <- feols(log(fuel_consumption_l) ~ treated + total_hp + predicted_subsidy_cap_l| eu_rnpa + year, right_pred)
rm5 <- feols(log(fuel_consumption_l) ~ treated + dist + total_hp  | eu_rnpa + year, right_pred)
rm6 <- feols(log(fuel_consumption_l) ~ treated + total_hp | eu_rnpa + year, right_pred %>% filter(!eu_rnpa %in% unique(by_numbers$eu_rnpa)))


# Make table
rmods <- list(rm1, rm2, rm3, rm4, rm5, rm6)
names(rmods) <- rep("log(L)", length(rmods))

modelsummary(rmods,
             output = here("results", "tab", "fuel_right.tex"),
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             statistic_vertical = T,
             title = "Right of kink, effect on fuel consumption. The last column excludes vessels that were always subsidized.",
             coef_rename = c("treatedTRUE" = "Subsidized",
                             "total_hp" = "Total Capacity (HP)",
                             "n_vessels" = "# Vessels",
                             "dist" = "Distance from kink (L)",
                             "predicted_subsidy_cap_l" = "Predicted subsidy cap (L)"))

h_rm1 <- feols(log(hours) ~ treated + total_hp, right_pred)
h_rm2 <- feols(log(hours) ~ treated + total_hp | eu_rnpa, right_pred)
h_rm3 <- feols(log(hours) ~ treated + total_hp | eu_rnpa + year, right_pred)
h_rm4 <- feols(log(hours) ~ treated + total_hp + predicted_subsidy_cap_l | eu_rnpa + year, right_pred)
h_rm5 <- feols(log(hours) ~ treated + dist + total_hp  | eu_rnpa + year, right_pred)
h_rm6 <- feols(log(hours) ~ treated + total_hp | eu_rnpa + year, right_pred %>% filter(!eu_rnpa %in% unique(by_numbers$eu_rnpa)))

# Make table
hrmods <- list(h_rm1, h_rm2, h_rm3, h_rm4, h_rm5, h_rm6)
names(hrmods) <- rep("log(H)", length(hrmods))

modelsummary(hrmods,
             output = here("results", "tab", "hours_right.tex"),
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             statistic_vertical = T,
             title = "Right of kink, effect on hours. The last column excludes vessels that were always subsidized.",
             coef_rename = c("treatedTRUE" = "Subsidized",
                             "total_hp" = "Total Capacity (HP)",
                             "n_vessels" = "# Vessels",
                             "dist" = "Distance from kink (L)",
                             "predicted_subsidy_cap_l" = "Predicted subsidy cap (L)"))




by_year <- function(data){
  feols(log(fuel_consumption_l) ~ treated + total_hp, data = data) %>% 
    broom::tidy()
}

l_by_year <- left_pred %>% 
  group_by(year) %>% 
  nest() %>% 
  mutate(m = map(data, by_year)) %>% 
  unnest(m) %>% 
  filter(term == "treatedTRUE") %>% 
  mutate(source = "left")

r_by_year <- right_pred %>% 
  group_by(year) %>% 
  nest() %>% 
  mutate(m = map(data, by_year)) %>% 
  unnest(m) %>% 
  filter(term == "treatedTRUE") %>% 
  mutate(source = "right")


d <- rbind(l_by_year, r_by_year)

b_in_time <- ggplot(d, aes(x = year, y = estimate, ymin = estimate - std.error, ymax = estimate + std.error, fill = source)) +
  geom_pointrange(color = "black", shape = 21, size = 1) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Year",
       y = bquote(beta[i]),
       fill = "Side") 

ggsave(plot = b_in_time,
       filename = here("results", "img", "b_in_time.pdf"),
       width = 6,
       height = 4)

# Models with binned distances
rm5 <- feols(log(fuel_consumption_l) ~ i(treated, log_dist_dummy, "15") | eu_rnpa + log_dist_dummy, right_pred)
rm5_full <- feols(log(fuel_consumption_l) ~ i(treated, log_dist_dummy, "15") + total_hp + n_vessels + predicted_subsidy_cap_l| eu_rnpa + log_dist_dummy, right_pred)

rm6 <- feols(log(fuel_consumption_l) ~ i(treated, rel_dist_dummy, "9+") | eu_rnpa + rel_dist_dummy, right_pred)
rm6_full <- feols(log(fuel_consumption_l) ~ i(treated, rel_dist_dummy, "9+") + total_hp + n_vessels + predicted_subsidy_cap_l| eu_rnpa + rel_dist_dummy, right_pred)

coefplot(list(rm5, rm5_full))
coefplot(list(rm6,rm6_full))

# Make table
rmods2 <- list(rm5, rm5_full, rm6, rm6_full)
names(rmods2) <- rep("log(L)", length(rmods2))

modelsummary(rmods2,
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             statistic_vertical = F,
             coef_rename = c("treatedTRUE" = "Subsidized",
                             "total_hp" = "Total Capacity (HP)",
                             "n_vessels" = "# Vessels"))




# NOW WITH ALL OF THE DATA



###########
#L/R S/U plot

full_shrimp <- shrimp %>% 
  mutate(left = 1 * ((treated & fuel_consumption_l <= predicted_subsidy_cap_l) | (!treated & fuel_consumption_l <= predicted_subsidy_cap_l)),
         right = 1 - left,
         treated = 1 * treated)

# Olivier Deschenes  17:23
# Table 1 & 2 look great, thanks!  For Table 3, let me propose the following model:
# y = a + b1*SUB*LEFT + b2*SUB*RIGHT (plus the same FEs and controls)

cm1 <- feols(log(fuel_consumption_l) ~ treated:left + treated:right, full_shrimp)
cm2 <- feols(log(fuel_consumption_l) ~ treated:left + treated:right | eu_rnpa , full_shrimp)
cm3 <- feols(log(fuel_consumption_l) ~ treated:left + treated:right | eu_rnpa + year, full_shrimp)
cm4 <- feols(log(fuel_consumption_l) ~ treated:left + treated:right + total_hp | eu_rnpa + year, full_shrimp)
cm5 <- feols(log(fuel_consumption_l) ~ treated:left + treated:right + total_hp + predicted_subsidy_cap_l | eu_rnpa + year, full_shrimp)
cm6 <- feols(log(fuel_consumption_l) ~ treated:left + treated:right | eu_rnpa + year, full_shrimp %>% filter(!eu_rnpa %in% unique(by_numbers$eu_rnpa)))

cm <- list(cm1, cm2, cm3, cm4, cm5, cm6)
names(cm) <- rep("log(L)", length(cm))

modelsummary(cm,
             output = here("results", "tab", "fuel_all.tex"),
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             statistic_vertical = T,
             title = "Effect on fuel consumption by all economic units. The last column excludes vessels that were always subsidized.",
             coef_rename = c("total_hp" = "Total Capacity (HP)",
                             "dist" = "Distance from kink",
                             "predicted_subsidy_cap_l" = "Predicted subsidy cap (L)"))

# Now with hours

h_cm1 <- feols(log(fuel_consumption_l) ~ treated:left + treated:right, full_shrimp)
h_cm2 <- feols(log(fuel_consumption_l) ~ treated:left + treated:right | eu_rnpa , full_shrimp)
h_cm3 <- feols(log(fuel_consumption_l) ~ treated:left + treated:right | eu_rnpa + year, full_shrimp)
h_cm4 <- feols(log(fuel_consumption_l) ~ treated:left + treated:right + total_hp | eu_rnpa + year, full_shrimp)
h_cm5 <- feols(log(fuel_consumption_l) ~ treated:left + treated:right + total_hp + predicted_subsidy_cap_l | eu_rnpa + year, full_shrimp)
h_cm6 <- feols(log(fuel_consumption_l) ~ treated:left + treated:right | eu_rnpa + year, full_shrimp %>% filter(!eu_rnpa %in% unique(by_numbers$eu_rnpa)))

h_cm <- list(h_cm1, h_cm2, h_cm3, h_cm4, h_cm5, h_cm6)
names(h_cm) <- rep("log(Hours)", length(h_cm))

modelsummary(h_cm,
             output = here("results", "tab", "hours_all.tex"),
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             statistic_vertical = T,
             title = "Effect on fuel consumption by all economic units. The last column excludes vessels that were always subsidized.",
             coef_rename = c("total_hp" = "Total Capacity (HP)",
                             "dist" = "Distance from kink",
                             "predicted_subsidy_cap_l" = "Predicted subsidy cap (L)"))


## TESTING ITO USING VESSELS THAT ARE ALWAYS SUBSIDIZED

a <- by_numbers %>% 
  mutate(r = fuel_consumption_l > subsidy_cap_l) %>% 
  group_by(eu_rnpa, r) %>% 
  mutate(n = n()) %>% 
  filter(n < 6)

ito1 <- feols(log(fuel_consumption_l) ~ r + log(p) | eu_rnpa, data = a)
ito2 <- feols(log(fuel_consumption_l) ~ dist | eu_rnpa + year , data = by_numbers)
ito3 <- feols(log(fuel_consumption_l) ~ dist + total_hp | eu_rnpa + year, data = by_numbers)
ito4 <- feols(log(fuel_consumption_l) ~ dist + total_hp + subsidy_cap_l | eu_rnpa + year, data = by_numbers)

# Make table
itomods <- list(ito1,
                ito2,
                ito3,
                ito4)

names(itomods) <- rep("log(L)", length(itomods))

modelsummary(itomods,
             # output = here("results", "tab", "always_subsidized.tex"),
             title = "Demand elasticity of fuel using the 177 economic units that are always subsidized.",
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             statistic_vertical = T,
             coef_rename = c("log(p)" = "log(price)",
                             "subsidy_cap_l" = "Subsidy Cap (L)",
                             "total_hp" = "Total Capacity (HP)",
                             "dist" = "Distance from kink (L)"))


nevers <- shrimp %>% 
  group_by(eu_rnpa) %>% 
  mutate(n_subs = sum(treated),
         n_obs = n()) %>% 
  ungroup() %>% 
  filter(!treated,
         n_subs == 0)

feols(log(fuel_consumption_l) ~ log(p) | eu_rnpa, data = nevers)
