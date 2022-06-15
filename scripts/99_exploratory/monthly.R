
library(ggridges)
library(tidyverse)


panel <- read_csv(file.path(project_path, "data", "processed_data", "monthly_estimation_panel.csv")) %>% 
  filter(shrimp, year <= 2019)

# Mean Q-bar
panel %>% 
  select(year, eu_rnpa, subsidy_cap_l) %>% 
  distinct() %>% 
  group_by(year) %>% 
  summarize(tot_cap = sum(subsidy_cap_l, na.rm = T) / 1e6) %>% 
  ggplot(aes(x = year, y = tot_cap)) + 
  geom_line(data = d2, aes(y = total)) +
  geom_line() +
  geom_point(size = 3) +
  labs(x = "Year", y = "Annual subsidized diesel (Million liters)") +
  scale_x_continuous(breaks = 2011:2019, labels = 2011:2019)

panel %>% 
  filter(month == 12,
         subsidy_cap_l > 0) %>% 
  select(year, eu_rnpa, subsidy_cap_l, fuel_consumption_to_date_l) %>% 
  # mutate(a = subsidy_cap_l / fuel_consumption_to_date_l)
  ggplot(aes(x = subsidy_cap_l / fuel_consumption_to_date_l)) +
  geom_histogram() +
  facet_wrap(~year, scales = "free")

# Month at which cap is exceeded
panel %>% 
  filter(subsidy_cap_l > 0, exceeds_cap) %>% 
  group_by(year, eu_rnpa) %>% 
  slice_min(n = 1, month) %>%
  ggplot(aes(x = month)) +
  geom_histogram(bins = 12) +
  facet_wrap(~year, scales = "free_y") +
  labs(x = "Month", y = "# of Economic Units") +
  scale_x_continuous(labels = c(1:12), breaks = c(1:12))

# Fraction of economic units that have exceeded cap per month
panel %>% 
  select(year, month, date, fuel_consumption_to_date_l, subsidy_cap_l, eu_rnpa, exceeds_cap) %>% 
  group_by(year) %>% 
  mutate(n = n_distinct(eu_rnpa)) %>% 
  group_by(year, month, n) %>% 
  summarize(a = sum(exceeds_cap, na.rm = T)) %>% 
  ggplot(aes(x = month, y = a / n, group = year)) + 
  geom_point(aes(fill = year)) +
  geom_line() +
  facet_wrap(~year) +
  labs(x = "Month", y = "% Economic units with Q > Q-bar") +
  scale_x_continuous(labels = c(1:12), breaks = c(1:12)) +
  scale_y_continuous(labels = scales::percent)

panel %>% 
  select(year, month, date, fuel_consumption_to_date_l, subsidy_cap_l, eu_rnpa, exceeds_cap) %>% 
  group_by(year) %>% 
  mutate(n = n_distinct(eu_rnpa)) %>% 
  group_by(year, month, n) %>% 
  summarize(a = sum(exceeds_cap, na.rm = T)) %>% 
  ggplot(aes(x = month, y = a / n, color = year, group = year)) + 
  geom_line() +
  labs(x = "Month", y = "% Economic units with Q > Q-bar") +
  scale_x_continuous(labels = c(1:12), breaks = c(1:12)) +
  scale_y_continuous(labels = scales::percent)


# Do they consume more before than after the kink?
panel %>% 
  filter(subsidy_cap_l > 0) %>% 
  ggplot(aes(x = fuel_consumption_to_date_l / subsidy_cap_l, y = fuel_consumption_l), group = month) +
  geom_smooth(method = "lm") +
  facet_wrap(~year, scales = "free_y") +
  scale_x_continuous(limits = c(0, 2)) +
  labs(x = "Monthly fuel consumption (relative to kink location)")

panel %>% 
  filter(subsidy_cap_l > 0) %>% 
  ggplot(aes(x = exceeds_cap, y = fuel_consumption_l)) +
  geom_boxplot()

ggplot(data = panel, # %>% 
         # filter(eu_rnpa == 203004411),
       aes(x = date, y = fuel_consumption_to_date_l / subsidy_cap_l, fill = subsidy_cap_l)) +
  geom_point() + 
  geom_hline(yintercept = 1, linetype = "dashed")

fixest::feols(fml = fuel_consumption_l ~ exceeds_cap | eu_rnpa + year, data = panel %>% filter(subsidy_cap_l > 0)) %>% 
  summary()


ggplot(panel, aes(x = date, y = fuel_consumption_l)) +
  stat_summary(geom = "point", fun = "sum", na.rm = T)

ggplot(panel, aes(x = date, y = fuel_consumption_l)) +
  stat_summary(geom = "point", fun = "mean", na.rm = T) +
  stat_summary(geom = "line", fun = "mean", na.rm = T)

panel %>% 
  select(year, eu_rnpa, subsidy_cap_l) %>% 
  distinct() %>% 
  ggplot(mapping = aes(x = year, y = subsidy_cap_l)) +
  stat_summary(geom = "point", fun = "sum", na.rm = T) +
  stat_summary(geom = "line", fun = "sum", na.rm = T)


panel %>% 
  filter(subsidy_cap_l > 0) %>% 
  ggplot(aes(x = date, y = fuel_consumption_to_date_l / subsidy_cap_l)) + 
  geom_line() +
  geom_point(aes(fill = (fuel_consumption_to_date_l / subsidy_cap_l) >= 1))



panel %>% 
  group_by(year) %>% 
  summarize(a = n_distinct(eu_rnpa)) %>% 
  ggplot(aes(x = year, y = a)) +
  geom_point()

d <- count(panel, eu_rnpa) %>% 
  arrange(desc(n)) 

n_at_least <- function(n, counts) {
  sum(counts$n >= n) / dim(counts)[1]
}

tibble(n = 1:108) %>%
  mutate(pct = map_dbl(n, n_at_least, d)) %>% 
  ggplot(aes(x = n, y = pct)) +
  geom_point() +
  labs(x = "Months with observations", y = "Fraction of all vessels))")
  

panel %>% 
  # filter(year == 2014,
         # month %in% c(6:8)) %>% 
  group_by(year, month) %>% 
  summarize(b = sum(fuel_consumption_to_date_l)) %>% 
  ggplot(aes(x = month, y = b)) +
  geom_point() +
  facet_wrap(~year)



















# COMPLETE

missing <- panel %>% 
  filter(year <= 2014,
         month %in% c(6:8)) %>% 
  select(year, month, date, fuel_consumption_to_date_l, subsidy_cap_l, eu_rnpa)

full <- missing %>% 
  group_by(year) %>% 
  complete(nesting(eu_rnpa), month, fill = list(fuel_consumption_l = 0))

dim(missing)
dim(full)

# Test

panel %>% 
  group_by(year, month) %>% 
  summarize(n = n_distinct(eu_rnpa)) %>% 
  ggplot(aes(x = month, y = n)) +
  geom_point() +
  facet_wrap(~year)
