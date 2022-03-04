
library(fixest)
library(modelsummary)
library(cowplo)
library(tidyverse)


shrimp <- read_csv(
  file = file.path(
  project_path, "data", "processed_data", "imputed_subsidy_economic_unit_annual_shrimp_panel.csv")) %>% 
  filter(year <= 2019,
         year > 2011)


# left of predicted
left_pred <- shrimp %>% 
  filter(fuel_consumption_l <= predicted_subsidy_cap_l)

right_pred <- shrimp %>% 
  filter(fuel_consumption_l > predicted_subsidy_cap_l)

unsubs <- shrimp %>% 
  filter(!treated, fuel_consumption_l <= predicted_subsidy_cap_l)

shrimp %>% count(year, treated) %>% spread(treated, n) %>% mutate(n = `TRUE` + `FALSE`)
left_pred %>% count(year, treated) %>% spread(treated, n) %>% mutate(n = `TRUE` + `FALSE`)
right_pred %>% count(year, treated) %>% spread(treated, n) %>% mutate(n = `TRUE` + `FALSE`)
unsubs %>% count(year, treated) %>% spread(treated, n)

three <- shrimp %>% 
  mutate(left = fuel_consumption_l < predicted_subsidy_cap_l) %>% 
  filter(left,
         year %in% seq(2012, 2017)) %>% 
  mutate(event_time = factor(year - 2014)) 

ggplot(three, aes(x = event_time, y = fuel_consumption_l, fill = treated, color = treated, linetype = left)) + 
  stat_summary(geom = "point", fun = "mean") +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0) +
  scale_color_brewer(palette = "Set1")

feols(fuel_consumption_l ~ i(treated, event_time) | eu_rnpa + event_time, three) %>% 
  coefplot(style = "interaction")
  

feols(fuel_consumption_l ~ treated * event_time + p + total_hp + subsidy_cap_l |  eu_rnpa , three) %>% 
  broom::tidy() %>% 
  filter(str_detect(term, ":")) %>% 
  mutate(term = str_remove(term, pattern = "treatedTRUE:event_time")) %>% 
  ggplot(aes(x = term, y = estimate)) +
  geom_point() +
  geom_hline(yintercept = 0)




# 2-peso subsidy as a percent of that year's price
a <- left_pred %>% 
  select(year, ph, pl) %>% 
  distinct() %>% 
  mutate(p_prop = ((ph - pl) / ph))

b <- shrimp %>% 
  group_by(year) %>% 
  summarize(tot = sum(subsidy_cap_l) / 1e3) %>% 
  ungroup()

ggplot(a, aes(x = year, y = p_prop)) +
  geom_col() +
  scale_x_continuous(labels = seq(2011, 2019, by = 2), breaks = seq(2011, 2019, by = 2)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Year", y = "Price subsidy (% of market price)")

ggplot(b, aes(x = year, y = tot)) +
  stat_summary(geom = "col", fun = sum, color = "black") +
  scale_x_continuous(labels = seq(2011, 2019, by = 2), breaks = seq(2011, 2019, by = 2)) +
  labs(x = "Year",
       y = "Total subsidized liters")

# Naive regression on left vessels
agg_cons <- left_pred %>% 
  group_by(treated) %>% 
  summarize(fuel_consumption_l = mean(fuel_consumption_l)) %>% 
  ungroup()

agg_left_plot <- ggplot(left_pred, aes(x = fuel_consumption_l, fill = treated)) +
  geom_density(alpha = 0.5) +
  # geom_vline(data = agg_cons, aes(xintercept = fuel_consumption_l, color = treated)) +
  scale_fill_brewer(palette = "Set1") +
  # scale_color_brewer(palette = "Set1") +
  lims(x = c(0, 1e6)) +
  labs(x = "Fuel consumption (L)",
       fill = "Subsidized",
       subtitle = "X-axis truncated at 1,000,000 L")

annual_left_plot <- ggplot(left_pred, aes(x = fuel_consumption_l, y = factor(year), fill = treated)) +
  geom_density_ridges(alpha = 0.5) +
  scale_fill_brewer(palette = "Set1") +
  lims(x = c(0, 1e6)) +
  labs(x = "Fuel consumption (L)",
       fill = "Subsidized",
       subtitle = "X-axis truncated at 1,000,000 L")


plot_grid(agg_left_plot, annual_left_plot, ncol = 1)

# What is the price elasticity of demand?
pm1 <- feols(log(fuel_consumption_l) ~ log(p) + total_hp, unsubs)
pm2 <- feols(log(fuel_consumption_l) ~ log(p) + total_hp | eu_rnpa, unsubs)

pmods <- list("log(Fuel Cosnumption)" = pm1,
              "log(Fuel Cosnumption)" = pm2)

modelsummary(pmods,
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             statistic_vertical = F,
             coef_rename = c("log(p)" = "log(Fuel Price)",
                             "total_hp" = "Total Capacity (HP)",
                             "n_vessels" = "# Vessels"))

# Left of kink
lm1 <- feols(log(fuel_consumption_l) ~ treated + total_hp + predicted_subsidy_cap_l, left_pred)
lm2 <- feols(log(fuel_consumption_l) ~  treated + total_hp + predicted_subsidy_cap_l| eu_rnpa, left_pred)

lmods <- list(lm1, lm2)
names(lmods) <- rep("log(Fuel Consumption)", 2)

modelsummary(lmods,
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             statistic_vertical = F,
             coef_rename = c("treatedTRUE" = "Subsidized",
                             "total_hp" = "Total Capacity (HP)",
                             "n_vessels" = "# Vessels"))


# Right
rm1 <- feols(log(fuel_consumption_l) ~ treated + total_hp + predicted_subsidy_cap_l, right_pred)
rm2 <- feols(log(fuel_consumption_l) ~ treated + total_hp + predicted_subsidy_cap_l | eu_rnpa, right_pred)


rmods <- list(rm1, rm2)
names(rmods) <- rep("log(Fuel Consumption)", 2)

modelsummary(rmods,
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             statistic_vertical = F,
             coef_rename = c("treatedTRUE" = "Subsidized",
                             "total_hp" = "Total Capacity (HP)",
                             "n_vessels" = "# Vessels"))










# NOW ITH ALL OF THE DATA



###########
#L/R S/U plot

full_shrimp <- shrimp %>% 
  mutate(left = case_when(treated & fuel_consumption_l <= subsidy_cap_l ~ "Left",
                          treated & fuel_consumption_l > subsidy_cap_l ~ "Right",
                          !treated & fuel_consumption_l <= predicted_subsidy_cap_l ~ "Left",
                          !treated & fuel_consumption_l > predicted_subsidy_cap_l ~ "Right"),
         sub = ifelse(treated, "Subsidized", "Not subsidized"),
         group = paste(left, sub, sep = "-"),
         group = fct_relevel(group,
                             "Left-Not subsidized",
                             "Right-Not subsidized",
                             "Left-Subsidized",
                             "Right-Subsidized"),
         l = left == "Left")

ggplot(data = full_shrimp,
       mapping = aes(x = fuel_consumption_l, fill = sub)) +
  geom_density(alpha = 0.5)+ 
  facet_wrap(~left, ncol = 1) +
  scale_fill_brewer(palette = "Set1") +
  lims(x = c(0, 1e6)) +
  labs(x = "Fuel consumption (L)",
       fill = "Subsidized",
       subtitle = "X-axis truncated at 1000,000 L")


feols(fuel_consumption_l ~ treated + l | eu_rnpa + year, full_shrimp)














