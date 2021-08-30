library(startR)
library(tidyverse)

source(here::here("scripts", "00_setup.R"))

model <- readRDS(file.path(project_path, "data", "output_data", "model.rds"))

response <- model$coefficients

panel <- read.csv(file.path(project_path, "data", "processed_data", "estimation_panel.csv")) %>% 
  mutate(act_price = (fuel_consumption_l<=subsidy_cap_l)*(treated)*pl + 
           (fuel_consumption_l<=subsidy_cap_l)*(!treated)*ph +
           (fuel_consumption_l>subsidy_cap_l)*ph,
         eu_rnpa = factor(eu_rnpa),
         year = year) %>% 
  drop_na(mean_price) %>% 
  filter(phi < 10)

sim_panel <- panel %>% 
  expand_grid(alpha = c(0, 1)) %>% 
  mutate(pp = ph - ((1 - alpha) * (ph - pl) * phi),
         p = case_when(!treated ~ ph,
                       treated & !D ~ pl,
                       treated & D ~ pp),
         R = ph - p,
         q_counter = pmax((fuel_consumption_l + (treated * response * R)), 0),
         overfishing = fuel_consumption_l - q_counter,
         pct = overfishing / fuel_consumption_l) %>% 
  filter(fuel_consumption_l > 0) %>% 
  select(year, eu_rnpa, species, fuel_consumption_l, hours, subsidy_cap_l, alpha, act_price, p, pp, q_counter, overfishing, pct)

sim_panel2 <- sim_panel %>% 
  group_by(year, alpha) %>% 
  summarize(overfishing = sum(overfishing, na.rm = T),
            tot_fuel_cons_l = sum(fuel_consumption_l, na.rm = T),
            tot_fuel_cap_l = sum(subsidy_cap_l, na.rm = T),
            pct_tot_con = overfishing / tot_fuel_cons_l,
            pct_tot_cap = overfishing / tot_fuel_cap_l,
            pct_tot_con_subs = overfishing / (sum(fuel_consumption_l * treated))) %>% 
  mutate(response = ifelse(alpha == 1, "Marginal", "Average"))

sim_panel3 <- sim_panel %>% 
  filter(!D | phi == 0,
         alpha == 1) %>% 
  group_by(year) %>% 
  summarize(overfishing = sum(overfishing, na.rm = T),
            tot_fuel_cons_l = sum(fuel_consumption_l, na.rm = T),
            tot_fuel_cap_l = sum(subsidy_cap_l, na.rm = T),
            pct_tot_con = overfishing / tot_fuel_cons_l,
            pct_tot_cap = overfishing / tot_fuel_cap_l,
            pct_tot_con_subs = overfishing / (sum(fuel_consumption_l * treated)))



# Export data

saveRDS(sim_panel, file.path(project_path, "data", "output_data", "simulated_counterfactuals.rds"))



# Totals
abs <- ggplot(sim_panel2, aes(x = year, y = overfishing / 1e6)) + 
  geom_line(aes(linetype = response)) +
  geom_point(aes(size = tot_fuel_cap_l / 1e6), fill = "black") +
  # geom_line(data = sim_panel3, linetype = "dashed") +
  labs(x = "Year",
       y = "Additional fishing effort (Million L)") +
  guides(linetype = guide_legend(title = "Response"),
         size = guide_legend(title = "Total fuel\ncap (Million L)")) +
  scale_x_continuous(labels = 2011:2019, breaks = 2011:2019)

rel <- ggplot(sim_panel2, aes(x = year, y = pct_tot_con)) + 
  geom_line(aes(linetype = response)) +
  geom_point(aes(size = tot_fuel_cap_l/1e6), fill = "black") +
  # geom_line(data = sim_panel3, linetype = "dashed") +
  labs(x = "Year",
       y = "Relative overfishing\n(Aditional / Counterfactual)") +
  guides(linetype = guide_legend(title = "Response"),
         size = guide_legend(title = "Total fuel\ncap (Million L)")) +
  scale_x_continuous(labels = 2011:2019, breaks = 2011:2019) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 0.03, by = 0.005), limits = c(0, 0.03)) 

rel2 <- ggplot(sim_panel2, aes(x = year, y = pct_tot_cap)) + 
  geom_line(aes(linetype = response)) +
  geom_point(aes(size = tot_fuel_cap_l/1e6), fill = "black") +
  # geom_line(data = sim_panel3, linetype = "dashed") +
  labs(x = "Year",
       y = "Relative overfishing\n(Additional / Total subsidized liters)") +
  guides(linetype = guide_legend(title = "Response"),
         size = guide_legend(title = "Total fuel\ncap (Million L)")) +
  scale_x_continuous(labels = 2011:2019, breaks = 2011:2019) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 0.03, by = 0.005), limits = c(0, 0.03)) 

rel3 <- ggplot(sim_panel2, aes(x = year, y = pct_tot_con_subs)) + 
  geom_line(aes(linetype = response)) +
  geom_point(aes(size = tot_fuel_cap_l/1e6), fill = "black") +
  # geom_line(data = sim_panel3, linetype = "dashed") +
  labs(x = "Year",
       y = "Relative overfishing\n(Additional / Total subsidized liters)") +
  guides(color = guide_colorbar(title = bquote("Weight on\nmarginal"(alpha)),
                                frame.colour = "black",
                                ticks.colour = "black"),
         size = guide_legend(title = "Total fuel\ncap (L)")) +
  labs(subtitle = "Only for subsidized vessels") +
  scale_x_continuous(labels = 2011:2019, breaks = 2011:2019) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 0.03, by = 0.005), limits = c(0, 0.03)) 


ggsave(plot = abs,
       filename = file.path(project_path, "results", "figures", "overfishing_by_year.png"),
       width = 6,
       height = 3,
       units = "in")

ggsave(plot = rel,
       filename = file.path(project_path, "results", "figures", "relative_overfishing_by_year.png"),
       width = 6,
       height = 3,
       units = "in")

ggsave(plot = rel2,
       filename = file.path(project_path, "results", "figures", "relative_overfishing_by_year_caps.png"),
       width = 6,
       height = 3,
       units = "in")

ggsave(plot = rel3,
       filename = file.path(project_path, "results", "figures", "relative_overfishing_by_year_subs.png"),
       width = 6,
       height = 3,
       units = "in")



