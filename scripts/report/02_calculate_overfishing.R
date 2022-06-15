library(startR)
library(cowplot)
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
  drop_na(mean_price) 

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
  select(year, eu_rnpa, species, fuel_consumption_l, hours, subsidy_cap_l, alpha, act_price, p, pp, q_counter, overfishing, pct, treated)

sim_panel2 <- sim_panel %>% 
  group_by(year, alpha) %>% 
  summarize(overfishing = sum(overfishing, na.rm = T),
            tot_fuel_cons_l = sum(fuel_consumption_l, na.rm = T),
            tot_fuel_cap_l = sum(subsidy_cap_l, na.rm = T),
            pct_tot_con = overfishing / tot_fuel_cons_l,
            pct_tot_cap = overfishing / tot_fuel_cap_l) %>% 
  mutate(response = ifelse(alpha == 1, "Marginal", "Average"))


# Export data

saveRDS(sim_panel, file.path(project_path, "data", "output_data", "simulated_counterfactuals.rds"))



# Totals
abs <- ggplot(sim_panel2, aes(x = year, y = overfishing / 1e3, group = response)) + 
  geom_line(aes(linetype = response)) +
  geom_point(aes(size = tot_fuel_cap_l / 1e6), fill = "black") +
  # geom_line(data = sim_panel3, linetype = "dashed") +
  labs(x = "Year",
       y = "Additional fishing effort\n(Thousand L)") +
  guides(linetype = guide_legend(title = "Response"),
         size = guide_legend(title = "Total fuel\ncap (Million L)")) +
  scale_x_continuous(labels = seq(2011, 2019, by = 2), breaks = seq(2011, 2019, by = 2)) +
  theme(legend.position = "None")

rel <- ggplot(sim_panel2, aes(x = year, y = pct_tot_con)) + 
  geom_line(aes(linetype = response)) +
  geom_point(aes(size = tot_fuel_cap_l/1e6), fill = "black") +
  # geom_line(data = sim_panel3, linetype = "dashed") +
  labs(x = "Year",
       y = "% Change in effort\n") +
  guides(linetype = guide_legend(title = "Response"),
         size = guide_legend(title = "Total fuel\ncap (Million L)")) +
  scale_x_continuous(labels = seq(2011, 2019, by = 2), breaks = seq(2011, 2019, by = 2)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), breaks = seq(0, 0.03, by = 0.005), limits = c(0, 0.03)) +
  theme(legend.justification = c(1, 1),
        legend.position = c(1, 1))


addtl_effort <- plot_grid(abs, rel, ncol = 1)

ggsave(plot = addtl_effort,
       filename = file.path(project_path, "results", "figures", "annual_additional_effort.png"),
       height = 8, width = 6)



