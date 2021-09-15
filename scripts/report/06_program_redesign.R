library(startR)
library(tidyverse)

source(here::here("scripts", "00_setup.R"))

model <- readRDS(file.path(project_path, "data", "output_data", "model.rds"))

response <- model$coefficients

panel <- read_csv(file.path(project_path, "data", "processed_data", "estimation_panel.csv"), show_col_types = F) %>% 
  filter(year == 2019) %>% 
  filter(phi < 10) %>%
  mutate(eu_rnpa = factor(eu_rnpa)) %>% 
  # filter(eu_rnpa %in% c("203000922", "203004411")) %>%
  select(eu_rnpa, species, total_hp, fuel_consumption_l, treated, subsidy_cap_l, ph, pl, phi, D)

pcts <- seq(0, 2, by = 0.25)

sim_panel <- panel %>% 
  expand_grid(alpha = c(1),
              pct_p = pcts,
              pct_q = pcts) %>%
  mutate(resp = ifelse(alpha == 1, "Marginal", "Average")) %>% 
  mutate(price_subsidy = -2 * pct_p,
         subsidy_cap_l = pct_q * subsidy_cap_l,
         p_redesign = ph + price_subsidy,
         pp = ph - ((1 - alpha) * (ph - pl) * phi),
         p = case_when(!treated ~ ph,
                       treated & !D ~ pl,
                       treated & D ~ pp),
         R = ph - p,
         q_counter = pmax((fuel_consumption_l + (treated * response * R)), 0),
         q_redesign = pmax((q_counter + (treated * response * price_subsidy)), 0),
         q_redesign = (ifelse(q_redesign > subsidy_cap_l, q_counter, q_redesign)), 
         q_additional = q_redesign - q_counter,
         subsidized_fuel_l = pmin(q_counter, subsidy_cap_l),
         govt_outlay = -price_subsidy * subsidized_fuel_l)


# subsidy_cap_l = pct_q * subsidy_cap_l,
# price_subsidy = -pct_p * 2,
#          pl = ph - price_subsidy,
#          q_redesign = pmax((q_counter + (treated * response * price_subsidy)), 0),
#          phi = subsidy_cap_l / q_counter,
#          D = phi <= 1L,
#          pp = ph - ((1 - alpha) * (ph - pl) * phi),
#          p = case_when(!treated ~ ph,
#                        treated & !D ~ pl,
#                        treated & D ~ pp),
#          R = ph - p,
#          # q_redesign = pmax((q_counter + (treated * response * R)), 0),
#          q_additonal = q_redesign - q_counter,
#          subsidized_fuel_l = pmin(q_counter, subsidy_cap_l),
#          govt_outlay = subsidized_fuel_l * R) 




# ###
# 
# sim_panel <- panel %>% 
#   expand_grid(alpha = c(0, 1)) %>% 
#   mutate(pp = ph - ((1 - alpha) * (ph - pl) * phi),
#          p = case_when(!treated ~ ph,
#                        treated & !D ~ pl,
#                        treated & D ~ pp),
#          R = ph - p,
#          q_counter = pmax((fuel_consumption_l + (treated * response * R)), 0),
#          overfishing = fuel_consumption_l - q_counter,
#          pct = overfishing / fuel_consumption_l) %>% 
#   filter(fuel_consumption_l > 0) %>% 
#   select(year, eu_rnpa, species, fuel_consumption_l, hours, subsidy_cap_l, alpha, act_price, p, pp, q_counter, overfishing, pct)
# 
# 
# ###

summarized_simulations <- sim_panel %>% 
  group_by(resp, pct_p, pct_q) %>% 
  summarize(fuel_consumption_l = sum(fuel_consumption_l),
            q_counter = sum(q_counter),
            q_redesign = sum(q_redesign),
            q_additional = sum(q_additional),
            subsidized_fuel_l = sum(subsidized_fuel_l),
            govt_outlay = sum(govt_outlay)) %>% 
  mutate(pct_additional = q_additional / q_counter)


p <- ggplot(summarized_simulations, aes(x = pct_q, y = pct_p, fill = pct_additional, size = govt_outlay / 1e6, z = govt_outlay)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_contour(show.legend = F, color = "black") +
  # geom_text_contour() +
  geom_point(alpha = 0.9) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_c(labels = scales::percent, limits = c(0, 0.02)) +
  # scale_fill_continuous(labels = scales::percent) +
  facet_wrap(~resp, ncol = 1) +
  labs(x = bquote("%"~bar(q)),
       y = bquote("%"~bar(p))) +
  guides(size = guide_legend("Cost of program\n(Million MXP)"),
         fill = guide_colorbar("% Additional fuel\nconsumption")) +
  theme(strip.text.x=element_text(margin=margin(b = 2)))

ggsave(p, filename = file.path(project_path, "results", "figures", "program_redesign.png"), 
       width = 8,
       height = 6)


summarized_simulations %>% 
  select(pct_p, pct_q, resp, additional_l) %>% 
  spread(resp, additional_l) %>% 
  mutate(dif = Average - Marginal) %>% 
  ggplot(aes(x = pct_q, y = pct_p, fill = dif / 1e6)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_point(alpha = 0.9, size = 5) +
  scale_fill_viridis_c() +
  labs(x = bquote("%"~Delta~bar(q)),
       y = bquote("%"~Delta~bar(p))) +
  guides(fill = guide_colorbar("Differente in additional\nfuel consumption\n(Average - Marginal; L)")) +
  theme(strip.text.x=element_text(margin=margin(b = 2)))
  


    