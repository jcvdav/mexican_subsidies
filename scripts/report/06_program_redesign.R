library(startR)
library(tidyverse)

source(here::here("scripts", "00_setup.R"))

panel <- read_csv(file.path(project_path, "data", "processed_data", "estimation_panel.csv"), show_col_types = F) %>% 
  filter(year == 2019) %>% 
  filter(phi < 10) %>%
  select(eu_rnpa, species, total_hp, fuel_consumption_l, treated, subsidy_cap_l, ph)

model <- readRDS(file.path(project_path, "data", "output_data", "model.rds"))

response <- model$coefficients

pcts <- seq(0, 2, by = 0.25)

sim_panel <- panel %>% 
  expand_grid(alpha = c(0, 1),
              pct_p = pcts,
              pct_q = pcts) %>%
  mutate(resp = ifelse(alpha == 1, "Marginal", "Average"),
         pl = ph - (pct_p * 2),
         subsidy_cap_l = pct_q * subsidy_cap_l,
         phi = subsidy_cap_l / fuel_consumption_l,
         D = phi <= 1L,
         R = ph - pl,
         act_price = (fuel_consumption_l<=subsidy_cap_l)*(treated)*pl + 
           (fuel_consumption_l<=subsidy_cap_l)*(!treated)*ph +
           (fuel_consumption_l>subsidy_cap_l)*ph,
         eu_rnpa = factor(eu_rnpa),
         pp = ph - ((1 - alpha) * (ph - pl) * phi),
         p = case_when(!treated ~ ph,
                       treated & !D ~ pl,
                       treated & D ~ pp),
         R = ph - p,
         q_counter = pmax((fuel_consumption_l + (treated * response * 2)), 0),
         q_redesign = pmax((fuel_consumption_l + (treated * response * R)), 0),
         q_additonal = q_redesign - q_counter,
         subsidized_fuel_l = pmin(q_counter, subsidy_cap_l),
         govt_outlay = subsidized_fuel_l * R) 

summarized_simulations <- sim_panel %>% 
  group_by(resp, pct_p, pct_q) %>% 
  summarize(fuel_consumption_l = sum(fuel_consumption_l),
            q_counter = sum(q_counter),
            q_redesign = sum(q_redesign),
            q_additonal = sum(q_additonal),
            subsidized_fuel_l = sum(subsidized_fuel_l),
            govt_outlay = sum(govt_outlay)) %>% 
  mutate(pct_additional = q_additonal / q_counter)


ggplot(summarized_simulations, aes(x = pct_q, y = pct_p, size = pct_additional, fill = q_additonal / 1e6, z = govt_outlay)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_contour(show.legend = F, color = "black") +
  # geom_text_contour() +
  geom_point(alpha = 0.9) +
  scale_fill_viridis_c() +
  scale_size_continuous(labels = scales::percent) +
  facet_wrap(~resp, ncol = 1) +
  labs(x = bquote("%"~Delta~bar(q)),
       y = bquote("%"~Delta~bar(p))) +
  guides(fill = guide_colorbar("Additional fuel\nconsumption (L)"),
         size = guide_legend("% Additional fuel\nconsumption")) +
  theme(strip.text.x=element_text(margin=margin(b = 2)))


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
  


    