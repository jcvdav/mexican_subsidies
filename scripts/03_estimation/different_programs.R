################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
library(tidyverse)

# Load data --------------------------------------------------------------------
data <- readRDS(file.path(project_path, "data", "processed_data", "shrimp_estimation_panel.rds")) %>% 
  filter(year == 2019) %>% 
  select(eu, total_hp, fuel_consumption_l, treated, subsidy_cap_l, ph, pl)


## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
price_effect <- (exp(-0.101) - 1)
salience_effect <- (exp(-0.412) - 1)

q <- 10

q + ((1 * (salience_effect + price_effect)) * q)

((1 + ((salience_effect + price_effect ) * 2))) * q

1.325146 / ((1 + ((salience_effect + price_effect ) * 2)))



caps <- 1#seq(0.25, 2, by = 0.25)

sim_panel <- data %>% 
  filter(treated == 1) %>% 
  mutate(p = ifelse(treated, pl, ph),
         R = ph - p,
         q_counter = ((1 + ((salience_effect + price_effect ) * R))) * fuel_consumption_l) %>% 
  expand_grid(pct_q = caps) %>%
  mutate(pct_p = 1 / pct_q,
         # New prices and subsidy caps
         price_subsidy = -2 * pct_p,
         new_subsidy_cap_l = pct_q * subsidy_cap_l,
         # New redesign values
         p_redesign = ph + price_subsidy,
         q_redesign = q_counter / ((1 + ((salience_effect + price_effect ) * price_subsidy))),
         q_redesign = (ifelse(q_redesign > subsidy_cap_l, q_counter, q_redesign)),
         q_additional = q_redesign - q_counter,
         govt_outlay = -price_subsidy * new_subsidy_cap_l)
## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

sum(data$fuel_consumption_l[data$treated == 1])

sim_panel %>% 
  filter(treated == 1,
         pct_p == 1,
         pct_q == 1) %>% 
  pull(q_redesign) %>% 
  sum()

vis_data <- sim_panel %>% 
  group_by(pct_p, pct_q) %>% 
  summarize(q_counter = sum(q_counter),
            q_redesign = sum(q_redesign),
            q_additional = q_redesign - q_counter,
            pct_change = q_additional / q_counter)

ggplot(data = vis_data,
       aes(x = pct_p,
           y = pct_q,
           size = pct_change)) +
  geom_point()

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
