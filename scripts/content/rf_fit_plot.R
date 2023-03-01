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
library(here)
library(tidyverse)

# Load data --------------------------------------------------------------------
predicted_panel <- file = here("data", "processed", "imputed_subsidy_economic_unit_annual_shrimp_panel.rds")

## VISUALIZE ###################################################################

# Make plot --------------------------------------------------------------------
fit <- predicted_panel %>% 
  mutate(predicted = subsidy_cap_l > 0) %>% 
  ggplot(mapping = aes(x = subsidy_cap_l / 1e3, y = predicted_subsidy_cap_l / 1e3, fill = predicted)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Random forest predictions",
       subtitle = "Out-of-bag stats: R2 = 0.839, MSE = 48979279688",
       x = "Observed subsidy cap (1000 L)",
       y = "Predicted subsidy cap (1000 L)",
       fill = "Predicted") +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1)) +
  coord_equal()

## EXPORT ######################################################################
ggsave(plot = fit,
       filename = here("results", "img", "rf_fit.pdf"),
       width = 6,
       height = 4.5)