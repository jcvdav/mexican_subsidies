################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# jc_villasenor@miami.edu
# date
#
# Description
#
################################################################################
  
# SET UP #######################################################################

## Load packages ---------------------------------------------------------------
pacman::p_load(
  here,
  fixest,
  broom,
  tidyverse
)

## Load data -------------------------------------------------------------------
semi_elasticity_twfe <- readRDS(file = here("data", "output", "semi_elasticity_twfe_model.rds"))
subsidized_n_times_models <- readRDS(file = here("data", "output", "subsidized_n_times_models.rds")) |> 
  mutate(var = fct_relevel(var, "Fishing time", "Fishing area", "Landings"))

# PROCESSING ###################################################################

## Some step -------------------------------------------------------------------
coefficients <- map_dfr(semi_elasticity_twfe, tidy, .id = "var") |> 
  mutate(var = fct_relevel(var, "Fishing time", "Fishing area", "Landings"))

# VISUALIZE ####################################################################

## Another step ----------------------------------------------------------------
p <- ggplot(data = subsidized_n_times_models,
             aes(x = n_times, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_hline(data = coefficients, aes(yintercept = estimate, color = var)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(aes(ymin = estimate - std.error,
                      ymax = estimate + std.error,
                      color = var),
                  fatten = 6,
                  linewidth = 1.5) +
  scale_colour_brewer(palette = 'Set2') +
  guides(color = "none") +
  labs(x = "Subsidized at most # times",
       y = "Estimate and 95% CI") +
  facet_wrap(~var, ncol = 3)

# EXPORT #######################################################################

## The final step --------------------------------------------------------------
ggsave(plot = p,
       filename = here("content", "figures", "fig_semi_elasticity_by_frequency.pdf"),
       width = 6,
       height = 4,
       units = "in")  

