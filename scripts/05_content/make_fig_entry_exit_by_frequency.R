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
ext <- readRDS(file = here("data", "output", "ext_model.rds"))
levels <- readRDS(file = here("data", "output", "levels_model.rds"))
semi_elasticity <- readRDS(file = here("data", "output", "semi_elasticity_twfe_model.rds"))

subsidized_n_times_ext_models <- readRDS(file = here("data", "output", "subsidized_n_times_ext_models.rds")) |> 
  mutate(var = fct_relevel(var, "Fishing time", "Fishing area", "Landings"))
subsidized_n_times_levels_models <- readRDS(file = here("data", "output", "subsidized_n_times_levels_models.rds")) |> 
  mutate(var = fct_relevel(var, "Fishing time", "Fishing area", "Landings"))
subsidized_n_times_semi_elasticity_models <- readRDS(file = here("data", "output", "subsidized_n_times_semi_elasticity_models.rds")) |> 
  mutate(var = fct_relevel(var, "Fishing time", "Fishing area", "Landings"))

# PROCESSING ###################################################################

## Get coefficients for each ---------------------------------------------------
coefplot <- function(models, model) {
  coefficients <- map_dfr(model, tidy, .id = "var") |> 
    mutate(var = fct_relevel(var, "Fishing time", "Fishing area", "Landings"))
  
  # VISUALIZE ####################################################################
  
  ## Another step ----------------------------------------------------------------
  p <- ggplot(data = models,
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
         y = "Estimate ± (SE, and 95% Conf.Int.)") +
    facet_wrap(~var, ncol = 3, scales = "free_y")
  
  return(p)
}

p1 <- coefplot(models = subsidized_n_times_ext_models, model = ext)
p2 <- coefplot(models = subsidized_n_times_levels_models, model = levels)
p3 <- coefplot(models = subsidized_n_times_semi_elasticity_models, model = semi_elasticity)

# EXPORT #######################################################################

## The final step --------------------------------------------------------------
ggsave(plot = p1,
       filename = here("content", "figures", "fig_ext_by_frequency.pdf"),
       width = 6,
       height = 4,
       units = "in")  

ggsave(plot = p2,
       filename = here("content", "figures", "fig_levels_by_frequency.pdf"),
       width = 6,
       height = 4,
       units = "in")  

ggsave(plot = p3,
       filename = here("content", "figures", "fig_semi_elasticity_by_frequency.pdf"),
       width = 6,
       height = 4,
       units = "in")  

