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
  ggfixest,
  tidyverse
)

## Load models -----------------------------------------------------------------
event_study_self_ext <- read_rds(file = here("data/output/es_self_reform_model_ext.rds"))
event_study_self_levels <- read_rds(file = here("data/output/es_self_reform_model_levels.rds"))

# VISUALIZE ####################################################################

theme_set(theme_minimal(base_size = 10))

my_iplot <- function(model){
  coefs <- iplot_data(model) |> 
    mutate(id = fct_relevel(id, "Fishing time", "Fishing area", "Landings"),
           std.error = (ci_high - ci_low) / (2 * qnorm(0.975)))

  ggplot(coefs, aes(x = estimate_names, y = y, color = id)) + 
    geom_hline(yintercept = 0, linetype = "solid") +
    geom_vline(xintercept = 2019, linetype = "dashed") +
    geom_pointrange(aes(ymin = ci_low, ymax = ci_high), color = "black") +
    geom_pointrange(aes(ymin = estimate - std.error,
                        ymax = estimate + std.error,
                        color = id),
                    fatten = 6,
                    linewidth = 1.5) +
    facet_wrap(~id, scales = "free_y",
               ncol = 3) +
    scale_colour_brewer(palette = "Set2") +
    theme(legend.position = "None") +
    scale_x_continuous(breaks = seq(2012, 2024, by = 3)) +
    labs(x = "Year",
         y = "Estimate ± (SE, and 95% CI)")
}



## Generate plots --------------------------------------------------------------
es_ext <- my_iplot(event_study_self_ext)
es_levels <- my_iplot(event_study_self_levels)

# EXPORT #######################################################################

## The final step --------------------------------------------------------------

ggsave(plot = es_ext,
       filename = here("content/figures/event_study_2020_reform_ext.pdf"), width = 8, height = 2)

ggsave(plot = es_levels,
       filename = here("content/figures/event_study_2020_reform_levels.pdf"), width = 8, height = 2)

