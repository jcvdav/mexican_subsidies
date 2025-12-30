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
  ggfixest,
  tidyverse,
  cowplot
)

# Standard errors clustered by economic unit by default
setFixest_vcov(all = "cluster", no_FE = "iid")

## Load data -------------------------------------------------------------------
shrimp_panel <- readRDS(here("data", "estimation_panels", "shrimp_estimation_panel.rds")) |> 
  mutate(post = 1 * (year >= 2020))

## Load models -----------------------------------------------------------------
event_study_exit <- read_rds(file = here("data/output/es_self_reform_model_p_exit.rds"))
event_study_levels <- read_rds(file = here("data/output/es_self_reform_levels.rds"))

# VISUALIZE ####################################################################
theme_set(theme_minimal(base_size = 10))

my_iplot <- function(model){
  
  if(class(model) == "fixest_multi") {
    
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
    
  } else {
    coefs <- iplot_data(model) |> 
      mutate(std.error = (ci_high - ci_low) / (2 * qnorm(0.975)))
    
    ggplot(coefs, aes(x = estimate_names, y = y)) + 
      geom_hline(yintercept = 0, linetype = "solid") +
      geom_vline(xintercept = 2019, linetype = "dashed") +
      geom_pointrange(aes(ymin = ci_low, ymax = ci_high), color = "black") +
      geom_pointrange(aes(ymin = estimate - std.error,
                          ymax = estimate + std.error),
                      fatten = 6,
                      linewidth = 1.5) +
      scale_colour_brewer(palette = "Set2") +
      theme(legend.position = "None") +
      scale_x_continuous(breaks = seq(2012, 2024, by = 3)) +
      labs(x = "Year",
           y = "Estimate ± (SE, and 95% CI)")
  }
}



## Generate plots --------------------------------------------------------------
es_exit <- my_iplot(event_study_exit)
es_levels <- my_iplot(event_study_levels)

plot <- plot_grid(es_exit, es_levels, ncol = 1)

# EXPORT #######################################################################

## The final step --------------------------------------------------------------

ggsave(plot = plot,
       filename = here("content/figures/event_study_2020_reform_exit.pdf"), width = 8, height = 4.5)

