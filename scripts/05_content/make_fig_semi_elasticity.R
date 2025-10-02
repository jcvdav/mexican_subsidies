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
  tidyverse,
  broom,
  fixest
)

## Load data -------------------------------------------------------------------
all_ext_models <- readRDS(file = here("data", "output", "all_ext_models.rds"))
all_level_models <- readRDS(file = here("data", "output", "all_level_models.rds")) 
all_semi_elasticity_models <- readRDS(file = here("data", "output", "all_semi_elasticity_models.rds"))


# PROCESSING ###################################################################

## Extract coefficients --------------------------------------------------------
get_coefficients <- function(models){
  coefficients <- c("TWFE sometimes sub." = models$`TWFE sometimes sub.`,
                    "Cov sometimes sub." = models$`Cov sometimes sub.`,
                    "TWFE all" = models$`TWFE all`,
                    "TWFE modernized" = models$`TWFE modernized`) |> 
    map_dfr(tidy,
            conf.int = T,
            .id = "model") %>% 
    filter(term == "treated") %>% 
    mutate(var = str_extract(model, "Fishing time|Fishing area|Landings"),
           var = fct_relevel(var, "Fishing time", "Fishing area", "Landings"),
           sample = str_extract(model, "sometimes sub\\.|all|modernized"),
           model = str_extract(model, "TWFE|Cov"),
           group = paste(model, sample),
           group = fct_relevel(group, "TWFE sometimes sub.", "Cov sometimes sub.", "TWFE all"))
  
  return(coefficients)
}

all_ext_coefs <- get_coefficients(all_ext_models)
all_level_coefs <- get_coefficients(all_level_models)
all_semi_elasticity_coefs <- get_coefficients(all_semi_elasticity_models)


# VISUALIZE ####################################################################

## Build figure ----------------------------------------------------------------

build_coefplot <-  function(coefficients) {
  ggplot(data = coefficients,
         mapping = aes(x = group, y = estimate, fill = var, color = var)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_linerange(aes(ymin = conf.low,
                       ymax = conf.high),
                   color = "black",
                   position = position_dodge(width = 0.5),
                   linewidth = 0.1) +
    geom_pointrange(aes(ymin = estimate - std.error,
                        ymax = estimate + std.error),
                    position = position_dodge(width = 0.5),
                    fatten = 6,
                    linewidth = 1.5) +
    scale_shape_manual(values = c(21, 22, 23)) +
    scale_colour_brewer(palette = 'Set2') +
    scale_fill_brewer(palette = 'Set2') +
    guides(fill = "none",
           color = "none",
           shape = guide_legend(ncol = 2,
                                override.aes = list(fill = "black",
                                                    size = 1))) +
    labs(x = "",
         y = "Estimate ± (SE, and 95% Conf.Int.)") +
    theme(legend.position = "None") +
    facet_wrap(~var, scales = "free_x") +
    coord_flip()
}

p_ext <- build_coefplot(all_ext_coefs)
p_level <- build_coefplot(all_level_coefs)
p_semi_elasticity <- build_coefplot(all_semi_elasticity_coefs)


# EXPORT #######################################################################

## Export figure ---------------------------------------------------------------
ggsave(plot = p_ext,
       filename = here("content", "figures", "fig_extensive.pdf"),
       width = 7,
       height = 2,
       units = "in")

ggsave(plot = p_level,
       filename = here("content", "figures", "fig_levels.pdf"),
       width = 7,
       height = 2,
       units = "in")

ggsave(plot = p_semi_elasticity,
       filename = here("content", "figures", "fig_semi_elasticity.pdf"),
       width = 7,
       height = 2,
       units = "in")

