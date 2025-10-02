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
all_models <- readRDS(file = here("data", "output", "all_elasticity_models.rds"))


# PROCESSING ###################################################################

## Some step -------------------------------------------------------------------
coefficients <- c("TWFE" = all_models$TWFE,
                  "TWFE always" = all_models$`TWFE Always`,
                  "TWFE sometimes" = all_models$`TWFE Sometimes`,
                  "Cov" = all_models$Cov,
                  "TWFE modernized" = all_models$`TWFE Modernized`) |> 
  map_dfr(tidy,
          conf.int = T,
          .id = "model") %>% 
  filter(term == "log(subsidy_pesos)") %>% 
  mutate(var = str_extract(model, "Fishing time|Fishing area|Landings"),
         var = fct_relevel(var, "Fishing time", "Fishing area", "Landings"),
         sample = str_extract(model, "always|sometimes|modernized"),
         sample = replace_na(sample, ""),
         model = str_extract(model, "TWFE|Cov"),
         group = str_squish(paste(model, sample)),
         group = fct_relevel(group, "TWFE", "TWFE always", "TWFE sometimes", "TWFE modernized", "Cov"))


# VISUALIZE ####################################################################

## Another step ----------------------------------------------------------------
p1 <- ggplot(data = coefficients,
             mapping = aes(x = var, y = estimate, fill = var, color = var, shape = group)) +
  geom_hline(yintercept = 0, linetype = "solid") +
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
  scale_shape_manual(values = c(21, 5, 23, 22, 25)) +
  scale_colour_brewer(palette = 'Set2') +
  scale_fill_brewer(palette = 'Set2') +
  guides(fill = "none",
         color = "none",
         shape = guide_legend(ncol = 2,
                              override.aes = list(fill = "black",
                                                  size = 1))) +
  labs(x = "",
       y = "Estimate ± (SE, and 95% Conf.Int.)",
       shape = "Specification and sample") +
  theme(legend.position = "inside",
        legend.position.inside = c(0, 1),
        legend.justification = c(0, 1))


# EXPORT #######################################################################

## The final step --------------------------------------------------------------
ggsave(plot = p1,
       filename = here("content", "figures", "fig_elasticity.pdf"),
       width = 6,
       height = 4,
       units = "in")
