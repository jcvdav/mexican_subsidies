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
  tidyverse
)
## Load data -------------------------------------------------------------------
af <- read_csv(here("data/raw/adjustment_factors.csv"))


# VISUALIZE ####################################################################

## Another step ----------------------------------------------------------------
p <- ggplot(af,
            aes(x = year)) +
  geom_pointrange(aes(y = af, ymin = min, ymax = max),
                  fatten = 1,
                  size = 4,
                  shape = 21,
                  fill = "gray50",
                  color = "black") +
  scale_x_continuous(breaks = c(2011:2019)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(x = "Year", y = "Adjustment factor")

# EXPORT #######################################################################

## The final step --------------------------------------------------------------
ggsave(plot = p,
       filename = here("content/figures/fig_adjustment_factor.pdf"),
       width = 5,
       height = 3.5)  
