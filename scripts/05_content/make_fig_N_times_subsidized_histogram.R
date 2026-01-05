################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# jc_villasenor@miami.edu
# date
#
# Build a figure that a histogram of the number of times a vessel is subsidized
#
################################################################################
  
# SET UP #######################################################################

## Load packages ---------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse
)

## Load data -------------------------------------------------------------------
shrimp_panel <- readRDS(here("data", "estimation_panels", "shrimp_estimation_panel.rds")) |> 
  filter(year <= 2019)

# PROCESSING ###################################################################

## Get unique data -------------------------------------------------------------
data <- shrimp_panel |> 
  select(eu, subsidy_frequency, n_times_sub) |> 
  distinct()

# VISUALIZE ####################################################################

## Build plot ------------------------------------------------------------------
n_times_sub <- ggplot(data = data,
                      mapping = aes(x = n_times_sub,
                                    fill = fct_relevel(str_to_sentence(subsidy_frequency),
                                                       c("Never", "Sometimes", "Always")))) +
  geom_histogram(binwidth = 1) +
  geom_linerange(y = -2, xmin = 1, xmax = 8, linewidth = 1) +
  geom_linerange(y = -6, xmin = 2, xmax = 9, linewidth = 1, linetype = "dashed") +
  geom_linerange(y = -10, xmin = 8.5, xmax = 9.5, linewidth = 1, linetype = "dotted") +
  scale_x_continuous(labels = c(0:9), breaks = c(0:9)) +
  ylim(c(-10, NA)) +
  theme(legend.position = "inside",
        legend.position.inside = c(0, 1),
        legend.justification.inside = c(0, 1),
        legend.background = element_blank()) +
  labs(x = "Number of times subsidized (2011-2019)",
       y = "Number of  economic units",
       fill = "Subsidy frequency group") +
  scale_fill_brewer(palette = "Blues")

# EXPORT #######################################################################
## Save figure -----------------------------------------------------------------
ggsave(plot = n_times_sub,
       filename = here("content", "figures", "fig_N_times_subsidized_histogram.pdf"),
       width = 6,
       height = 4,
       units = "in")

