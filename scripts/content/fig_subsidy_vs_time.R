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
pacman::p_load(
  here,
  tidyverse
)
# Load data --------------------------------------------------------------------
shrimp_panel <- readRDS(here("data", "estimation_panels", "shrimp_estimation_panel.rds")) |> 
  filter(year <= 2019)

## VISUALIZE ###################################################################

# theme_set(theme_minimal(base_size = 7))

# X ----------------------------------------------------------------------------
p <- ggplot(data = shrimp_panel %>% 
         filter(!never) %>% 
           mutate(subsidy_pesos = log(subsidy_pesos)) %>% 
           replace_na(replace = list(subsidy_pesos = 0)),
       mapping = aes(x = year,
                     y = subsidy_pesos)) +
  geom_line(mapping = aes(group = eu),
            linewidth = 0.1,
            alpha = 0.2) +
  stat_summary(geom = "line", fun = "mean", color = "steelblue", linewidth = 1) +
  stat_summary(geom = "pointrange", fun.data = mean_sdl, fill = "steelblue", shape = 21) + 
  facet_wrap(~fct_infreq(str_to_sentence(subsidy_frequency)),
             ncol = 1) +
  labs(x = "Year",
       y = "log(Subsidy pesos [MXN])") +
  scale_x_continuous(breaks = 2011:2019)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
ggsave(plot = p,
       filename = here("content", "figures", "fig_subsidy_vs_time.pdf"),
       width = 6,
       height = 3,
       units = "in")
