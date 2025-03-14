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
  cowplot,
  tidyverse
)

# Load data --------------------------------------------------------------------
shrimp_panel <- readRDS(here("data", "estimation_panels", "shrimp_estimation_panel.rds"))

# Custom functions -------------------------------------------------------------
effect_plot <- function(data, var = hours, n = 1){
  # browser()
  fill <- palette.colors(n = 3, palette = "Set 2")[n]
  
  plot_data <- data %>% 
    mutate(treatment = ifelse(treated == 1,
                              "Subsidized",
                              "Not subsidized"),
           subsidy_frequency = str_to_sentence(subsidy_frequency),
           subsidy_frequency = fct_relevel(subsidy_frequency, "Never", "Sometimes", "Always"))
  
  vals <- plot_data %>% 
    filter(subsidy_frequency == "Sometimes") %>%
    group_by(treatment) %>%
    summarize(var = mean(log({{var}})), .groups = "drop") %>% 
    mutate(a = diff(var) / var)
  
  num <- vals %>% 
    pull(a) %>% 
    head(1) %>% 
    scales::percent(accuracy = 0.01)
  
  height <- vals %>% 
    pull(var) %>% 
    max()
  
  
  pos <- position_jitter(width = 0.5,
                         height = 0,
                         seed = 1)
  
  ggplot(data = plot_data,
         mapping = aes(x = treatment,
                       y = log({{var}}),
                       shape = subsidy_frequency)) + 
    stat_summary(geom = "linerange",
                 fun.data = mean_cl_normal,
                 color = "black",
                 linewidth = 0.1,
                 position = pos) +
    stat_summary(geom = "line",
                 fun = mean,
                 color = fill,
                 linetype = "dashed",
                 aes(group = subsidy_frequency),
                 position = pos) +
    stat_summary(geom = "pointrange",
                 fun.data = mean_se,
                 size = 3,
                 linewidth = 1.5,
                 fatten = 1,
                 fill = fill,
                 color = fill,
                 position = pos) +
    labs(x = "Status") +
    theme(legend.position = "None") +
    scale_shape_manual(values = c(22, 21, 23)) +
    annotate(x = 1.4,
             y = 1.01 * height,
             geom = "text",
             label = paste0(num, " change"),
             color = fill)
}

## VISUALIZE ###################################################################

# Plot for hours ---------------------------------------------------------------
hrs <- effect_plot(data = shrimp_panel,
                   var = hours) +
  labs(title = "Fishing time (hours)",
       y = "log(time)",
       x = "")

# Plot for area ----------------------------------------------------------------
area <- effect_plot(data = shrimp_panel %>% 
                      filter(!is.na(fg_area_km),
                             fg_area_km > 0),
                    var = fg_area_km,
                    n = 2) +
  labs(title = "Fishing area (Km^2)",
       y = "log(area)")

# Plot for landings ------------------------------------------------------------
landings <- effect_plot(data = shrimp_panel %>% 
                          filter(!is.na(landed_weight),
                                 landed_weight > 0),
                        var = landed_weight,
                        n = 3) +
  labs(title = "Landings (Kg)",
       y = "log(landings)", 
       x = "")

# Combine ---------------------------------------------------------------
l_with_leg <- landings +
  theme(legend.position = "bottom") +
  guides(shape = guide_legend(title = "Sub-sample",
                              override.aes = list(size = 0.5,
                                                  fill = "black",
                                                  color = "black")),
         linetype = "none")

leg <- cowplot::get_legend(plot = l_with_leg)

p1 <- cowplot::plot_grid(hrs, area, landings,
                         labels = c("a)", "b)", "c)"),
                         ncol = 3,
                         align = "hv")

p2 <- cowplot::plot_grid(p1, leg,
                         ncol = 1,
                         rel_heights = c(1, 0.2))

## EXPORT ######################################################################
# X ----------------------------------------------------------------------------
ggsave(plot = p2,
       filename = here("results", "img", "fig_enter.pdf"),
       width = 7,
       height = 4,
       units = "in")

