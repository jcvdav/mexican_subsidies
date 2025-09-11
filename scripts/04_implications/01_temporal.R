################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
# How many times does a vessel enter / exit the roster?
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  fixest,
  modelsummary,
  tidyverse
)

theme_set(theme_minimal(base_size = 10))

# Load data --------------------------------------------------------------------
shrimp_panel <- readRDS(here("data", "estimation_panels", "shrimp_estimation_panel.rds"))

semi_mod <- readRDS(here("data", "output", "semi_elasticity_twfe_model.rds"))
elasticity_mod <- readRDS(here("data", "output", "elasticity_twfe_model.rds"))


pcts <- c(0.1, 0.3, 0.5, 0.9)

## FUNCTIONS ####################################################################

calc_semielasticity_counterfactual <- function(model, var_name) {
  # Get the actual variable name from the data and model index
  var_col <- case_when(
    var_name == "Fishing time" ~ "hours",
    var_name == "Fishing area" ~ "fg_area_km", 
    var_name == "Landings" ~ "landed_weight"
  )
  
  model_idx <- case_when(
    var_name == "Fishing time" ~ 1L,
    var_name == "Fishing area" ~ 2L, 
    var_name == "Landings" ~ 3L
  )
  
  if (is.na(var_col) || is.na(model_idx)) {
    stop("Variable name not recognized")
  }
  
  # Extract coefficient and calculate % change
  semi_coef <- coef(model[[model_idx]])[[1]]
  change <- as.numeric(exp(semi_coef) - 1)
  
  # Calculate counterfactual
  result <- shrimp_panel %>%
    select(year, eu, treated, !!sym(var_col)) %>%
    mutate(
      additional = ifelse(treated == 1, change * !!sym(var_col), 0),
      treated = ifelse(treated == 1, "Subsidized", "Not subsidized")
    ) %>%
    group_by(year, treated) %>%
    summarize(
      !!sym(var_col) := sum(!!sym(var_col), na.rm = TRUE),
      subsidy = sum(additional, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(baseline = !!sym(var_col) - subsidy) %>%
    select(-!!sym(var_col)) %>%
    pivot_longer(
      cols = c(subsidy, baseline),
      values_to = var_col,
      names_to = "source"
    ) %>%
    mutate(treated = paste(treated, source, sep = "-")) %>%
    filter(!!sym(var_col) > 0)
  
  return(result)
}

calc_elasticity_counterfactual <- function(model, var_name, pct_reductions = c(0.1, 0.3, 0.5, 0.9)) {
  # Get the actual variable name from the data and model index
  var_col <- case_when(
    var_name == "Fishing time" ~ "hours",
    var_name == "Fishing area" ~ "fg_area_km", 
    var_name == "Landings" ~ "landed_weight"
  )
  
  model_idx <- case_when(
    var_name == "Fishing time" ~ 1L,
    var_name == "Fishing area" ~ 2L, 
    var_name == "Landings" ~ 3L
  )
  
  if (is.na(var_col) || is.na(model_idx)) {
    stop("Variable name not recognized")
  }
  
  # Extract elasticity coefficient
  elasticity <- as.numeric(coef(model[[model_idx]])[[1]])
  
  # Calculate counterfactual for different reduction scenarios
  result <- shrimp_panel %>%
    select(year, eu, treated, !!sym(var_col)) %>%
    expand_grid(pct = pct_reductions) %>%
    mutate(
      factor = 1 + (((1 - pct)^elasticity) - 1),
      additional = treated * (!!sym(var_col) - (factor * !!sym(var_col))),
      treated = ifelse(treated == 1, "Subsidized", "Not subsidized")
    ) %>%
    group_by(year, treated, pct) %>%
    summarize(
      !!sym(var_col) := sum(!!sym(var_col), na.rm = TRUE),
      subsidy = sum(additional, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(baseline = !!sym(var_col) - subsidy) %>%
    select(year, treated, pct, subsidy, baseline) %>%
    pivot_longer(
      cols = c(subsidy, baseline),
      values_to = var_col,
      names_to = "source"
    ) %>%
    filter(
      !!sym(var_col) > 0,
      treated == "Subsidized",
      source == "subsidy"
    ) %>%
    mutate(treated = paste(treated, source, sep = "-"))
  
  return(result)
}

generate_counterfactual_summary <- function(data, var_col) {
  # Overall summary by year
  overall_stats <- data %>%
    group_by(year) %>%
    summarize(!!sym(var_col) := sum(!!sym(var_col), na.rm = TRUE), .groups = "drop") %>%
    pull(!!sym(var_col))
  
  overall_summary <- summary(overall_stats)
  overall_sd <- sd(overall_stats, na.rm = TRUE)
  
  # Summary for subsidized vessels only
  subsidized_stats <- data %>%
    filter(treated != "Not subsidized-baseline") %>%
    group_by(year) %>%
    summarize(!!sym(var_col) := sum(!!sym(var_col), na.rm = TRUE), .groups = "drop") %>%
    pull(!!sym(var_col))
  
  subsidized_summary <- summary(subsidized_stats)
  subsidized_sd <- sd(subsidized_stats, na.rm = TRUE)
  
  # Values attributable to subsidy
  subsidy_attributable_stats <- data %>%
    filter(treated != "Not subsidized-baseline") %>%
    filter(source == "subsidy") %>%
    pull(!!sym(var_col))
  
  subsidy_attributable_summary <- summary(subsidy_attributable_stats)
  subsidy_attributable_sd <- sd(subsidy_attributable_stats, na.rm = TRUE)
  
  # Percentage of total attributable to subsidy
  pct_attributable_stats <- data %>%
    group_by(year) %>%
    mutate(pct_total = !!sym(var_col) / sum(!!sym(var_col), na.rm = TRUE)) %>%
    filter(source == "subsidy") %>%
    pull(pct_total)
  
  pct_attributable_summary <- summary(pct_attributable_stats)
  pct_attributable_sd <- sd(pct_attributable_stats, na.rm = TRUE)
  
  # Combine summaries with SD
  summary_table <- bind_rows(
    Overall = c(overall_summary, SD = overall_sd),
    Subsidized = c(subsidized_summary, SD = subsidized_sd),
    Subsidy_Attributable = c(subsidy_attributable_summary, SD = subsidy_attributable_sd),
    Pct_Attributable = c(pct_attributable_summary, SD = pct_attributable_sd)
  )
  
  return(summary_table)
}

plot_counterfactual_scenarios <- function(remove_data, reduce_data, var_col, y_label, title, 
                                         show_linetype = FALSE) {
  
  # Define color palette based on variable type
  if (var_col == "hours") {
    # Greens for hours
    palette <- c(
      "Not subsidized-baseline" = "#B3B3B3",
      "Subsidized-baseline" = "#66C2A5",
      "Subsidized-subsidy" = "#A6C2A5"
    )
  } else if (var_col == "fg_area_km") {
    # Purples for area
    palette <- c(
      "Not subsidized-baseline" = "#B3B3B3",
      "Subsidized-baseline" = "#8DA0CB",
      "Subsidized-subsidy" = "#ADA0CB"
    )
  } else if (var_col == "landed_weight") {
    # Oranges for landings
    palette <- c(
      "Not subsidized-baseline" = "#B3B3B3",
      "Subsidized-baseline" = "#FC8D62",
      "Subsidized-subsidy" = "#CC8D62"
    )
  } else {
    # Default palette
    palette <- c(
      "Not subsidized-baseline" = "#B3B3B3",
      "Subsidized-baseline" = "#FC8D62",
      "Subsidized-subsidy" = "#CC8D62"
    )
  }
  
  # Create the plot
  p <- ggplot(data = remove_data,
              mapping = aes_string(x = "year", y = var_col, fill = "treated")) +
    stat_summary(aes_string(x = "year", y = var_col),
                 geom = "line",
                 fun = "sum",
                 position = "stack",
                 inherit.aes = FALSE) +
    stat_summary(geom = "area", fun = "sum",
                 position = "stack")
  
  # Add reduction lines
  if (show_linetype) {
    p <- p + geom_line(data = reduce_data,
                       mapping = aes_string(x = "year",
                                           y = var_col,
                                           linetype = "paste0(pct * 100, \"%\")",
                                           group = "pct"),
                       inherit.aes = FALSE)
  } else {
    p <- p + geom_line(data = reduce_data,
                       mapping = aes_string(x = "year",
                                           y = var_col,
                                           group = "pct"),
                       color = "black",
                       linewidth = 0.3,
                       alpha = 0.5,
                       inherit.aes = FALSE)
  }
  
  # Add scales and labels
  p <- p + scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0),
                       breaks = seq(2011, 2019, by = 2),
                       limits = c(2011, 2019.5)) +
    scale_fill_manual(values = palette) +
    labs(title = title,
         x = "Year",
         y = y_label,
         fill = "Source") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(p)
}

# ## PROCESSING ##################################################################

## Get counterfacutual simulations -----------------------------------------------
### For hours
alternative_hours_remove <- calc_semielasticity_counterfactual(semi_mod, "Fishing time")
alternative_hours_reduce <- calc_elasticity_counterfactual(elasticity_mod, "Fishing time")
summary_hours <- generate_counterfactual_summary(alternative_hours_remove, "hours")

# For fishing area
alternative_area_remove <- calc_semielasticity_counterfactual(semi_mod, "Fishing area")
alternative_area_reduce <- calc_elasticity_counterfactual(elasticity_mod, "Fishing area")
summary_area <- generate_counterfactual_summary(alternative_area_remove, "fg_area_km")

# For landings
alternative_landings_remove <- calc_semielasticity_counterfactual(semi_mod, "Landings")
alternative_landings_reduce <- calc_elasticity_counterfactual(elasticity_mod, "Landings")
summary_landings <- generate_counterfactual_summary(alternative_landings_remove, "landed_weight")


plot_counterfactual_scenarios(remove_data = alternative_hours_remove,
                              reduce_data = alternative_hours_reduce,
                              var_col = "hours",
                              y_label = "Hours",
                              title = "A", show_linetype = T)

plot_counterfactual_scenarios(remove_data = alternative_area_remove,
                              reduce_data = alternative_area_reduce,
                              var_col = "fg_area_km",
                              y_label = "Area",
                              title = "A")

plot_counterfactual_scenarios(remove_data = alternative_landings_remove,
                              reduce_data = alternative_landings_reduce,
                              var_col = "landed_weight",
                              y_label = "Area",
                              title = "A")

## VISUALIZE ###################################################################

# Hours ------------------------------------------------------------------------
palette <- c(
  "Not subsidized-baseline" = "#B3B3B3",
  "Subsidized-baseline" = "#66C2A5",
  "Subsidized-subsidy" = "#A6C2A5"
)


p1 <- ggplot(data = alternative_hours_remove,
             mapping = aes(x = year, y = hours, fill = treated)) +
  stat_summary(aes(x = year, y = hours),
               geom = "line",
               fun = "sum",
               position = "stack",
               inherit.aes = F) +
  stat_summary(geom = "area", fun = "sum",
               position = "stack") +
  geom_line(data = alternative_hours_reduce,
            mapping = aes(x = year,
                          y = hours,
                          linetype = paste0(pct * 100, "%"),
                          group = pct),
            inherit.aes = F) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(2011, 2019, by = 2),
                     limits = c(2011, 2019.5)) +
  scale_fill_manual(values = palette) +
  labs(title = "Fishing time",
       x = "Year",
       y = "Total activity\n(Millions of hours)",
       fill = "Source",
       linetype = "% Subsidy reduction") +
  theme(legend.position = "None")

# Area -------------------------------------------------------------------------

palette <- c(
  "Not subsidized" = "#B3B3B3",
  "Subsidized" = "#8DA0CB",
  "Subsidy" = "#ADA0CB"
)
p2 <- ggplot(data = alternative_area_remove,
             mapping = aes(x = year, y = fg_area_km, fill = treated)) +
  stat_summary(aes(x = year, y = fg_area_km),
               geom = "line",
               fun = "sum",
               position = "stack",
               inherit.aes = F) +
  stat_summary(geom = "area", fun = "sum",
               position = "stack") +
  geom_line(data = alternative_area_reduce,
            mapping = aes(x = year,
                          y = fg_area_km,
                          linetype = paste0(pct * 100, "%"),
                          group = pct),
            inherit.aes = F) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(2011, 2019, by = 2),
                     limits = c(2011, 2019.5)) +
  scale_fill_manual(values = palette) +
  labs(title = "Fishing area",
       x = "Year",
       y = "Total area\n(Millions of Km2)",
       fill = "Source",
       linetype = "% Subsidy reduction") +
  theme(legend.position = "None")

palette <- c(
  "Not subsidized-baseline" = "#B3B3B3",
  "Subsidized-baseline" = "#FC8D62",
  "Subsidized-subsidy" = "#CC8D62"
)

# Landings ---------------------------------------------------------------------
p3 <- ggplot(data = alternative_landings_remove,
             mapping = aes(x = year, y = landed_weight, fill = treated)) +
  stat_summary(aes(x = year, y = landed_weight),
               geom = "line",
               fun = "sum",
               position = "stack",
               inherit.aes = F) +
  stat_summary(geom = "area", fun = "sum",
               position = "stack") +
  geom_line(data = alternative_landings_reduce,
            mapping = aes(x = year,
                          y = landed_weight,
                          linetype = paste0(pct * 100, "%"),
                          group = pct),
            inherit.aes = F) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(2011, 2019, by = 2),
                     limits = c(2011, 2019.5)) +
  scale_fill_manual(values = palette) +
  labs(title = "Landings",
       x = "Year",
       y = "Total Landings\n(Thousand tonnes)",
       fill = "Source",
       linetype = "% Subsidy reduction") +
  theme(legend.position = "None")

leg <- cowplot::get_plot_component(
  p3 +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(ncol = 1, title.position = "top"),
           linetype = guide_legend(ncol = 2, title.position = "top")),
  pattern = "guide-box-bottom",
  return_all = T
)

plot <- cowplot::plot_grid(p1, p2, p3, leg,
                           ncol = 2, labels = c("a)", "b)", "c)"),
                           align = "hv")

ggsave(plot = plot,
       filename = here("results", "img", "fig_temporal_attribution.pdf"),
       width = 7,
       height = 4)



pp <- bind_rows(alternative_hours2 %>% rename(value = hours),
          alternative_area2 %>% rename(value = fg_area_km),
          alternative_landings2 %>% rename(value = landed_weight),
          .id = "variable") %>% 
  mutate(variable = case_when(variable == 1 ~ "d) Fishing time (Millions of hours)",
                              variable == 2 ~ "e) Fishing area (Millions of Km2)",
                              variable == 3 ~ "f) Landings (Tones)"),
         variable = fct_relevel(variable,
                                "d) Fishing time (hours)",
                                "e) Fishing area (Km2)",
                                "f) Landings (Kg)")) %>% 
  ggplot(aes(x = pct, y = -value, color = variable)) +
  stat_summary(geom = "pointrange", fun.data = mean_cl_normal, color = "black") +
  stat_summary(geom = "pointrange", fun.data = mean_se, linewidth = 1.5) +
  facet_wrap(~variable, scales = "free_y", ncol = 3) +
  scale_x_continuous(labels = scales::percent) +
  scale_color_brewer(palette = "Set2") +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "%Change in subsidy allocations",
       y = "Mean reduction") +
  theme_minimal() +
  theme(legend.position = "None")

ggsave(plot = pp,
       filename = here("results", "img", "fig_pct_reductions.pdf"),
       width = 6,
       height = 2)
