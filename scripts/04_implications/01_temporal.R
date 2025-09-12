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
  tidyverse,
  cowplot
)

theme_set(theme_minimal(base_size = 10))

# Load data --------------------------------------------------------------------
shrimp_panel <- readRDS(here("data", "estimation_panels", "shrimp_estimation_panel.rds"))

semi_mod <- readRDS(here("data", "output", "semi_elasticity_twfe_model.rds"))
elasticity_mod <- readRDS(here("data", "output", "elasticity_twfe_model.rds"))


pcts <- c(0.1, 0.3, 0.5, 0.9)

## FUNCTIONS ####################################################################

# Calculate counterfactual scenarios assuming full subsidy removal using semi-elasticity model
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

# Calculate counterfactual scenarios for different subsidy reduction levels using elasticity model
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

# Generate summary statistics for counterfactual scenarios
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
    Pct_Attributable = c(pct_attributable_summary, SD = pct_attributable_sd),
    .id = "var"
  ) |> 
    janitor::clean_names()
  
  return(summary_table)
}

# Create dual-panel plots showing stacked areas and reduction scenarios
plot_counterfactual_scenarios <- function(remove_data, reduce_data, var_col, x_label, y_label, labels = NULL, legend = F) {
  
  remove_data <- remove_data |> 
    mutate(treated = case_when(treated == "Not subsidized-baseline" ~ "Not subsidized",
                               treated == "Subsidized-subsidy" ~ "Subsidized",
                               treated == "Subsidized-baseline" ~ "Induced by subsidies"),
           treated = fct_relevel(treated, "Not subsidized", "Subsidized", "Induced by subsidies"),
           !!sym(var_col) := !!sym(var_col) / 1e6)
  
  reduce_data <- reduce_data |> 
    mutate(!!sym(var_col) := !!sym(var_col) / 1e6)
  
  # Define color palette based on variable type
  if (var_col == "hours") {
    # Greens for hours
palette <- c(
      "Not subsidized" = "#B3B3B3",
      "Subsidized" = "#66C2A5",
      "Induced by subsidies" = "#A6C2A5"
    )
  } else if (var_col == "fg_area_km") {
    # Oranges for area
palette <- c(
      "Not subsidized" = "#B3B3B3",
      "Subsidized" = "#FC8D62",
      "Induced by subsidies" = "#CC8D62"
    )
    
  } else if (var_col == "landed_weight") {
    # Purples for landings
palette <- c(
  "Not subsidized" = "#B3B3B3",
  "Subsidized" = "#8DA0CB",
      "Induced by subsidies" = "#ADA0CB"
    )
  } else {
    stop("Palette not assigned")
  }
  
  # Create the stacked area plot
  p1 <- ggplot(data = remove_data,
               mapping = aes_string(x = "year", y = var_col, fill = "treated")) +
    stat_summary(aes_string(x = "year", y = var_col),
               geom = "line",
               fun = "sum",
               position = "stack",
                 inherit.aes = FALSE) +
    stat_summary(geom = "area",
                 fun = "sum",
               position = "stack") +
    geom_line(data = reduce_data,
              mapping = aes_string(x = "year",
                                   y = var_col,
                                   linetype = "paste0(pct * 100, \"%\")",
                                   group = "pct"),
              inherit.aes = FALSE) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0),
                       breaks = seq(2011, 2019, by = 2)) +
  scale_fill_manual(values = palette) +
    labs(x = x_label,
         y = y_label,
         fill = "Source of activity",
         linetype = "% Subsidy reduction",) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "None",
          plot.margin = margin(t = 5, r = 5, b = 5, l = 15, unit = "pt"))
  
  # Extract units from y_label for the right panel
  if (is.expression(y_label)) {
    # For expressions, create an expression with proper formatting
    change_y_label <- expression("Change\n(Million km"^2*")")
  } else {
    # For character strings, extract units
    units <- str_extract(y_label, "\\([^)]+\\)")
    if (is.na(units)) {
      change_y_label <- "Change"
    } else {
      change_y_label <- paste0("Change\n", units)
    }
  }
  
  # Create the reduction plot
  p2 <- reduce_data %>%
    ggplot(aes_string(x = "pct", y = paste0("-", var_col))) +
    stat_summary(geom = "linerange", fun.data = mean_cl_normal) +
    stat_summary(geom = "point", fun = mean, fill = palette[2]) +
    scale_x_continuous(labels = scales::percent) +
    geom_hline(yintercept = 0, linetype = "dashed") + 
    labs(x = ifelse(x_label == "", "", "% Subsidy reduction"),
         y = change_y_label) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "None",
          plot.margin = margin(t = 5, r = 5, b = 5, l = 15, unit = "pt"))
  
  # Combine plots
  combined_plot <- plot_grid(p1, p2,
                             labels = labels,
                             align = "hv",
                             axis = "tr",
                             ncol = 2,
                             label_size = 10,
                             rel_widths = c(1, 0.5))
  
  if(legend) {
    # Get legend
    leg <- get_plot_component(
      p1 +
        theme(legend.position = "top") +
        guides(fill = guide_legend(ncol = 3, title.position = "top"),
               linetype = guide_legend(ncol = 4, title.position = "top")),
      pattern = "guide-box-top",
      return_all = T)
    
    return(leg)
  } else {
    return(combined_plot)
  }
}

# ## PROCESSING ##################################################################

## Get counterfactual simulations -----------------------------------------------
# Calculate scenarios for full subsidy removal and partial reductions
### For hours
alternative_hours_remove <- calc_semielasticity_counterfactual(semi_mod, "Fishing time")
alternative_hours_reduce <- calc_elasticity_counterfactual(elasticity_mod, "Fishing time")
summary_hours <- generate_counterfactual_summary(alternative_hours_remove, "hours")
summary_hours

# For fishing area
alternative_area_remove <- calc_semielasticity_counterfactual(semi_mod, "Fishing area")
alternative_area_reduce <- calc_elasticity_counterfactual(elasticity_mod, "Fishing area")
summary_area <- generate_counterfactual_summary(alternative_area_remove, "fg_area_km")
summary_area

# For landings
alternative_landings_remove <- calc_semielasticity_counterfactual(semi_mod, "Landings")
alternative_landings_reduce <- calc_elasticity_counterfactual(elasticity_mod, "Landings")
summary_landings <- generate_counterfactual_summary(alternative_landings_remove, "landed_weight")

## VISUALIZE ###################################################################
# Generate combined plots showing temporal attribution of fishing activity
# Create individual plots for each outcome variable

p1 <- plot_counterfactual_scenarios(remove_data = alternative_hours_remove,
                                    reduce_data = alternative_hours_reduce,
                                    var_col = "hours",
                                    labels = c("a)", "b)"),
                                    x_label = "",
                                    y_label = "Fishing time\n(Millions of hours)")

p2 <- plot_counterfactual_scenarios(remove_data = alternative_area_remove,
                                    reduce_data = alternative_area_reduce,
                                    var_col = "fg_area_km",
                                    labels = c("c)", "d)"),
                                    x_label = "",
                                    y_label = expression("Fishing area\n(Million km"^2*")"))

p3 <- plot_counterfactual_scenarios(remove_data = alternative_landings_remove,
                                    reduce_data = alternative_landings_reduce,
                                    var_col = "landed_weight",
                                    labels = c("e)", "f)"),
                                    x_label = "Year",
                                    y_label = "Landings\n(Thousand tons)")
# Get legend
leg <- plot_counterfactual_scenarios(remove_data = alternative_hours_remove,
                                     reduce_data = alternative_hours_reduce,
                                     var_col = "hours",
                                     x_label = "",
                                     y_label = "Fishing time\n(Millions of hours)",
                                     legend = T)

# Add the legend
final_plot <- plot_grid(leg,
                        plot_grid(p1, p2, p3,
                                  ncol = 1,
                                  axis = "bl",
                                  align = "v"),
                        ncol = 1, rel_heights = c(0.1, 1))

# Export final figure
ggsave(plot = final_plot,
       filename = here("content", "figures", "fig_temporal_attribution.pdf"),
       width = 7,
       height = 6,
       device = cairo_pdf)
