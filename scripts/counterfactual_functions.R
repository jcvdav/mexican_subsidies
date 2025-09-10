################################################################################
# Counterfactual Analysis Functions
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Functions to calculate counterfactual scenarios using elasticity and 
# semi-elasticity models for different policy interventions
#
################################################################################

# Load packages
pacman::p_load(
  here,
  tidyverse,
  broom,
  fixest
)

#' Calculate counterfactual using semi-elasticity (full subsidy removal)
#' 
#' @param model Semi-elasticity model object
#' @param var_name Name of the variable (e.g., "Fishing time", "Fishing area", "Landings")
#' @param data Panel data with treated indicator and variable of interest
#' @return Data frame with counterfactual analysis
calculate_semi_elasticity_counterfactual <- function(model, var_name, data) {
  
  # Extract coefficient and calculate % change
  semi_coef <- coef(model[[var_name]])[[1]]
  change <- (exp(semi_coef) - 1)
  
  # Get the actual variable name from the data
  var_col <- case_when(
    var_name == "Fishing time" ~ "hours",
    var_name == "Fishing area" ~ "fg_area_km", 
    var_name == "Landings" ~ "landed_weight",
    TRUE ~ stop("Variable name not recognized")
  )
  
  # Calculate counterfactual
  result <- data %>%
    drop_na(!!sym(var_col)) %>%
    select(year, eu, treated, !!sym(var_col)) %>%
    mutate(
      additional = ifelse(treated == 1, change * !!sym(var_col), 0),
      treated = ifelse(treated == 1, "Subsidized", "Not subsidized")
    ) %>%
    group_by(year, treated) %>%
    summarize(
      !!sym(var_col) := sum(!!sym(var_col)) / 1e6,  # Convert to millions
      subsidy = sum(additional) / 1e6,
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

#' Calculate counterfactual using elasticity (range of subsidy reductions)
#' 
#' @param model Elasticity model object
#' @param var_name Name of the variable (e.g., "Fishing time", "Fishing area", "Landings")
#' @param data Panel data with treated indicator and variable of interest
#' @param pct_reductions Vector of percentage reductions to simulate (e.g., c(0.1, 0.3, 0.5, 0.9))
#' @return Data frame with counterfactual analysis for different reduction scenarios
calculate_elasticity_counterfactual <- function(model, var_name, data, pct_reductions = c(0.1, 0.3, 0.5, 0.9)) {
  
  # Extract elasticity coefficient
  elasticity <- coef(model[[var_name]])[[1]]
  
  # Get the actual variable name from the data
  var_col <- case_when(
    var_name == "Fishing time" ~ "hours",
    var_name == "Fishing area" ~ "fg_area_km", 
    var_name == "Landings" ~ "landed_weight",
    TRUE ~ stop("Variable name not recognized")
  )
  
  # Calculate counterfactual for different reduction scenarios
  result <- data %>%
    drop_na(!!sym(var_col)) %>%
    expand_grid(pct = pct_reductions) %>%
    mutate(
      factor = 1 + (((1 - pct)^elasticity) - 1),
      additional = treated * (!!sym(var_col) - (factor * !!sym(var_col))),
      treated = ifelse(treated == 1, "Subsidized", "Not subsidized")
    ) %>%
    group_by(year, treated, pct) %>%
    summarize(
      !!sym(var_col) := sum(!!sym(var_col)) / 1e6,  # Convert to millions
      subsidy = sum(additional) / 1e6,
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

#' Generate summary statistics table from counterfactual data
#' 
#' @param data Counterfactual data from calculate_semi_elasticity_counterfactual or calculate_elasticity_counterfactual
#' @param var_name Name of the variable for labeling
#' @return List of summary statistics
generate_counterfactual_summary <- function(data, var_name) {
  
  # Get the actual variable name from the data
  var_col <- case_when(
    var_name == "Fishing time" ~ "hours",
    var_name == "Fishing area" ~ "fg_area_km", 
    var_name == "Landings" ~ "landed_weight",
    TRUE ~ stop("Variable name not recognized")
  )
  
  # Overall summary by year
  overall_summary <- data %>%
    group_by(year) %>%
    summarize(!!sym(var_col) := sum(!!sym(var_col)), .groups = "drop") %>%
    pull(!!sym(var_col)) %>%
    summary()
  
  # Standard deviation
  overall_sd <- data %>%
    group_by(year) %>%
    summarize(!!sym(var_col) := sum(!!sym(var_col)), .groups = "drop") %>%
    pull(!!sym(var_col)) %>%
    sd()
  
  # Summary for subsidized vessels only
  subsidized_summary <- data %>%
    filter(treated != "Not subsidized-baseline") %>%
    group_by(year) %>%
    summarize(!!sym(var_col) := sum(!!sym(var_col)), .groups = "drop") %>%
    pull(!!sym(var_col)) %>%
    summary()
  
  subsidized_sd <- data %>%
    filter(treated != "Not subsidized-baseline") %>%
    group_by(year) %>%
    summarize(!!sym(var_col) := sum(!!sym(var_col)), .groups = "drop") %>%
    pull(!!sym(var_col)) %>%
    sd()
  
  # Range of values attributable to subsidy
  subsidy_attributable <- data %>%
    filter(treated != "Not subsidized-baseline") %>%
    filter(source == "subsidy") %>%
    pull(!!sym(var_col)) %>%
    summary()
  
  subsidy_attributable_sd <- data %>%
    filter(treated != "Not subsidized-baseline") %>%
    filter(source == "subsidy") %>%
    pull(!!sym(var_col)) %>%
    sd()
  
  # Percentage of total attributable to subsidy
  pct_attributable <- data %>%
    group_by(year) %>%
    mutate(pct_total = !!sym(var_col) / sum(!!sym(var_col))) %>%
    filter(source == "subsidy") %>%
    pull(pct_total) %>%
    range()
  
  # Return summary list
  list(
    overall_summary = overall_summary,
    overall_sd = overall_sd,
    subsidized_summary = subsidized_summary,
    subsidized_sd = subsidized_sd,
    subsidy_attributable_summary = subsidy_attributable,
    subsidy_attributable_sd = subsidy_attributable_sd,
    pct_attributable_range = pct_attributable
  )
}

# Example usage:
# Load data and models
# shrimp_panel <- readRDS(here("data", "estimation_panels", "shrimp_estimation_panel.rds"))
# semi_mod <- readRDS(here("data", "output", "semi_elasticity_twfe_model.rds"))
# elasticity_mod <- readRDS(here("data", "output", "elasticity_twfe_model.rds"))
# 
# # Calculate counterfactuals
# hours_semi <- calculate_semi_elasticity_counterfactual(semi_mod, "Fishing time", shrimp_panel)
# hours_elasticity <- calculate_elasticity_counterfactual(elasticity_mod, "Fishing time", shrimp_panel)
# 
# # Generate summaries
# hours_semi_summary <- generate_counterfactual_summary(hours_semi, "Fishing time")
# hours_elasticity_summary <- generate_counterfactual_summary(hours_elasticity, "Fishing time")
