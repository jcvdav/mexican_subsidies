################################################################################
# Test Counterfactual Functions
################################################################################
#
# This script demonstrates how to use the counterfactual analysis functions
#
################################################################################

# Load packages
pacman::p_load(
  here,
  tidyverse
)

# Source the functions
source(here("scripts", "counterfactual_functions.R"))

# Load data and models
shrimp_panel <- readRDS(here("data", "estimation_panels", "shrimp_estimation_panel.rds"))
semi_mod <- readRDS(here("data", "output", "semi_elasticity_twfe_model.rds"))
elasticity_mod <- readRDS(here("data", "output", "elasticity_twfe_model.rds"))

# Test with fishing time
cat("=== FISHING TIME ANALYSIS ===\n")

# Semi-elasticity counterfactual (full removal)
hours_semi <- calculate_semi_elasticity_counterfactual(semi_mod, "Fishing time", shrimp_panel)
cat("Semi-elasticity counterfactual calculated\n")
print(head(hours_semi))

# Elasticity counterfactual (range of reductions)
hours_elasticity <- calculate_elasticity_counterfactual(elasticity_mod, "Fishing time", shrimp_panel)
cat("\nElasticity counterfactual calculated\n")
print(head(hours_elasticity))

# Generate summaries
hours_semi_summary <- generate_counterfactual_summary(hours_semi, "Fishing time")
cat("\nSemi-elasticity summary:\n")
print(hours_semi_summary)

# Test with fishing area
cat("\n\n=== FISHING AREA ANALYSIS ===\n")

area_semi <- calculate_semi_elasticity_counterfactual(semi_mod, "Fishing area", shrimp_panel)
cat("Semi-elasticity counterfactual for area calculated\n")
print(head(area_semi))

area_elasticity <- calculate_elasticity_counterfactual(elasticity_mod, "Fishing area", shrimp_panel)
cat("\nElasticity counterfactual for area calculated\n")
print(head(area_elasticity))

# Test with landings
cat("\n\n=== LANDINGS ANALYSIS ===\n")

landings_semi <- calculate_semi_elasticity_counterfactual(semi_mod, "Landings", shrimp_panel)
cat("Semi-elasticity counterfactual for landings calculated\n")
print(head(landings_semi))

landings_elasticity <- calculate_elasticity_counterfactual(elasticity_mod, "Landings", shrimp_panel)
cat("\nElasticity counterfactual for landings calculated\n")
print(head(landings_elasticity))
