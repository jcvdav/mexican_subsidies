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
library(here)
library(vip)
library(tidymodels)
library(tidyverse)

# Load data --------------------------------------------------------------------
full_panel <- readRDS(file = here("data", "processed", "shrimp_economic_unit_annual_panel.rds"))

## PROCESSING ##################################################################

# Build learning set -----------------------------------------------------------
panel <- full_panel %>%
  filter(treated) %>% 
  select(subsidy_cap_l, year, total_hp, n_vessels)

# Split into training and testing sets
set.seed(123)
sub_split <- panel %>% 
  initial_split(strata = year, prop = 0.60)

sub_train <- training(sub_split)
sub_test <- testing(sub_split)

# Recipe -----------------------------------------------------------------------
rec <- 
  recipe(subsidy_cap_l ~ ., data = sub_train) %>% 
  step_normalize(all_numeric_predictors())

# Specify which parameters need to be tuned
tune_spec <- 
  rand_forest(mtry = tune(),
              trees = tune(),
              min_n = tune()) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("regression")

# Build the grid
rf_grid <- grid_regular(mtry(range = c(1, 3)),
                        trees(),
                        min_n(),
                        levels = 3)

# Perform cross validation -----------------------------------------------------
set.seed(234)
sub_folds <- vfold_cv(data = sub_train, v = 5)

rf_wf <- workflow() %>%
  add_model(tune_spec) %>%
  add_recipe(rec)

rf_res <- 
  rf_wf %>% 
  tune_grid(resamples = sub_folds,
            grid = rf_grid)

# Get best model ---------------------------------------------------------------
# Get the best metaparameters
best_rf <- rf_res %>% 
  select_best("rmse")

# Apply workflow
final_wf <- 
  rf_wf %>% 
  finalize_workflow(best_rf)

# Now we fit it
final_fit <- 
  final_wf %>%
  last_fit(sub_split) 

# Show variable importance
imp <- final_fit %>% 
  pluck(".workflow", 1) %>%   
  extract_fit_parsnip() %>% 
  vip(num_features = 20)


model <- rand_forest(mtry = 3,
                     trees = 2000,
                     min_n = 2) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("regression") %>% 
  fit(subsidy_cap_l ~ ., panel)

# Make predictions -------------------------------------------------------------
predicted_panel <- full_panel %>% 
  mutate(predicted_subsidy_cap_l = predict(model, .)$.pred)

## EXPORT ######################################################################
saveRDS(object = predicted_panel,
        file = here("data", "processed", "imputed_subsidy_economic_unit_annual_shrimp_panel.rds"))
