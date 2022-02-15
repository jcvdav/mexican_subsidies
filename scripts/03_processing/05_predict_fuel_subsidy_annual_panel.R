
library(tidymodels)
library(tidyverse)

full_panel <- read_csv(file.path(project_path, "data", "processed_data", "economic_unit_annual_panel.csv"))

panel <- full_panel %>%
  filter(treated) %>% 
  select(subsidy_cap_l, year, total_hp, n_vessels, tuna, sardine, shrimp, others, ph)

set.seed(123)
sub_split <- panel %>% 
  initial_split(strata =year, prop = 0.60)

sub_train <- training(sub_split)
sub_test  <- testing(sub_split)

# Recipe
rec <- 
  recipe(subsidy_cap_l ~ ., data = sub_train) %>% 
  step_normalize(all_numeric_predictors())

# nrow(sub_train)
# nrow(sub_train)/nrow(panel)
# 
# sub_test %>% 
#   count(year) %>% 
#   mutate(pct = n / sum(n))

folds <- vfold_cv(sub_train, v = 10)

tune_spec <- 
  rand_forest(mtry = tune(),
              trees = tune(),
              min_n = tune()) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("regression")

rf_grid <- grid_regular(mtry(range = c(1, 8)),
                            trees(),
                            min_n(),
                            levels = 3)

set.seed(234)
sub_folds <- vfold_cv(data = sub_train, v = 5)


rf_wf <- workflow() %>%
  add_model(tune_spec) %>%
  add_recipe(rec)

rf_res <- 
  rf_wf %>% 
  tune_grid(resamples = sub_folds,
            grid = rf_grid)

# Get the best metaparameters
best_rf <- rf_res %>% 
  select_best("rmse")

final_wf <- 
  rf_wf %>% 
  finalize_workflow(best_rf)

# Now we fit it
final_fit <- 
  final_wf %>%
  last_fit(sub_split) 



a <- rand_forest(mtry = 4,
                 trees = 1000,
                 min_n = 2) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("regression") %>% 
  fit(subsidy_cap_l ~ ., panel)

final_fit %>% 
  pluck(".workflow", 1) %>%   
  extract_fit_parsnip() %>% 
  vip(num_features = 20)

predicted_panel <- full_panel %>% 
  mutate(predicted_subsidy_cap_l = predict(a, .)$.pred)

predicted_panel %>% 
  mutate(predicted = subsidy_cap_l > 0,
         subsidy_cap_l = ifelse(subsidy_cap_l == 0, predicted_subsidy_cap_l, subsidy_cap_l)) %>% 
  ggplot(mapping = aes(x = subsidy_cap_l / 1e3, y = predicted_subsidy_cap_l / 1e3, fill = predicted)) + 
  geom_point() +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Random forest predictions",
       subtitle = "Out-of-sample stats: R2 = 0.857, RMSE = 55756",
       x = "Observed subsidy cap (1000 L)",
       y = "Predicted subsidy cap (1000 L)",
       fill = "Predicted") +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1)) +
  scale_x_log10() +
  scale_y_log10() +
  coord_equal()

write_csv(x = predicted_panel,
          file = file.path(project_path, "data", "processed_data", "imputed_subsidy_economic_unit_annual_panel.csv"))
