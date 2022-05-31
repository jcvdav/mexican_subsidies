library(here)
library(vip)
library(tidymodels)
library(tidyverse)

full_panel <- read_csv(file.path(project_path, "data", "processed_data", "shrimp_economic_unit_annual_panel.csv"))

panel <- full_panel %>%
  filter(treated) %>% 
  select(subsidy_cap_l, year, total_hp, n_vessels)

set.seed(123)
sub_split <- panel %>% 
  initial_split(strata = year, prop = 0.60)

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

tune_spec <- 
  rand_forest(mtry = tune(),
              trees = tune(),
              min_n = tune()) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("regression")

rf_grid <- grid_regular(mtry(range = c(1, 3)),
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


imp <- final_fit %>% 
  pluck(".workflow", 1) %>%   
  extract_fit_parsnip() %>% 
  vip(num_features = 20)


a <- rand_forest(mtry = 3,
                 trees = 2000,
                 min_n = 2) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("regression") %>% 
  fit(subsidy_cap_l ~ ., panel)


predicted_panel <- full_panel %>% 
  mutate(predicted_subsidy_cap_l = predict(a, .)$.pred)

fit <- predicted_panel %>% 
  mutate(predicted = subsidy_cap_l > 0) %>% 
  ggplot(mapping = aes(x = subsidy_cap_l / 1e3, y = predicted_subsidy_cap_l / 1e3, fill = predicted)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Random forest predictions",
       subtitle = "Out-of-bag stats: R2 = 0.839, MSE = 48979279688",
       x = "Observed subsidy cap (1000 L)",
       y = "Predicted subsidy cap (1000 L)",
       fill = "Predicted") +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1)) +
  coord_equal()

ggsave(plot = fit,
       filename = here("results", "img", "rf_fit.pdf"),
       width = 6,
       height = 4.5)

write_csv(x = predicted_panel,
          file = file.path(project_path, "data", "processed_data", "imputed_subsidy_economic_unit_annual_shrimp_panel.csv"))
