library(dials)
library(tidymodels)
library(bonsai)
library(xgboost)
library(vip)
# library(butcher) # reduce size of exported model
# library(bundle) #save model in more reliable way



## Notes

## knn test accuracy 0.41 rsq 2 params
## bag test accuracy 0.46 rsq 2 params

# surv = readr::read_csv("clean_survey.csv")


devwages_split <- initial_split(surv)
devwages_other <- training(devwages_split)
devwages_test  <- testing(devwages_split)

set.seed(234)
trees_folds <- vfold_cv(devwages_other, v = 5)


## Build recipe

rec <- recipes::recipe(AnnualNetSalary ~.,
                       data = devwages_other) %>%
  step_mutate(AnnualNetSalary,
             AnnualNetSalary = if_else(AnnualNetSalary < 3000, NA_real_, AnnualNetSalary),
             skip = TRUE) %>%
  step_impute_bag(all_predictors())  %>%
  step_impute_bag(all_outcomes(), skip = TRUE) %>%
  step_dummy(all_nominal_predictors(), -c(Company, Studies)) %>%
  step_mutate(across(where(is.logical), as.numeric)) |>
  step_ordinalscore(c(Company, Studies)) |>
  step_range(all_numeric_predictors(),min = 0, max = 1) |>
  step_nzv(all_predictors()) |>
  step_zv(all_predictors())



encoded_data1 <- prep(rec) %>% bake(new_data = NULL)
encoded_data1


encoded_data1$AnnualNetSalary

xgb_spec <- boost_tree(
             trees = tune(), 
             min_n = tune(),
             tree_depth = tune()
             ) %>% 
  # Engine
  set_engine("xgboost") %>% 
  # Mode
  set_mode("regression")




xgb_params <- dials::parameters(
    # The parameters have sane defaults, but if you have some knowledge
    # of the process you can set upper and lower limits to these parameters.
    trees(), 
    min_n(),
    tree_depth()
)

xgb_grid <-
  dials::grid_max_entropy(
    xgb_params,
    size = 10
  )
xgb_grid

xgb_wf <-
  workflows::workflow() %>%
  add_recipe(rec) %>%
  add_model(xgb_spec)


# Model tuning via grid search
set.seed(345)

grid = tune_grid(
  object = xgb_wf,
  resamples = trees_folds,
  grid = xgb_grid,
  metrics = yardstick::metric_set(rmse),
  control = tune::control_grid(verbose = T)
)

grid %>% 
  collect_metrics()


best_tree <- grid %>% 
  select_best(metric =  "rmse")


best_tree

final_wf <- xgb_wf %>% 
  finalize_workflow(best_tree)


final_wf

final_wf %>%
  fit(data = devwages_other) %>%
  extract_fit_parsnip() %>%
  vip(geom = "point")

# Make a last fit
final_fit <- final_wf %>% 
  last_fit(devwages_split)

# Collect metrics
final_fit %>% 
  collect_metrics()

final_model_rf <- extract_workflow(final_fit)

xgb_bundle = bundle::bundle(final_model_rf)
saveRDS(xgb_bundle, "devwages.rds")


## Test data prediction

test_data = dplyr::tibble(
  YearsProgr = 10,
  PersonalProj = "Yes",
  Gender = "Woman",
  Company = "501plus",
  Supervising = "Yes",
  Employer = "Greece",
  observation_count = 1,
  WorkType = "Onsite",
  Studies = "Bachelor",
  Employee = "Greece",
  RelativeComp = "Other")

predict(xgb_bundle, new_data = test_data)

