library(parsnip)
library(tune)
library(dplyr)
library(modeldata)
library(rsample)
library(recipes)
library(workflows)

ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))

set.seed(1)

ames_split <- initial_split(ames, strata = "Sale_Price")
ames_train <- training(ames_split)
ames_test <- testing(ames_split)

set.seed(2)
ames_folds <- vfold_cv(ames_train, v = 20)

library(bonsai)

bt_spec <-
  boost_tree(learn_rate = tune(), stop_iter = tune(), trees = 1000) %>%
  set_engine("lightgbm", num_leaves = tune()) %>%
  set_mode("regression")

bt_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.05) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_")) %>% 
  step_ns(Latitude, Longitude, deg_free = tune())

bt_wflow <- workflow(bt_rec, bt_spec)

set.seed(3)


library(future)

system("srun hostname >> hostfile")
hostname <- readLines("hostfile")

plan(cluster,
     workers = rep(hostname, 128))

bt_time_par <- system.time(
  bt_res_par <- tune_grid(bt_wflow, ames_folds, grid = 50)
)


library(finetune)

set.seed(5)

bt_time_race <- system.time(
  bt_res_race <- tune_race_anova(bt_wflow, ames_folds, grid = 50)
)

print(bt_time_par)
print(bt_time_race)
