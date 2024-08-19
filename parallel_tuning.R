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

extract_parameter_set_dials(bt_wflow)
## Collection of 4 parameters for tuning
## 
##  identifier       type    object
##  learn_rate learn_rate nparam[+]
##   stop_iter  stop_iter nparam[+]
##  num_leaves num_leaves nparam[+]
##    deg_free   deg_free nparam[+]

set.seed(3)

bt_time_grid <- system.time(
  bt_res_grid <- tune_grid(bt_wflow, ames_folds, grid = 50)
)

bt_res_grid
## # Tuning results
## # 20-fold cross-validation 
## # A tibble: 20 × 4
##    splits             id     .metrics           .notes          
##    <list>             <chr>  <list>             <list>          
##  1 <split [2087/110]> Fold01 <tibble [100 × 8]> <tibble [0 × 3]>
##  2 <split [2087/110]> Fold02 <tibble [100 × 8]> <tibble [0 × 3]>
##  3 <split [2087/110]> Fold03 <tibble [100 × 8]> <tibble [0 × 3]>
##  4 <split [2087/110]> Fold04 <tibble [100 × 8]> <tibble [0 × 3]>
##  5 <split [2087/110]> Fold05 <tibble [100 × 8]> <tibble [0 × 3]>
##  6 <split [2087/110]> Fold06 <tibble [100 × 8]> <tibble [0 × 3]>
##  7 <split [2087/110]> Fold07 <tibble [100 × 8]> <tibble [0 × 3]>
##  8 <split [2087/110]> Fold08 <tibble [100 × 8]> <tibble [0 × 3]>
##  9 <split [2087/110]> Fold09 <tibble [100 × 8]> <tibble [0 × 3]>
## 10 <split [2087/110]> Fold10 <tibble [100 × 8]> <tibble [0 × 3]>
## 11 <split [2087/110]> Fold11 <tibble [100 × 8]> <tibble [0 × 3]>
## 12 <split [2087/110]> Fold12 <tibble [100 × 8]> <tibble [0 × 3]>
## 13 <split [2087/110]> Fold13 <tibble [100 × 8]> <tibble [0 × 3]>
## 14 <split [2087/110]> Fold14 <tibble [100 × 8]> <tibble [0 × 3]>
## 15 <split [2087/110]> Fold15 <tibble [100 × 8]> <tibble [0 × 3]>
## 16 <split [2087/110]> Fold16 <tibble [100 × 8]> <tibble [0 × 3]>
## 17 <split [2087/110]> Fold17 <tibble [100 × 8]> <tibble [0 × 3]>
## 18 <split [2088/109]> Fold18 <tibble [100 × 8]> <tibble [0 × 3]>
## 19 <split [2088/109]> Fold19 <tibble [100 × 8]> <tibble [0 × 3]>
## 20 <split [2088/109]> Fold20 <tibble [100 × 8]> <tibble [0 × 3]>

autoplot(bt_res_grid)

collect_metrics(bt_res_grid) %>%
  filter(.metric == "rmse") %>%
  arrange(mean)
## # A tibble: 50 × 10
##    learn_rate stop_iter num_leaves deg_free .metric .estimator   mean     n
##         <dbl>     <int>      <int>    <int> <chr>   <chr>       <dbl> <int>
##  1    0.00713        19         45       15 rmse    standard   0.0751    20
##  2    0.0211          7         17        2 rmse    standard   0.0759    20
##  3    0.00960        16         82       12 rmse    standard   0.0760    20
##  4    0.0129         12         65        4 rmse    standard   0.0763    20
##  5    0.00317         5         37       11 rmse    standard   0.0764    20
##  6    0.0416          3         72       13 rmse    standard   0.0784    20
##  7    0.0494         11         57        9 rmse    standard   0.0801    20
##  8    0.00198        15         71        6 rmse    standard   0.0811    20
##  9    0.00473         7          6       10 rmse    standard   0.0812    20
## 10    0.0921          6         53       12 rmse    standard   0.0816    20
## # ℹ 40 more rows
## # ℹ 2 more variables: std_err <dbl>, .config <chr>

library(future)
plan(multisession)

bt_time_par <- system.time(
  bt_res_par <- tune_grid(bt_wflow, ames_folds, grid = 50)
)


library(finetune)

set.seed(5)

bt_time_race <- system.time(
  bt_res_race <- tune_race_anova(bt_wflow, ames_folds, grid = 50)
)

plot_race(bt_res_race)
