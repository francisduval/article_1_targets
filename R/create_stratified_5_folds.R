create_stratified_5_folds <- function(data) {
  set.seed(2021)
  vfold_cv(data, v = 5, strata = claim_ind)
}