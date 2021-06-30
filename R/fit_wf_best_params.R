fit_wf_best_params <- function(wf, tune_results, data) {
  best_params <-
    tune_results %>%
    select_best(metric = "roc_auc")
  
  wf %>%
    finalize_workflow(best_params) %>%
    fit(data = data)
}
