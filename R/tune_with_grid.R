tune_with_grid <- function(wf, resamples, grid) {
  tune_grid(
    wf,
    resamples = resamples,
    grid = grid,
    metrics = metric_set(roc_auc),
    control = control_grid(save_pred = T)
  )
}