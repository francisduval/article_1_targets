prepend_element_to_list <- function(the_list, element) {
  n <- length(the_list) + 1
  liste <- vector(mode = "list", length = n)
  
  liste[[1]] <- element
  
  for(i in 2:n) {
    liste[[i]] <- the_list[[i - 1]]
  }
  
  return(liste)
}

# ===============================================================================================================================

plot_glmnet_coefs <- function(fitted_wf, title = NULL, subtitle = NULL) {
  df <- 
    fitted_wf %>% 
    pull_workflow_fit() %>% 
    tidy() %>% 
    filter(term != "(Intercept)")
  
  df_non_zero <- 
    df %>% 
    filter(estimate != 0)
  
  nb_zero_coef <- nrow(df) - nrow(df_non_zero)

  df_non_zero %>% 
    mutate(Sign = if_else(estimate > 0, "+", "-")) %>% 
    mutate(abs_estimate = abs(estimate)) %>% 
    mutate(term = fct_reorder(term, abs_estimate)) %>% 
    ggplot(aes(x = term, y = abs_estimate, fill = Sign)) +
    geom_col(alpha = 0.7) +
    xlab(NULL) +
    ylab("Absolute value of coefficient") +
    labs(caption = glue("Note: {nb_zero_coef} other coefficients are zero")) +
    scale_fill_manual(values = c("darkred", "darkblue")) +
    coord_flip()
}

# ===============================================================================================================================

plot_tuning_lasso <- function(tune_results, title = NULL, subtitle = NULL) {
  tune_results %>%
    collect_metrics() %>%
    ggplot(aes(x = penalty, y = mean)) +
    geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err), alpha = 0.5) +
    geom_point(size = 1) +
    scale_x_log10(labels = trans_format("log10", math_format(10 ^ .x))) +
    scale_y_continuous(n.breaks = 10) +
    ggtitle(title) +
    labs(subtitle = subtitle) +
    xlab("Penalty") +
    ylab("Mean AUC")
}

# ===============================================================================================================================

compute_tune_results_auc <- function(tune_results) {
    tune_results %>% 
    collect_predictions(parameters = tune_results %>% select_best(metric = "roc_auc")) %>% 
    roc_auc(claim_ind, .pred_0) %>% 
    pull(.estimate)
}

# ===============================================================================================================================

compute_tune_results_roc <- function(tune_results, model_name = NULL) {
  auc <- compute_tune_results_auc(tune_results)
  
  tune_results %>% 
    collect_predictions(parameters = tune_results %>% select_best(metric = "roc_auc")) %>% 
    roc_curve(claim_ind, .pred_0) %>% 
    mutate(model = glue("{model_name} (AUC = {round(auc, 4)})"))
}

# ===============================================================================================================================

compute_auc <- function(fit, new_data) {
  predict(fit, new_data = new_data, type = "prob") %>% 
    bind_cols(claim_ind = new_data$claim_ind) %>% 
    roc_auc(claim_ind, .pred_0) %>% 
    pull(.estimate)
}

# ===============================================================================================================================

compute_roc <- function(fit, new_data, model_name = "NULL") {
  auc <- compute_auc(fit, new_data)
  
  predict(fit, new_data = new_data, type = "prob") %>% 
    bind_cols(claim_ind = new_data$claim_ind) %>% 
    roc_curve(claim_ind, .pred_0) %>% 
    mutate(model = glue("{model_name} (AUC = {round(auc, 4)})"))
}

# ===============================================================================================================================

plot_glm_coefs <- function(fit, title = NULL, subtitle = NULL) {
  dat <- 
    fit %>% 
    pull_workflow_fit() %>% 
    tidy() %>% 
    filter(term != "(Intercept)") %>% 
    mutate(signif = factor(if_else(p.value < 0.05, "Yes", "No")))
  
  ggplot(dat, aes(x = estimate, y = term, xmin = estimate - std.error, xmax = estimate + std.error, color = signif)) +
    geom_pointrange(alpha = 0.9, size = 0.5) +
    geom_vline(xintercept = 0, alpha = 0.5) +
    scale_color_manual("P-value < 0.05?", values = c("grey", "black")) +
    ggtitle(title) +
    labs(subtitle = subtitle) +
    ylab(NULL) +
    xlab("Estimate")
}

# ===============================================================================================================================
