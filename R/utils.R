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

plot_glmnet_coefs <- function(fitted_wf, title = NULL, subtitle = NULL, caption = T) {
  df <- 
    fitted_wf %>% 
    pull_workflow_fit() %>% 
    tidy() %>% 
    filter(term != "(Intercept)")
  
  df_non_zero <- 
    df %>% 
    filter(estimate != 0)
  
  nb_zero_coef <- nrow(df) - nrow(df_non_zero)

  p <- 
    df_non_zero %>% 
    mutate(Sign = if_else(estimate > 0, "+", "-")) %>% 
    mutate(abs_estimate = abs(estimate)) %>% 
    mutate(term = fct_reorder(term, abs_estimate)) %>% 
    ggplot(aes(x = term, y = abs_estimate, fill = Sign)) +
    geom_col(alpha = 0.7) +
    xlab(NULL) +
    ylab("Absolute value of coefficient") +
    scale_fill_manual(values = c("#a61d21", "#00743F")) +
    coord_flip()
  
  # if(caption) {
  #   p + labs(caption = glue("Note: {nb_zero_coef} other coefficients are zero"))
  # }
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

compute_auc_bootstrap <- function(fit, new_data, nboot) {
  set.seed(1994)
  boot <- bootstraps(new_data, times = nboot, strata = claim_ind)
  boot_id_df <- 
    map_dfc(seq_len(nboot), ~ boot$splits[[.]]$in_id)
  
  auc_vec <- vector(mode = "double", length = nboot)
  
  for(i in seq_len(nboot)) {
    dat <- slice(new_data, boot_id_df[[i]])
    auc_vec[i] <- compute_auc(fit, new_data = dat)
    print(glue("{i} / {nboot}"))
  }
  
  return(auc_vec)
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

plot_rf_tuning <- function(tune_results, title = NULL, subtitle = NULL) {
  dat <- 
    tune_results %>%
    collect_metrics() %>%
    filter(.metric == "roc_auc") %>%
    mutate(best_param = mean == max(mean))
  
  dat_best_param <-
    dat %>%
    filter(best_param) %>%
    filter(mtry == max(mtry)) %>%
    filter(min_n == max(min_n))
  
  best_mtry <- signif(dat_best_param$mtry, 2)
  best_min_n <- signif(dat_best_param$min_n, 2)
  best_auc <- signif(dat_best_param$mean, 4)
  
  lab <- glue("mtry = {best_mtry}\nmin_n = {best_min_n}\nAUC = {best_auc}")
  
  ggplot(dat, aes(mtry, mean, color = factor(min_n))) +
    geom_line(alpha = 0.6, size = 1.1) +
    geom_point() +
    annotate(
      "label", 
      x = min(dat$mtry) + 0.75 * (max(dat$mtry) - min(dat$mtry)),
      y = min(dat$mean) + 0.75 * (max(dat$mean) - min(dat$mean)),
      label = lab, 
      hjust = 0.5, 
      color = "red", 
      size = 2.1,
      alpha = 0.9,
      family = "Roboto"
    ) +  
    ggtitle(title) +
    labs(y = "AUC", color = "min_n", subtitle = subtitle) +
    coord_cartesian(clip = "off")
}


# Fonction pour obtenir les coefficients d'une régression logistique LASSO ======================================================
get_glmnet_coefs <- function(wf, data, tune_results) {
  fit_wf_best_params(wf, tune_results, data) %>% 
  pull_workflow_fit() %>%
  tidy()
}

# Faire un data frame à partir d'une liste de coefficients ======================================================================
get_df_from_param_ls <- function(param_ls) {
  map(param_ls, select, -penalty) %>% 
    reduce(inner_join, by = "term") %>% 
    set_names(c("variable", glue("...{1:500}"), "original"))
}
