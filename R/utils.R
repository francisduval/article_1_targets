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
