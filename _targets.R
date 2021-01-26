library(targets)
library(tarchetypes)
library(silgelib)
library(fs)
library(here)
library(purrr)
library(tidyverse)
library(readr)
library(tidymodels)
options(scipen = 999)

theme_set(theme_roboto())


# Lire les fonctions ============================================================================================================
walk(dir_ls("R"), source)


# Options =======================================================================================================================
tar_option_set(
  packages = c(
    "ggplot2",
    "Cairo",
    "dtplyr",
    "lubridate",
    "hms",
    "glue",
    "magrittr",
    "viridis",
    "embed",
    "glmnet",
    "magrittr",
    "glue",
    "hms",
    "fastDummies"
  ),
  garbage_collection = TRUE,
  memory = "transient",
  format = "qs"
)


# Targets =======================================================================================================================
list(
  tar_target(contract_file, "data/raw/Contrat_Nov2020.csv", format = "file"),
  tar_target(contract_data, prepare_contract_data(contract_file)),
  tar_target(claim_data, prepare_claim_data(contract_data)),
  tar_files_input(trip_files, list.files(here("data", "raw"), pattern = "TRIP_VIN", full.names = T), format  = "file"),
  tar_target(trip_data, clean_trip_file(trip_files), pattern = map(trip_files)),
  tar_target(contracts_claims_trips_data, join_contracts_claims_trips(contract_data, claim_data, trip_data), pattern = map(trip_data)),
  tar_target(df_list_months, create_df_list_months(contracts_claims_trips_data), pattern = map(contracts_claims_trips_data), iteration = "list"),
  tar_target(df_list_km, create_df_list_km(contracts_claims_trips_data), pattern = map(contracts_claims_trips_data), iteration = "list"),
  tar_target(df_list_months_merged, reduce(df_list_months, ~ map2(.x, .y, bind_rows))),
  tar_target(train_test_months_lists, split_train_test(df_list_months_merged)),
  tar_target(train_12_months, train_test_months_lists$train[[13]]),
  tar_target(test_12_months, train_test_months_lists$test[[13]]),
  
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    recipe_classic_tele, 
    {
      recipe(claim_ind ~ ., data = train_12_months) %>% 
        step_rm(vin, contract_start_date, contract_end_date, c_expo, nb_claims) %>%
        step_lencode_glm(all_nominal(), -all_outcomes(), outcome = vars(claim_ind)) %>%
        step_bagimpute(c_commute_distance) %>%
        step_normalize(all_predictors()) %>%
        step_YeoJohnson(all_predictors())
    }
  ),
  
  tar_target(
    recipe_interactions,
    {
      recipe(claim_ind ~ ., data = train_12_months) %>% 
        step_rm(vin, contract_start_date, contract_end_date, c_expo, nb_claims, all_of(covariates_to_exclude)) %>%
        step_lencode_glm(all_nominal(), -all_outcomes(), outcome = vars(claim_ind)) %>%
        step_bagimpute(c_commute_distance) %>%
        step_interact(terms = ~ all_numeric():all_numeric()) %>%
        step_normalize(all_predictors()) %>%
        step_YeoJohnson(all_predictors())
    }
  ),
  
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(resamples_train_12_months, create_stratified_5_folds(train_12_months)),
  tar_target(lambda_grid, grid_regular(penalty(), levels = 50)),
  tar_target(glmnet_grid, grid_regular(penalty(), mixture(), levels = c(50, 5))),
  
  tar_target(
    lasso_tune_spec, 
    logistic_reg(
      penalty = tune(),
      mixture = 1
    ) %>%
      set_engine("glmnet")
  ),
  
  tar_target(
    glmnet_tune_spec, 
    logistic_reg(
      penalty = tune(),
      mixture = tune()
    ) %>%
      set_engine("glmnet")
  ),
  
  tar_target(
    rf_tune_spec,
    rand_forest(
      mtry = tune(),
      trees = 1000,
      min_n = tune()
    ) %>%
      set_engine("ranger") %>%
      set_mode("classification")
  ),
  
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    rf_param,
    rf_wf %>%
      parameters() %>%
      update(mtry = mtry(range = c(1, 24)))
  ),

  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    lasso_wf,
    workflow() %>%
      add_model(lasso_tune_spec) %>%
      add_recipe(recipe_classic_tele)
  ),

  tar_target(
    lasso_interactions_wf,
    workflow() %>%
      add_model(lasso_tune_spec) %>%
      add_recipe(recipe_interactions)
  ),

  tar_target(
    glmnet_wf,
    workflow() %>%
      add_model(glmnet_tune_spec) %>%
      add_recipe(recipe_classic_tele)
  ),

  tar_target(
    glmnet_interactions_wf,
    workflow() %>%
      add_model(glmnet_tune_spec) %>%
      add_recipe(recipe_interactions)
  ),

  tar_target(
    rf_wf,
    workflow() %>%
      add_model(rf_tune_spec) %>%
      add_recipe(recipe_classic_tele)
  ),

  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    lasso_tuning,
    tune_grid(
      lasso_wf,
      resamples = resamples_train_12_months,
      grid = lambda_grid,
      metrics = metric_set(roc_auc),
      control = control_grid(save_pred = T)
    )
  ),

  tar_target(
    lasso_interactions_tuning,
    tune_grid(
      lasso_interactions_wf,
      resamples = resamples_train_12_months,
      grid = lambda_grid,
      metrics = metric_set(roc_auc),
      control = control_grid(save_pred = T)
    )
  ),

  tar_target(
    glmnet_tuning,
    tune_grid(
      glmnet_wf,
      resamples = resamples_train_12_months,
      grid = glmnet_grid,
      metrics = metric_set(roc_auc),
      control = control_grid(save_pred = T)
    )
  ),

  tar_target(
    glmnet_interactions_tuning,
    tune_grid(
      glmnet_interactions_wf,
      resamples = resamples_train_12_months,
      grid = glmnet_grid,
      metrics = metric_set(roc_auc),
      control = control_grid(save_pred = T)
    )
  ),

  tar_target(
    rf_tuning,
    tune_bayes(
      rf_wf,
      resamples = resamples_train_12_months,
      metrics = metric_set(roc_auc),
      iter = 50,
      initial = 10,
      param_info = rf_param,
      control = control_bayes(save_pred = TRUE, verbose = T, seed = 2021)
    )
  ),

  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    lasso_best_fit,
    {
      best_lambda <-
        lasso_tuning %>%
        select_best(metric = "roc_auc")

      lasso_wf %>%
        finalize_workflow(best_lambda) %>%
        fit(data = train_12_months)
    }
  ),

  tar_target(
    lasso_interactions_best_fit,
    {
      best_lambda <-
        lasso_interactions_tuning %>%
        select_best(metric = "roc_auc")

      lasso_interactions_wf %>%
        finalize_workflow(best_lambda) %>%
        fit(data = train_12_months)
    }
  ),

  tar_target(
    glmnet_best_fit,
    {
      best_params <-
        glmnet_tuning %>%
        select_best(metric = "roc_auc")

      glmnet_wf %>%
        finalize_workflow(best_params) %>%
        fit(data = train_12_months)
    }
  ),

  tar_target(
    glmnet_interactions_best_fit,
    {
      best_params <-
        glmnet_interactions_tuning %>%
        select_best(metric = "roc_auc")

      glmnet_interactions_wf %>%
        finalize_workflow(best_params) %>%
        fit(data = train_12_months)
    }
  ),

  tar_target(
    rf_best_fit,
    {
      best_params <-
        rf_tuning %>%
        select_best(metric = "roc_auc")

      rf_wf %>%
        finalize_workflow(best_params) %>%
        fit(data = train_12_months)
    }
  ),
  
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    lasso_tuning_plot,
    {
      plot <-
        plot_tuning_lasso(
          lasso_tuning,
          title = "LASSO logistic regression tuning results",
          subtitle = "10 classic covariates + 14 telematic covariates"
        )
      ggsave(here("figures", "lasso_tuning_plot.png"), plot, width = 10)
      here("figures", "lasso_tuning_plot.png")
    }
  ),

  tar_target(
    lasso_interactions_tuning_plot,
    {
      plot <-
        plot_tuning_lasso(
          lasso_interactions_tuning,
          title = "LASSO logistic regression tuning results",
          subtitle = "9 classic covariates + 14 telematic covariates + 253 interactions"
        )
      ggsave(here("figures", "lasso_interactions_tuning_plot.png"), plot, width = 10)
      here("figures", "lasso_interactions_tuning_plot.png")
    }
  ),
  
  # -----------------------------------------------------------------------------------------------------------------------------

  tar_target(
    lasso_coefs_plot,
    {
      plot <- plot_glmnet_coefs(lasso_best_fit)
      ggsave(here("figures", "lasso_coefs.png"), plot, width = 10)
      here("figures", "lasso_coefs.png")
    },
    format = "file"
  ),

  tar_target(
    lasso_interactions_coefs_plot,
    {
      plot <- plot_glmnet_coefs(lasso_interactions_best_fit)
      ggsave(here("figures", "lasso_interactions_coefs.png"), plot, width = 10)
      here("figures", "lasso_interactions_coefs.png")
    },
    format = "file"
  ),

  tar_target(
    glmnet_coefs_plot,
    {
      plot <- plot_glmnet_coefs(glmnet_best_fit)
      ggsave(here("figures", "glmnet_coefs.png"), plot, width = 10)
      here("figures", "glmnet_coefs.png")
    },
    format = "file"
  ),

  tar_target(
    glmnet_interactions_coefs_plot,
    {
      plot <- plot_glmnet_coefs(glmnet_interactions_best_fit)
      ggsave(here("figures", "glmnet_interactions_coefs.png"), plot, width = 10)
      here("figures", "glmnet_interactions_coefs.png")
    },
    format = "file"
  ),
  
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    covariates_to_exclude,
    lasso_best_fit %>% pull_workflow_fit() %>% tidy() %>% filter(estimate == 0) %>% pull(term)
  ),

  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    roc_curves_cv_plot,
    {
      roc_lasso <- compute_tune_results_roc(lasso_tuning, model_name = "LASSO without interaction")
      roc_lasso_interactions <- compute_tune_results_roc(lasso_interactions_tuning, model_name = "LASSO with interactions")
      roc_glmnet <- compute_tune_results_roc(glmnet_tuning, model_name = "GLMNET without interaction")
      roc_glmnet_interactions <- compute_tune_results_roc(glmnet_interactions_tuning, model_name = "GLMNET with interactions")
      roc_rf <- compute_tune_results_roc(rf_tuning, model_name = "Random forest")

      plot <-
        bind_rows(roc_lasso, roc_lasso_interactions, roc_glmnet, roc_glmnet_interactions, roc_rf) %>%
        ggplot(aes(x = 1 - specificity, y = sensitivity, col = model)) +
        geom_path(lwd = 1.2, alpha = 0.8) +
        geom_abline(lty = 3) +
        coord_equal() +
        labs(subtitle = "Cross-validation ROC curves with best hyperparameters", col = NULL) +
        ggtitle("Results for the 12-month training dataset") +
        scale_color_viridis_d(option = "plasma", end = .6)

      ggsave(here("figures", "cv_roc_curves.png"), plot, width = 10)
      here("figures", "cv_roc_curves.png")
    }
  ),
  
  tar_target(
    roc_curves_test_plot,
    {
      roc_lasso <- compute_roc(lasso_best_fit, new_data = test_12_months, model_name = "LASSO without interaction")
      roc_lasso_interactions <- compute_roc(lasso_interactions_best_fit, new_data = test_12_months, model_name = "LASSO with interaction")
      roc_glmnet <- compute_roc(glmnet_best_fit, new_data = test_12_months, model_name = "GLMNET without interaction")
      roc_glmnet_interactions <- compute_roc(glmnet_interactions_best_fit, new_data = test_12_months, model_name = "GLMNET with interactions")
      roc_rf <- compute_roc(rf_best_fit, new_data = test_12_months, model_name = "Random forest")
      
      plot <-
        bind_rows(roc_lasso, roc_lasso_interactions, roc_glmnet, roc_glmnet_interactions, roc_rf) %>%
        ggplot(aes(x = 1 - specificity, y = sensitivity, col = model)) +
        geom_path(lwd = 1.2, alpha = 0.8) +
        geom_abline(lty = 3) +
        coord_equal() +
        labs(subtitle = "Test set ROC curves", col = NULL) +
        ggtitle("Results for the 12-month test dataset") +
        scale_color_viridis_d(option = "plasma", end = .6)
      
      ggsave(here("figures", "test_roc_curves.png"), plot, width = 10)
      here("figures", "test_roc_curves.png")
    }
  )
)

