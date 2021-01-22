library(targets)
library(tarchetypes)
library(ggplot2)
library(silgelib)
library(fs)
library(here)
library(purrr)
library(tidyverse)
library(Cairo)
options(scipen = 999)

theme_set(theme_roboto())


# Lire les fonctions ============================================================================================================
walk(dir_ls("R"), source)


# Librairies ====================================================================================================================
tar_option_set(
  packages = c(
    "tidyverse",
    "ggplot2",
    "here",
    "dtplyr",
    "lubridate",
    "hms",
    "fastDummies",
    "glue",
    "magrittr",
    "readr",
    "tidymodels",
    "poissonreg",
    "silgelib",
    "Cairo",
    "hrbrthemes",
    "GGally",
    "viridis",
    "cvAUC",
    "fs",
    "progress",
    "gridExtra",
    "embed",
    "Matrix",
    "glmnet"
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
  tar_target(recipe_interactions, define_recipe_interactions(train_12_months)),
  tar_target(
    lasso_tune_spec, 
    logistic_reg(
      penalty = tune(),
      mixture = 1
    ) %>%
      set_engine("glmnet")
  ),
  tar_target(
    lasso_interactions_wf,
    workflow() %>%
      add_model(lasso_tune_spec) %>%
      add_recipe(recipe_interactions)
  ),
  tar_target(
    resamples_cv_5, 
    {
      set.seed(2021)
      vfold_cv(train_12_months, v = 5, strata = claim_ind)
    }
  ),
  tar_target(lambda_grid, grid_regular(penalty(), levels = 50)),
  tar_target(
    lasso_interactions_tuning,
    tune_grid(
      lasso_interactions_wf,
      resamples = resamples_cv_5,
      grid = lambda_grid,
      metrics = metric_set(roc_auc)
    )  
  ),
  tar_target(
    lasso_interactions_tuning_plot,
    {
      plot <- 
        plot_tuning_lasso(
          lasso_interactions_tuning, 
          title = "LASSO logistic regression tuning results", 
          subtitle = "With pairwise interactions"
        )
      ggsave(here("figures", "lasso_interactions_tuning_plot.png"), plot, width = 10)
      here("figures", "lasso_interactions_tuning_plot.png")
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
    coefs_plot_lasso_interactions,
    {
      plot <- plot_glmnet_coefs(lasso_interactions_best_fit)
      ggsave(here("figures", "lasso_interactions_coefs.png"), plot, width = 10)
      here("figures", "lasso_interactions_coefs.png")
    },
    format = "file"
  )
)




