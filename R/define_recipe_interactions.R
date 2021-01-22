define_recipe_interactions <- function(data) {
  recipe(claim_ind ~ ., data = data) %>% 
    step_rm(vin, contract_start_date, contract_end_date, c_expo, nb_claims) %>%
    step_lencode_glm(all_nominal(), -all_outcomes(), outcome = vars(claim_ind)) %>%
    step_bagimpute(c_commute_distance) %>%
    step_interact(terms = ~ starts_with("c_"):starts_with("c_")) %>%
    step_interact(terms = ~ starts_with("t_"):starts_with("t_")) %>%
    step_normalize(all_predictors()) %>%
    step_YeoJohnson(all_predictors())
}