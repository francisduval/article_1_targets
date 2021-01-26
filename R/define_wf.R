define_wf <- function(model_spec, recipe) {
  workflow() %>%
    add_model(model_spec) %>%
    add_recipe(recipe)
}