library(targets)
library(tarchetypes)
library(tidymodels)
library(embed)
library(ggplot)
library(forcats)
library(silgelib)
library(gridExtra)
theme_set(theme_roboto())

# ===============================================================================================================================

train <- tar_read(train_12_months)
rec <-
  recipe(claim_ind ~ ., data = train) %>% 
  step_rm(vin, contract_start_date, contract_end_date, c_expo, nb_claims) %>%
  step_lencode_glm(all_nominal(), -all_outcomes(), outcome = vars(claim_ind))

rec_prep <- prep(rec)
train_baked <- bake(rec_prep, new_data = train)

# ===============================================================================================================================

make_plot <- function(data, subtitle = NULL) {
  ggplot(data, aes(x = valeur, y = valeur_encodee)) +
    geom_col(fill = "blue") +
    labs(subtitle = subtitle) +
    xlab(NULL) +
    ylab("Valeur encodée") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
}

# ===============================================================================================================================

gender <- 
  bind_cols(valeur_encodee = train_baked$c_gender, valeur = train$c_gender) %>% 
  group_by(valeur) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(valeur = fct_reorder(valeur, valeur_encodee, .desc = T))

marital_status <- 
  bind_cols(valeur_encodee = train_baked$c_marital_status, valeur = train$c_marital_status) %>% 
  group_by(valeur) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(valeur = fct_reorder(valeur, valeur_encodee, .desc = T))

pmt_plan <- 
  bind_cols(valeur_encodee = train_baked$c_pmt_plan, valeur = train$c_pmt_plan) %>% 
  group_by(valeur) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(valeur = fct_reorder(valeur, valeur_encodee, .desc = T))

veh_use <- 
  bind_cols(valeur_encodee = train_baked$c_veh_use, valeur = train$c_veh_use) %>% 
  group_by(valeur) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(valeur = fct_reorder(valeur, valeur_encodee, .desc = T))

# ===============================================================================================================================

p1 <- make_plot(gender, subtitle = "gender")
p2 <- make_plot(marital_status, subtitle = "marital_status")
p3 <- make_plot(pmt_plan, subtitle = "pmt_plan")
p4 <- make_plot(veh_use, subtitle = "veh_use")

# ===============================================================================================================================

grid.arrange(p1, p2, p3, p4, nrow = 2, top = "Target encoding")
