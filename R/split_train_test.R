split_train_test <- function(df_list_total) {
  # Ne garder que les VINs présents dans tous les mois 
  vins_to_keep <- map(df_list_total, "vin") %>% reduce(intersect)
  df_list <- map(df_list_total, filter, vin %in% vins_to_keep)
  
  # Créer l'ensemble d'entrainement et de validation
  set.seed(2020)
  vins_to_keep_shuffled <- sample(vins_to_keep)
  train_ls <- map(df_list, filter, vin %in% vins_to_keep_shuffled[1:(round(length(vins_to_keep) * 0.7))])
  test_ls <- map(df_list, filter, vin %in% vins_to_keep_shuffled[(round(length(vins_to_keep) * 0.7) + 1):length(vins_to_keep)])
  
  return(list(total = df_list, train = train_ls, test = test_ls))
}