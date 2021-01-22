join_contracts_claims_trips <- function(contract_data, claim_data, trip_data) {
  contracts_and_trips_data <- 
    trip_data %>% 
    left_join(contract_data %>% select(vin:years_licensed), by = "vin") %>% 
    filter(date_start >= contract_start_date) %>% 
    filter(date_start < contract_end_date) 
  
  claim_data <- 
    claim_data %>% 
    group_by(vin) %>% 
    summarise(nb_claims = n())
  
  contracts_claims_trips <- 
    left_join(contracts_and_trips_data, claim_data, by = "vin") %>% 
    mutate(nb_claims = replace_na(nb_claims, 0)) %>% 
    mutate(claim_ind = factor(as.numeric(nb_claims > 0), levels = c("0", "1")))
  
  return(contracts_claims_trips)
}

