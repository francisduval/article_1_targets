create_df_list_km <- function(contracts_claims_trips_data) {
  # Fonction pour calculer les variables télématiques
  compute_telematics_summaries <- function(contracts_claims_trips_data, nb_thousand_km) {
    contracts_claims_trips_data %>%
      group_by(vin) %>%
      mutate(distance_cum = cumsum(distance)) %>%
      filter(distance_cum <= 1000 * nb_thousand_km) %>% 
      mutate(nb_jours = as.numeric(last(date_start) - first(contract_start_date))) %>% 
      mutate(
        weekday = weekdays(date_start, abbreviate = T),
        is_weekend = weekday %in% c("Sat", "Sun"),
        duration_night_trip = (time_start >= as_hms("00:00:00") & time_start < as_hms("06:00:00")) * duration,
        duration_noon_trip = (time_start >= as_hms("11:00:00") & time_start < as_hms("14:00:00")) * duration,
        duration_evening_trip = (time_start >= as_hms("20:00:00") & time_start <= as_hms("23:59:59")) * duration,
        duration_peak_morning_trip = ((time_start >= as_hms("07:00:00") & time_start < as_hms("09:00:00")) & !is_weekend) * duration,
        duration_peak_evening_trip = ((time_start >= as_hms("17:00:00") & time_start < as_hms("20:00:00")) & !is_weekend) * duration,
        duration_mon_to_thu = (weekday %in% c("Mon", "Tue", "Wed", "Thu")) * duration,
        duration_fri_sat = (weekday %in% c("Fri", "Sat")) * duration,
        duration_sun = (weekday == "Sun") * duration
      ) %>% 
      rename(
        trip_avg_speed = avg_speed,
        trip_distance = distance,
        trip_duration = duration,
        trip_max_speed = max_speed
      ) %>% 
      summarise(
        contract_start_date      = first(contract_start_date),
        contract_end_date        = first(contract_end_date),
        c_expo                   = first(expo),
        c_annual_distance        = first(annual_distance),
        c_commute_distance       = first(commute_distance),
        c_conv_count_3_yrs_minor = first(conv_count_3_yrs_minor),
        c_gender                 = first(gender),
        c_marital_status         = first(marital_status),
        c_pmt_plan               = first(pmt_plan),
        c_veh_age                = first(veh_age),
        c_veh_use                = first(veh_use),
        c_years_claim_free       = first(years_claim_free),
        c_years_licensed         = first(years_licensed),
        t_avg_daily_distance     = sum(trip_distance) / first(nb_jours),
        t_avg_daily_nb_trips     = n() / first(nb_jours),
        t_med_trip_avg_speed     = median(trip_avg_speed),
        t_med_trip_distance      = median(trip_distance),
        t_med_trip_max_speed     = median(trip_max_speed),
        t_max_trip_max_speed     = max(trip_max_speed),
        t_prop_long_trip         = sum(trip_distance > 100) / n(),
        t_frac_expo_night        = sum(duration_night_trip) / sum(trip_duration),
        t_frac_expo_noon         = sum(duration_noon_trip) / sum(trip_duration),
        t_frac_expo_evening      = sum(duration_evening_trip) / sum(trip_duration),
        t_frac_expo_peak_morning = sum(duration_peak_morning_trip) / sum(trip_duration),
        t_frac_expo_peak_evening = sum(duration_peak_evening_trip) / sum(trip_duration),
        t_frac_expo_mon_to_thu   = sum(duration_mon_to_thu) / sum(trip_duration),
        t_frac_expo_fri_sat      = sum(duration_fri_sat) / sum(trip_duration),
        nb_claims                = first(nb_claims),
        claim_ind                = first(claim_ind)
      ) %>% 
      ungroup() %>% 
      filter(
        is.finite(t_avg_daily_distance), 
        is.finite(t_avg_daily_nb_trips), 
        !is.nan(t_avg_daily_distance),
        !is.nan(t_avg_daily_nb_trips),
        !is.na(c_years_claim_free)
      )
  }
  
  # Calculer les 12 bases de données avec variables télématiques
  df_ls_tele <- map(1:12, ~ compute_telematics_summaries(contracts_claims_trips_data, nb_thousand_km = .x))
  
  # Créer le jeu de données classique seulement
  df_classic <- df_ls_tele[[1]] %>% select(-starts_with("t_"))
  
  # Mettre les 12 jeux de télématiques et le jeu classique dans la même liste
  df_ls_final <-  prepend_element_to_list(df_ls_tele, element = df_classic)
  
  return(df_ls_final)
}
