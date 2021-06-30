plot_scatter_trips <- function(trip_data) {
  get_nb_days_since_monday <- function(datetime) {
    wday(datetime, week_start = 1) + hour(datetime) / 24 + minute(datetime) / 1440 + second(datetime) / 86400 - 1
  }
  
  vin <- unique(trip_data$vin)
  date_start <- unique(trip_data$contract_start_date)
  date_end <- unique(trip_data$contract_end_date)
  
  q90 <- quantile(trip_data$distance, probs = 0.975)
  
  plot <- 
    trip_data %>% 
    mutate(days_since_monday = get_nb_days_since_monday(datetime_start)) %>% 
    ggplot(aes(x = days_since_monday, y = distance)) +
    geom_hex(bins = 90) +
    geom_point(alpha = 0) +
    scale_x_continuous(breaks = seq(0, 6, by = 1), labels = c("L", "M", "M", "J", "V", "S", "D")) +
    scale_y_continuous(limits = c(0, q90)) +
    scale_fill_gradient(low = "lightgrey", high = "black") +
    ylab("Distance (km)") +
    xlab(NULL) +
    labs(subtitle = glue("Véhicule {vin}\n Contrat commençant le {date_start} et finissant le {date_end}\n {nrow(trip_data)} trajets")) +
    theme(legend.title = element_blank())
  
  ggMarginal(
    plot, 
    type = "histogram", 
    size = 8, 
    xparams = list(fill = "white", binwidth = 1 / 24, alpha = 0.5),
    yparams = list(fill = "white", bins = 50, alpha = 0.5)
  )
}
