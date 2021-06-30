make_hist_num_vars_classic <- function(data) {
  dat <- 
    data %>% 
    select_at(vars(starts_with("c_"))) %>%
    select_if(is.numeric) %>% 
    rename_all(~str_replace(., "^c_", "")) %>% 
    select(-expo) %>% 
    filter(annual_distance < quantile(annual_distance, 0.99)) %>% 
    filter(commute_distance < quantile(commute_distance, 0.99, na.rm = T)) %>% 
    pivot_longer(everything())
  
  ggplot(dat, aes(x = value)) + 
    geom_histogram(
      data = filter(dat, name == "annual_distance"), 
      binwidth = 1000,
      fill = "#192E5B", col = "#192E5B", alpha = 0.2
    ) +
    geom_histogram(
      data = filter(dat, name == "commute_distance"), 
      binwidth = 1,
      fill = "#192E5B", col = "#192E5B", alpha = 0.2
    ) +
    geom_histogram(
      data = filter(dat, name == "conv_count_3_yrs_minor"), 
      binwidth = 0.1,
      fill = "#192E5B", col = "#192E5B", alpha = 0.2
    ) +
    geom_histogram(
      data = filter(dat, name == "veh_age"), 
      binwidth = 1,
      fill = "#192E5B", col = "#192E5B", alpha = 0.2
    ) +
    geom_histogram(
      data = filter(dat, name == "years_claim_free"), 
      binwidth = 2,
      fill = "#192E5B", col = "#192E5B", alpha = 0.2
    ) +
    geom_histogram(
      data = filter(dat, name == "years_licensed"), 
      binwidth = 2,
      fill = "#192E5B", col = "#192E5B", alpha = 0.2
    ) +
    facet_wrap(~name, scales = "free") +
    xlab(NULL) +
    ylab("Number of contracts")
}
