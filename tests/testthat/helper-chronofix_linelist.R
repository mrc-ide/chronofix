make_mock_samples <- function(id = "id", group = "group") {
  n_per_group <- 3
  groups <- c("community-alive", "hospitalised-alive", "community-dead")
  n_ind <- n_per_group * length(groups)
  n_evt <- 5
  n_samp <- 4
  
  observed_data <- data.frame(
    id = seq_len(n_ind),
    group = rep(groups, each = n_per_group),
    onset = as.Date(rep("2025-01-01", n_ind)),
    hospitalisation = as.Date(rep(NA, n_ind)),
    report = as.Date(rep("2025-01-10", n_ind)),
    death = as.Date(rep(NA, n_ind)),
    discharge = as.Date(rep(NA, n_ind))
  )
  
  names(observed_data)[names(observed_data) == "id"] <- id
  names(observed_data)[names(observed_data) == "group"] <- group
  
  hospitalised_alive <- observed_data[[group]] == "hospitalised-alive"
  observed_data$hospitalisation[hospitalised_alive] <- as.Date("2025-01-05")
  observed_data$discharge[hospitalised_alive] <- as.Date("2025-01-15")
  community_dead <- observed_data[[group]] == "community-dead"
  observed_data$death[community_dead] <- as.Date("2025-01-20")
  
  estimated_dates <- array(
    as.Date(NA_character_),
    dim = c(n_ind, n_evt, n_samp)
  )
  
  error_indicators <- array(NA, dim = c(n_ind, n_evt, n_samp))
  
  for (i in seq_len(n_ind)) {
    allowed_events <- switch(
      observed_data[[group]][i],
      "community-alive" = c(1, 3),
      "hospitalised-alive" = c(1, 2, 3, 5),
      "community-dead" = c(1, 3, 4)
    )
    
    for (e in allowed_events) {
      estimated_dates[i, e, ] <- 
        date_to_int(as.Date("2025-01-01") + e + seq_len(n_samp)) + runif(n_samp)
      error_indicators[i, e, ] <- c(FALSE, FALSE, TRUE, TRUE)
    }
  }
  
  rownames(estimated_dates) <- observed_data[[id]]
  colnames(estimated_dates) <- setdiff(names(observed_data), c(id, group))
  rownames(error_indicators) <- observed_data[[id]]
  colnames(error_indicators) <- setdiff(names(observed_data), c(id, group))
  
  samples <- list(
    data = chronofix_prepare_data(observed_data, id = id, group = group),
    augmented_data = list(
      estimated_dates = estimated_dates,
      error_indicators = error_indicators
    )
  )
  class(samples) <- "chronofix_mcmc_samples"
  
  samples
}
