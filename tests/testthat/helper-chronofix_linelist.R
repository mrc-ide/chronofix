make_mock_data <- function() {
  n_per_group <- 3
  groups <- c("community-alive", "hospitalised-alive", "community-dead")
  n_ind <- n_per_group * length(groups)
  n_evt <- 5
  n_iter <- 4
  n_chains <- 1
  
  observed_data <- data.frame(
    id = seq_len(n_ind),
    group = rep(groups, each = n_per_group),
    onset = as.Date(rep("2025-01-01", n_ind)),
    hospitalisation = as.Date(rep(NA, n_ind)),
    report = as.Date(rep("2025-01-10", n_ind)),
    death = as.Date(rep(NA, n_ind)),
    discharge = as.Date(rep(NA, n_ind))
  )
  
  observed_data$hospitalisation[observed_data$group == "hospitalised-alive"] <-
    as.Date("2025-01-05")
  observed_data$discharge[observed_data$group == "hospitalised-alive"] <-
    as.Date("2025-01-15")
  observed_data$death[observed_data$group == "community-dead"] <-
    as.Date("2025-01-20")
  
  estimated_dates <- array(
    as.Date(NA_character_),
    dim = c(n_ind, n_evt, n_iter, n_chains)
  )
  
  error_indicators <- array(NA, dim = c(n_ind, n_evt, n_iter, n_chains))
  
  for (i in seq_len(n_ind)) {
    allowed_events <- switch(
      observed_data$group[i],
      "community-alive" = c(1, 3),
      "hospitalised-alive" = c(1, 2, 3, 5),
      "community-dead" = c(1, 3, 4)
    )
    
    for (e in allowed_events) {
      estimated_dates[i, e, , 1] <- as.character(as.Date("2025-01-01") + e + seq_len(n_iter))
      error_indicators[i, e, , 1] <- c(FALSE, FALSE, TRUE, TRUE)
    }
  }
  
  list(
    observed = observed_data,
    mcmc = list(
      data = list(
        estimated_dates = as.character(as.vector(estimated_dates)),
        error_indicators = error_indicators
      )
    )
  )
}
