make_delay_summary_mock <- function() {
  delay_map <- data.frame(
    group = I(list("community-alive",
                   c("hospitalised-alive", "hospitalised-dead"))),
    from = c("onset", "hospitalisation"),
    to = c("report", "discharge"),
    distribution = c("gamma", "lognormal"),
    stringsAsFactors = FALSE
  )
  
  pars <- array(NA, dim = c(4, 5, 1), dimnames = list(
    c("delay_mean1", "delay_cv1", "delay_mean2", "delay_cv2"), NULL, NULL))
  
  pars["delay_mean1", , 1] <- c(1, 2, 3, 4, 5)
  pars["delay_cv1", , 1] <- c(0.1, 0.2, 0.3, 0.4, 0.5)
  pars["delay_mean2", , 1] <- c(10, 20, 30, 40, 50)
  pars["delay_cv2", , 1] <- c(1, 2, 3, 4, 5)
  
  list(
    mcmc_output = list(pars = pars),
    delay_map = delay_map
  )
}