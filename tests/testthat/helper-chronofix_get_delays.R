make_delay_summary_mock <- function() {
  delay_map <- data.frame(
    group = I(list("community-alive",
                   c("hospitalised-alive", "hospitalised-dead"))),
    from = c("onset", "hospitalisation"),
    to = c("report", "discharge"),
    distribution = c("gamma", "lognormal"),
    stringsAsFactors = FALSE
  )
  
  pars <- array(NA, dim = c(4, 5, 1),
                dimnames = list(c("delay_mean1", "delay_shape1",
                                  "delay_meanlog2", "delay_precisionlog2"),
                                NULL, NULL))
  
  pars["delay_mean1", , 1] <- c(1, 2, 3, 4, 5)
  pars["delay_shape1", , 1] <- c(2, 4, 6, 8, 10)
  pars["delay_meanlog2", , 1] <- c(1, 2, 3, 4, 5)
  pars["delay_precisionlog2", , 1] <- c(1, 2, 3, 4, 5)
  
  list(
    mcmc_output = list(pars = pars),
    delay_map = delay_map
  )
}
