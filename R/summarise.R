##' Summarise an individuals date
##'
##' @title Summarise an individual date
##'
##' @param samples A `chronofix_mcmc_samples` object as generated
##'   by `chronofix_mcmc()`
##'   
##' @param id The id of the individual to be summarised
##' 
##' @param date The name of the date to be summarised
##'
##' @return A data.frame with columns `Estimated date` and `Posterior weight`
##'
##' @export
chronofix_summarise_individual <- function(samples, id, date) {
  
  if (!inherits(samples, "chronofix_mcmc_samples")) {
    cli::cli_abort("Expected 'samples' to be a 'chronofix_mcmc_samples' object")
  }
  
  id_col <- attr(samples$data, "id")
  group_col <- attr(samples$data, "group")
  
  which_id <-  which(data[[id_col]] == id)
  if (length(which_id) == 0) {
    cli::cli_abort("No individual with {.col {id_col}} value of {.val {id}}")
  }
  
  if (!(group_col %in% setdiff(names(samples$data), c(id_col, group_col)))) {
    cli::cli_abort('No date column in data with name "{.val {date}}"')
  }
  
  cli::cli_alert_info(paste0(id_col, ": ", id))
  
  cli::cli_alert_info(paste("date:", date))
  
  
  if (!is.null(group_col)) {
    group <- samples$data[[group_col]][which_id]
    cli::cli_alert_info(paste0(group_col, ": ", group))
  }
  
  observed_date <- samples$data[[date]][which_id]
  missing <- is.na(observed_date)
  
  observed_date <- if (is.na(observed_date)) "missing" else observed_date
  cli::cli_alert_info(paste("observed date:", observed_date))
  
  error_indicators <- 
    samples$augmented_data$error_indicators[as.character(id), date, ]
  prob_error <- sum(error_indicators) / length(error_indicators)
  cli::cli_alert_info(paste("posterior error probability:", prob_error))
  
  estimated_dates <- 
    floor(samples$augmented_data$estimated_dates[as.character(id), date, ])
  tab <- table(as.Date(estimated_dates))
  tab <- as.data.frame(tab / sum(tab))
  names(tab) <- c("Estimated date", "Posterior weight")
  
  tab
}
