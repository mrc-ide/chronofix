##' Summarise the estimated dates for an individual
##'
##' @title Summarise an individual
##'
##' @param samples A `chronofix_mcmc_samples` object as generated
##'   by `chronofix_mcmc()`
##'   
##' @param id The id of the individual to be summarised
##' 
##' @return A list
##'
##' @export
chronofix_summarise_individual <- function(samples, id) {
  
  if (!inherits(samples, "chronofix_mcmc_samples")) {
    cli::cli_abort("Expected 'samples' to be a 'chronofix_mcmc_samples' object")
  }
  
  id_col <- attr(samples$data, "id")
  group_col <- attr(samples$data, "group")
  
  which_id <-  which(data[[id_col]] == id)
  if (length(which_id) == 0) {
    cli::cli_abort("No individual with {.col {id_col}} value of {.val {id}}")
  }
  
  cli::cli_alert_info(paste0(id_col, ": ", id))
  
  if (!is.null(group_col)) {
    group <- samples$data[[group_col]][which_id]
    cli::cli_alert_info(paste0(group_col, ": ", group))
  }
  
  error_indicators <- 
    samples$augmented_data$error_indicators[as.character(id), , ]
  relevant_dates <- rownames(error_indicators)[!is.na(error_indicators[, 1])]
  
  f <- function(date) {
    prob_error <- sum(error_indicators[date, ]) / ncol(error_indicators)
    
    observed_date <- samples$data[[date]][which_id]
    
    estimated_dates <- 
      floor(samples$augmented_data$estimated_dates[as.character(id), date, ])
    tab <- table(as.Date(estimated_dates))
    tab <- as.data.frame(tab / sum(tab))
    names(tab) <- c("Estimated date", "Posterior weight")
    
    list(prob_error = prob_error,
         observed = observed_date,
         estimated = tab)
  }
  
  x <- lapply(relevant_dates, f)
  names(x) <- relevant_dates
  
  x
}
