##' Plot the distributions of estimated dates for an individual
##'
##' @title Plot estimated date distributions for an individual
##'
##' @param samples A `chronofix_mcmc_samples` object as generated
##'   by `chronofix_mcmc()`
##'   
##' @param id The id of the individual to be summarised
##' 
##' @return A figure
##'
##' @export
chronofix_plot_individual <- function(samples, id) {
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
  
  estimated_dates <- 
    floor(samples$augmented_data$estimated_dates[which_id, relevant_dates, ])
  df <- as.data.frame(t(estimated_dates)) %>%
    pivot_longer(everything(), names_to = "event", values_to = "date")
  
  ggplot(df, aes(x = date, y = ..density..)) + 
    stat_bin(binwidth=1) + 
    scale_x_date(date_breaks ="1 day") +
    facet_wrap(vars(event), scales = "free_x") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
