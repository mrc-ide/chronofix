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
  format <- attr(samples$data, "format")
  
  which_id <-  which(samples$data[[id_col]] == id)
  if (length(which_id) == 0) {
    cli::cli_abort("No individual with {.col {id_col}} value of {.val {id}}")
  }
  
  
  if (!is.null(group_col)) {
    group <- samples$data[[group_col]][which_id]
    plot_subtitle <- sprintf("%s: %s\n%s: %s", id_col, id, group_col, group)
  } else {
    plot_subtitle  <- sprintf("%s: %s\n%s: %s", id_col, id)
  }
  
  error_indicators <- 
    samples$augmented_data$error_indicators[as.character(id), , ]
  relevant_dates <- rownames(error_indicators)[!is.na(error_indicators[, 1])]
  
  estimated_dates <- 
    floor(samples$augmented_data$estimated_dates[which_id, relevant_dates, ])
  df <- as.data.frame(t(estimated_dates)) %>%
    pivot_longer(everything(), names_to = "event", values_to = "date")
  df$date <- as.Date(df$date)
  
  prob_error <- 
    rowSums(error_indicators[relevant_dates, ]) / ncol(error_indicators)
  observed_dates <- 
    format(as.Date(unlist(samples$data[which_id, relevant_dates])), 
           format = format)
  labels <- 
    sprintf(c("<b>%s</b><br>observed date: %s<br> posterior error weight: %s"),
            relevant_dates, observed_dates, prob_error)
  df$event <- factor(df$event, levels = relevant_dates,
                     labels = labels)
  
  ggplot(df, aes(x = date, y = ..density..)) + 
    stat_bin(binwidth = 1, fill = "#4B7BB6") +
    scale_x_date(date_labels = format) +
    facet_wrap(vars(event), scales = "free") +
    theme_bw() +
    theme(strip.text = element_markdown(size = 10, lineheight = 1.2,
                                        margin = margin(b = 6, t = 6)),
          strip.background = element_rect(fill = "#f8f9fa", colour = "#cccccc")) +
    labs(
      x = "Date", 
      y = "Posterior weight",
      title = "Posterior Estimated Date Distributions",
      subtitle = plot_subtitle
    )
}
