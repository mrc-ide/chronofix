##' Plot the posterior distribution of estimated dates for each event for a 
##'  single individual.
##'
##' @title Plot estimated date distributions for an individual
##'
##' @param samples A `chronofix_mcmc_samples` object as generated
##'   by `chronofix_mcmc()`
##' @param id The id of the individual to be summarised
##' @param min_window Minimum number of days shown either side of the
##'   observed date in each panel. Panels are wider if the plotted dates
##'   extend further (default 7)
##' @param min_prob Dates with posterior probability below this are not
##'   shown (default 0.01)
##'   
##' @return A ggplot object showing the distributions of estimated dates for an
##'  individual
##'
##' @import ggplot2
##' @importFrom ggtext element_markdown
##' 
##' @export
chronofix_plot_individual <- function(samples, id, min_window = 7, min_prob = 0.01) {
  if (!inherits(samples, "chronofix_mcmc_samples")) {
    cli::cli_abort("Expected 'samples' to be a 'chronofix_mcmc_samples' object")
  }
  
  id_col <- attr(samples$data, "id")
  group_col <- attr(samples$data, "group")
  date_format <- attr(samples$data, "format")
  
  which_id <- which(samples$data[[id_col]] == id)
  if (length(which_id) == 0) {
    cli::cli_abort("No individual with {.col {id_col}} value of {.val {id}}")
  }
  
  subtitle <- sprintf("%s: %s", id_col, id)
  if (!is.null(group_col)) {
    subtitle <- sprintf("%s\n%s: %s", subtitle, group_col,
                        samples$data[[group_col]][which_id])
  }
  
  est <- samples$augmented_data$estimated_dates[which_id, , , drop = FALSE]
  err <- samples$augmented_data$error_indicators[which_id, , , drop = FALSE]
  est <- matrix(est, nrow = dim(est)[2], dimnames = dimnames(est)[2:3])
  err <- matrix(err, nrow = dim(err)[2], dimnames = dimnames(err)[2:3])
  
  events <- rownames(est)[!is.na(est[, 1])]
  est <- floor(est[events, , drop = FALSE])
  p_error <- rowMeans(err[events, , drop = FALSE])
  observed <- as.Date(unlist(samples$data[which_id, events]))
  n_iter <- ncol(est)
  
  # Posterior probability mass per day, per event
  df <- do.call(rbind, lapply(events, function(e) {
    tab <- table(est[e, ])
    data.frame(event = e,
               date = as.Date(as.numeric(names(tab))),
               prob = as.numeric(tab) / n_iter)
  }))
  
  obs_prob <- vapply(seq_along(events), function(i) {
    if (is.na(observed[i])) return(NA_real_)
    p <- df$prob[df$event == events[i] & df$date == observed[i]]
    if (length(p) == 0) 0 else p
  }, numeric(1))
  names(obs_prob) <- events
  
  row_obs <- observed[match(df$event, events)]
  row_prob <- obs_prob[df$event]
  df$category <- ifelse(
    is.na(row_obs), "No observed date",
    ifelse(df$date == row_obs, "Observed date",
           ifelse(df$prob > row_prob, "More probable than observed",
                  "Less probable than observed"))
  )
  
  bar_colours <- c("More probable than observed" = "#2A4A7F",
                   "Observed date" = "#4B7BB6",
                   "Less probable than observed" = "#A7C1E1",
                   "No observed date" = "grey60")
  df$category <- factor(df$category, levels = names(bar_colours))
  
  hidden <- df$prob < min_prob
  df <- df[!hidden, ]
  caption <- if (any(hidden)) {
    sprintf("Dates with posterior probability < %s not shown.", min_prob)
  }
  
  legend_keys <- names(bar_colours)[names(bar_colours) != "No observed date" |
                                      any(is.na(observed))]
  
  has_obs <- !is.na(observed)
  obs_df <- data.frame(event = events[has_obs], observed = observed[has_obs])
  range_df <- data.frame(event = rep(events[has_obs], 2),
                         date = c(observed[has_obs] - min_window,
                                  observed[has_obs] + min_window))
  
  lvl <- function(x) factor(x, levels = events)
  df$event <- lvl(df$event)
  obs_df$event <- lvl(obs_df$event)
  range_df$event <- lvl(range_df$event)
  
  strip_labels <- stats::setNames(
    sprintf("Event: <span style='font-weight:normal'>%s<br>Observed: %s<br>P(error): %s</span>",
            events,
            ifelse(has_obs, format(observed, date_format), "missing"),
            sprintf("%.3f", p_error)),
    events
  )
  
  ggplot(df, aes(x = date, y = prob)) +
    geom_blank(data = range_df, aes(x = date), inherit.aes = FALSE) +
    geom_col(aes(fill = category), width = 0.9, show.legend = TRUE) +
    geom_vline(data = obs_df, aes(xintercept = observed),
               linetype = "dashed", linewidth = 0.6, colour = "#4B7BB6") +
    scale_fill_manual(name = NULL, values = bar_colours,
                      limits = legend_keys, drop = FALSE) +
    scale_x_date(date_labels = date_format,
                 breaks = scales::breaks_pretty(4)) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = seq(0, 1, by = 0.25),
                       expand = expansion(mult = c(0, 0.05))) +
    facet_wrap(vars(event), scales = "free_x",
               labeller = as_labeller(strip_labels)) +
    labs(x = "Date", y = "Posterior Probability",
         title = "Posterior Estimated Date Distributions",
         subtitle = subtitle,
         caption = caption) +
    theme_bw(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      strip.text = element_markdown(face = "bold", size = 9, lineheight = 1.2,
                                    margin = margin(b = 6, t = 6)),
      strip.background = element_rect(fill = "#f8f9fa", colour = "#cccccc"),
      panel.grid.minor = element_blank(),
      axis.title = element_text(face = "bold"),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      legend.title = element_text(face = "bold", size = 11),
      legend.position = "bottom"
    )
}
