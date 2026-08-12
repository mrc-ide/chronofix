#' @title Get Summary Table of Estimated Delays
#'
#' @param mcmc_output Output list from `chronofix_mcmc_run()`.
#' @param delay_map The delay map used for the model setup.
#'
#' @return A data frame containing posterior summaries for the estimated delay
#'   mean and coefficient of variation (CV), including posterior medians and
#'   95% credible intervals.
#'
#' @importFrom stats quantile
#' @export
chronofix_get_delays <- function(mcmc_output, delay_map) {
  
  validate_delay_inputs(mcmc_output, delay_map)
  
  pars_array <- mcmc_output$pars
  n_params <- dim(pars_array)[1]
  param_names <- dimnames(pars_array)[[1]]
  
  pars_flat <- matrix(pars_array, nrow = n_params)
  rownames(pars_flat) <- param_names
  
  summary_list <- vector("list", nrow(delay_map))
  
  for (i in seq_len(nrow(delay_map))) {
    
    raw_dist <- as.character(delay_map$distribution[i])
    if (grepl("gamma", raw_dist, ignore.case = TRUE)) {
      dist_clean <- "Gamma"
    } else {
      dist_clean <- "Log-Normal"
    }
    
    clean_group <- clean_group_name(delay_map$group[[i]])
    from_name <- clean_event_name(delay_map$from[i])
    to_name <- clean_event_name(delay_map$to[i])
    
    delay_label <- paste(from_name, "to", to_name)
    
    if (dist_clean == "Gamma") {
      mean_samps <- pars_flat[paste0("delay_mean", i), , drop = TRUE]
      shape_samps <- pars_flat[paste0("delay_shape", i), , drop = TRUE]
      cv_samps <- 1 / sqrt(shape_samps)
      
    } else if (dist_clean == "Log-Normal") {
      meanlog_samps <- pars_flat[paste0("delay_meanlog", i), , drop = TRUE]
      prec_samps <- pars_flat[paste0("delay_precisionlog", i), , drop = TRUE]
      mean_samps <- exp(meanlog_samps + (1 / prec_samps) / 2)
      cv_samps <- sqrt(exp(1 / prec_samps) - 1)
    }
    
    mean_quantiles <- stats::quantile(
      mean_samps,
      probs = c(0.025, 0.5, 0.975),
      na.rm = TRUE,
      names = FALSE
    )
    
    cv_quantiles <- stats::quantile(
      cv_samps,
      probs = c(0.025, 0.5, 0.975),
      na.rm = TRUE,
      names = FALSE
    )
    
    summary_list[[i]] <- data.frame(
      Group = clean_group,
      Delay = delay_label,
      Distribution = dist_clean,
      Delay_Median = round(mean_quantiles[2], 3),
      Delay_Lower_95_CrI = round(mean_quantiles[1], 3),
      Delay_Upper_95_CrI = round(mean_quantiles[3], 3),
      CV_Median = round(cv_quantiles[2], 3),
      CV_Lower_95_CrI = round(cv_quantiles[1], 3),
      CV_Upper_95_CrI = round(cv_quantiles[3], 3),
      stringsAsFactors = FALSE
    )
  }
  
  results_df <- do.call(rbind, summary_list)
  rownames(results_df) <- NULL
  
  results_df
}
