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
    
    mean_name <- paste0("delay_mean", i)
    cv_name <- paste0("delay_cv", i)
    
    raw_dist <- as.character(delay_map$distribution[i])
    if (grepl("gamma", raw_dist, ignore.case = TRUE)) {
      dist_clean <- "Gamma"
    } else {
      dist_clean <- "Log-Normal"
    }
    
    raw_group <- as.character(delay_map$group[[i]])
    if (length(raw_group) == 1 && grepl("^c\\(", raw_group)) {
      raw_group <- gsub("^c\\(|\\)$", "", raw_group)
      raw_group <- gsub("[\"']", "", raw_group)
      raw_group <- trimws(strsplit(raw_group, ",")[[1]])
    }
    
    clean_group <- paste(raw_group, collapse = ", ")
    clean_group <- gsub("[-_]", " ", clean_group)
    clean_group <- tools::toTitleCase(clean_group)
    
    from_name <- gsub("[-_]", " ", as.character(delay_map$from[i]))
    to_name <- gsub("[-_]", " ", as.character(delay_map$to[i]))
    
    from_name <- tools::toTitleCase(from_name)
    to_name <- tools::toTitleCase(to_name)
    
    delay_label <- paste(from_name, "to", to_name)
    
    mean_samps <- pars_flat[mean_name, , drop = TRUE]
    cv_samps <- pars_flat[cv_name, , drop = TRUE]
    
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
