#' @title Plot Estimated Delay Distributions
#' 
#' @param mcmc_output Output list from `chronofix_mcmc_run()`
#' @param delay_map The delay_map used for the model setup
#' @param n_points Number of points along the x-axis to evaluate (default 200)
#' 
#' @import ggplot2
#' @importFrom stats median quantile dgamma dlnorm qgamma qlnorm
#' @importFrom ggtext element_markdown
#' @export
chronofix_plot_delays <- function(mcmc_output,
                                  delay_map,
                                  n_points = 200) {
  
  pars_array <- mcmc_output$pars
  n_params <- dim(pars_array)[1]
  param_names <- dimnames(pars_array)[[1]]
  
  pars_flat <- matrix(pars_array, nrow = n_params)
  rownames(pars_flat) <- param_names
  
  plot_data_list <- list()
  peak_data_list <- list() 
  
  for (i in seq_len(nrow(delay_map))) {
    
    raw_dist <- as.character(delay_map$distribution[i])
    if (grepl("gamma", raw_dist, ignore.case = TRUE)) {
      dist_clean <- "Gamma"
    } else {
      dist_clean <- "Log-Normal"
    }
    
    # clean up group names
    raw_group <- as.character(delay_map$group[[i]])
    if (grepl("^c\\(", raw_group[1])) {
      raw_group <- gsub("^c\\(|\\)$", "", raw_group[1]) 
      raw_group <- gsub("[\"']", "", raw_group)        
    }
    clean_group <- paste(raw_group, collapse = ", ")
    clean_group <- tools::toTitleCase(gsub("[-_]", " ", clean_group))
    clean_group_wrapped <- paste(strwrap(clean_group, width = 40), collapse = "<br>")
    
    # clean up delays
    from_name <- tools::toTitleCase(as.character(delay_map$from[i]))
    to_name <- tools::toTitleCase(as.character(delay_map$to[i]))
    
    panel_title <- sprintf(
      "<span style='color: #1F77B4;'>Group: %s</span><br><span style='color: #000000;'>Delay: %s to %s</span>",
      clean_group_wrapped,
      from_name,
      to_name
    )
    
    mean_samps <- pars_flat[paste0("delay_mean", i), ]
    cv_samps <- pars_flat[paste0("delay_cv", i), ]
    
    if (dist_clean == "Gamma") {
      shape <- (1 / cv_samps)^2
      scale <- mean_samps / shape
      
      med_shape <- median(shape)
      med_scale <- median(scale)
      max_x <- stats::qgamma(0.99, shape = med_shape, scale = med_scale)
      
    } else if (dist_clean == "Log-Normal") {
      sdlog <- sqrt(log(cv_samps^2 + 1))
      meanlog <- log(mean_samps) - (sdlog^2) / 2
      
      med_sdlog <- median(sdlog)
      med_meanlog <- median(meanlog)
      max_x <- stats::qlnorm(0.99, meanlog = med_meanlog, sdlog = med_sdlog)
    }
    
    x_seq <- seq(0.01, max_x, length.out = n_points)
    dens_matrix <- matrix(NA, nrow = n_points, ncol = length(mean_samps))
    
    for (k in seq_along(x_seq)) {
      if (dist_clean == "Gamma") {
        dens_matrix[k, ] <- stats::dgamma(x_seq[k], shape = shape, scale = scale)
      } else if (dist_clean == "Log-Normal") {
        dens_matrix[k, ] <- stats::dlnorm(x_seq[k], meanlog = meanlog, sdlog = sdlog)
      }
    }
    
    median_line <- apply(dens_matrix, 1, stats::quantile,
                         probs = 0.5, na.rm = TRUE)
    
    plot_data_list[[i]] <- data.frame(
      Panel_Title = panel_title,
      Distribution = dist_clean, 
      x = x_seq,
      lower = apply(dens_matrix, 1, stats::quantile, probs = 0.025, na.rm = TRUE),
      median = median_line,
      upper = apply(dens_matrix, 1, stats::quantile, probs = 0.975, na.rm = TRUE)
    )
    
    # find max density
    peak_idx <- which.max(median_line)
    peak_data_list[[i]] <- data.frame(
      Panel_Title = panel_title,
      Distribution = dist_clean,
      peak_x = x_seq[peak_idx],
      peak_y = median_line[peak_idx]
    )
  }
  
  plot_data <- do.call(rbind, plot_data_list)
  peak_data <- do.call(rbind, peak_data_list)
  
  dist_colors <- c("Gamma" = "#A7C1E1", "Log-Normal" = "#B7E4C7")
  line_colors <- c("Gamma" = "#4B7BB6", "Log-Normal" = "#52B788") 
  
  p <- ggplot(plot_data, aes(x = x, fill = Distribution, color = Distribution)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.55, color = NA) +
    geom_segment(
      data = peak_data, 
      aes(x = peak_x, xend = peak_x, y = 0, yend = peak_y, color = Distribution), 
      linetype = "dotted", linewidth = 0.8, inherit.aes = FALSE
    ) +
    geom_line(aes(y = median), linetype = "dashed", linewidth = 1) +
    facet_wrap(~ Panel_Title, scales = "free", ncol = 3) +
    scale_fill_manual(values = dist_colors) +
    scale_color_manual(values = line_colors) +
    theme_bw(base_size = 12) +
    labs(
      x = "Delay (Days)", 
      y = "Probability Density",
      title = "Posterior Estimated Delay Distributions",
      subtitle = "Dashed curve: Posterior Median. Dotted line: Peak Density. Shaded area: 95% CrI."
    ) +
    theme(
      strip.text = element_markdown(face = "bold", size = 9, lineheight = 1.2,
                                    margin = margin(b = 6, t = 6)),
      strip.background = element_rect(fill = "#f8f9fa", color = "#cccccc"),
      panel.grid.minor = element_blank(),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )
  
  return(p)
}
