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
  
  validate_delay_inputs(mcmc_output, delay_map)
  
  pars_flat <- mcmc_output$pars
  
  plot_data_list <- list()
  peak_data_list <- list()
  
  for (i in seq_len(nrow(delay_map))) {
    
    raw_dist <- as.character(delay_map$distribution[i])
    is_gamma <- grepl("gamma", raw_dist, ignore.case = TRUE)
    dist_clean <- if (is_gamma) "Gamma" else "Log-Normal"
    
    clean_group <- clean_group_name(delay_map$group[[i]])
    clean_group_wrapped <- paste(strwrap(clean_group, width = 40), collapse = "<br>")
    
    panel_title <- sprintf(
      "<span style='color: #1F77B4;'>Group: %s</span><br><span style='color: #000000;'>Delay: %s to %s</span>",
      clean_group_wrapped,
      clean_event_name(delay_map$from[i]),
      clean_event_name(delay_map$to[i])
    )
    
    if (is_gamma) {
      mean_samps <- pars_flat[paste0("delay", i, "_mean"), ]
      shape_samps <- pars_flat[paste0("delay", i, "_shape"), ]
      scale_samps <- mean_samps / shape_samps
      
      max_x <- stats::qgamma(0.99,
                             shape = mean(shape_samps, na.rm = TRUE),
                             scale = mean(scale_samps, na.rm = TRUE))
      x_seq <- seq(0.01, max_x, length.out = n_points)
      
      dens_matrix <- t(sapply(x_seq, function(x) {
        stats::dgamma(x, shape = shape_samps, scale = scale_samps)
      }))
      
    } else {
      meanlog_samps <- pars_flat[paste0("delay", i, "_meanlog"), ]
      prec_samps <- pars_flat[paste0("delay", i, "_precisionlog"), ]
      sdlog_samps <- sqrt(1 / prec_samps)
      
      max_x <- stats::qlnorm(0.99,
                             meanlog = mean(meanlog_samps, na.rm = TRUE),
                             sdlog = mean(sdlog_samps, na.rm = TRUE))
      x_seq <- seq(0.01, max_x, length.out = n_points)
      
      dens_matrix <- t(sapply(x_seq, function(x) {
        stats::dlnorm(x, meanlog = meanlog_samps, sdlog = sdlog_samps)
      }))
    }
    
    mean_line <- rowMeans(dens_matrix, na.rm = TRUE)
    
    quants <- apply(dens_matrix, 1, stats::quantile, probs = c(0.025, 0.975), na.rm = TRUE)
    
    plot_data_list[[i]] <- data.frame(
      Panel_Title = panel_title,
      Distribution = dist_clean, 
      x = x_seq,
      lower = quants[1, ], # 2.5%
      mean_density = mean_line,
      upper = quants[2, ] # 97.5%
    )
    
    # find max density
    peak_idx <- which.max(mean_line)
    peak_data_list[[i]] <- data.frame(
      Panel_Title = panel_title,
      Distribution = dist_clean,
      peak_x = x_seq[peak_idx],
      peak_y = mean_line[peak_idx]
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
    geom_line(aes(y = mean_density), linetype = "dashed", linewidth = 1) +
    facet_wrap(~ Panel_Title, scales = "free", ncol = 3) +
    scale_fill_manual(values = dist_colors) +
    scale_color_manual(values = line_colors) +
    theme_bw(base_size = 12) +
    labs(
      x = "Delay (Days)", 
      y = "Probability Density",
      title = "Posterior Estimated Delay Distributions",
      subtitle = "Dashed curve: Posterior Mean. Dotted line: Peak Density. Shaded area: 95% CrI."
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
