test_that("chronofix_plot_delays calls validation helper", {
  expect_error(chronofix_plot_delays(), "is missing")
})

test_that("chronofix_plot_delays generates a correct ggplot object", {
  
  mock_delay_map <- data.frame(
    from = c("onset", "hospitalisation"),
    to = c("hospitalisation", "death"),
    distribution = c("gamma", "log-normal"),
    stringsAsFactors = FALSE
  )
  mock_delay_map$group <- list("community-alive", "hospitalised-dead")
  
  # minimal mock mcmc_output array [parameters, iterations, chains]
  param_names <- c("delay_mean1", "delay_shape1", "delay_meanlog2", "delay_precisionlog2")
  
  set.seed(1)
  mock_pars <- array(
    data = NA, 
    dim = c(4, 50, 2),
    dimnames = list(param_names, NULL, NULL)
  )
  
  mock_pars["delay_mean1", , ] <- runif(100, min = 3, max = 8)
  mock_pars["delay_shape1", , ] <- runif(100, min = 2, max = 5)
  mock_pars["delay_meanlog2", , ] <- runif(100, min = 1, max = 2)
  mock_pars["delay_precisionlog2", , ] <- runif(100, min = 2, max = 5)
  
  mock_mcmc_output <- list(pars = mock_pars)
  
  p <- chronofix_plot_delays(
    mcmc_output = mock_mcmc_output, 
    delay_map = mock_delay_map, 
    n_points = 20
  )
  
  expect_s3_class(p, "ggplot")
  expect_length(p$layers, 3) # 3 geom layers (ribbon, segment, line)
  expect_identical(p$labels$x, "Delay (Days)")
  expect_identical(p$labels$y, "Probability Density")
  expect_s3_class(p$facet, "FacetWrap")
  expect_s3_class(p$theme$strip.text, "element_markdown")
})

test_that("chronofix_plot_delays handles edge cases in group names gracefully", {
  
  mock_delay_map <- data.frame(
    from = "onset",
    to = "report",
    distribution = "gamma",
    stringsAsFactors = FALSE
  )
  mock_delay_map$group <- list("c(\"complex_group_name\")")
  
  param_names <- c("delay_mean1", "delay_shape1")
  mock_pars <- array(
    data = runif(100, min = 0.5, max = 5), 
    dim = c(2, 50, 1),
    dimnames = list(param_names, NULL, NULL)
  )
  mock_mcmc_output <- list(pars = mock_pars)
  
  p <- chronofix_plot_delays(mock_mcmc_output, mock_delay_map, n_points = 10)
  
  # Extract underlying plot data to check if the group name cleaned up correctly
  plot_data <- ggplot2::ggplot_build(p)$data[[1]]
  
  # "c("complex_group_name")" should become "Complex Group Name"
  panel_titles <- unique(ggplot2::ggplot_build(p)$layout$layout$Panel_Title)
  expect_true(any(grepl("Complex Group Name", panel_titles)))
})

test_that("chronofix_plot_delays handles multiple groups in a single facet", {
  
  mock_delay_map <- data.frame(
    from = "onset",
    to = "report",
    distribution = "gamma",
    stringsAsFactors = FALSE
  )
  # Pass multiple groups into a single row
  mock_delay_map$group <- I(list(c("community_alive", "hospitalised_alive")))
  
  param_names <- c("delay_mean1", "delay_shape1")
  set.seed(1)
  mock_pars <- array(
    data = runif(100, min = 0.5, max = 5), 
    dim = c(2, 50, 1),
    dimnames = list(param_names, NULL, NULL)
  )
  mock_mcmc_output <- list(pars = mock_pars)
  
  p <- chronofix_plot_delays(mock_mcmc_output, mock_delay_map, n_points = 10)
  
  # Check if the title correctly pasted and capitalised both groups
  panel_titles <- unique(ggplot2::ggplot_build(p)$layout$layout$Panel_Title)
  expect_true(any(grepl("Community Alive, Hospitalised Alive", panel_titles)))
})

