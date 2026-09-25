test_that("chronofix_plot_delays calls validation helper", {
  expect_error(chronofix_plot_delays(), "`mcmc_output` is missing")
  expect_error(chronofix_plot_delays(mcmc_output = list(pars = matrix(1))),
               "`delay_map` is missing")
})

test_that("chronofix_plot_delays generates a correct standard ggplot (facet_by_group = FALSE)", {
  
  mock_delay_map <- data.frame(
    from = c("onset", "onset", "hospitalisation", "onset", "hospitalisation", "onset"),
    to = c("report", "hospitalisation", "discharge", "hospitalisation", "death", "death"),
    distribution = c("gamma", "log-normal", "gamma", "gamma", "log-normal", "gamma"),
    stringsAsFactors = FALSE
  )
  mock_delay_map$group <- as.list(c(
    "hospitalised-alive", "hospitalised-alive", "hospitalised-alive",
    "hospitalised-dead", "hospitalised-dead",
    "community-dead"
  ))
  
  param_names <- c(
    "delay1_mean", "delay1_shape",
    "delay2_meanlog", "delay2_precisionlog",
    "delay3_mean", "delay3_shape",
    "delay4_mean", "delay4_shape",
    "delay5_meanlog", "delay5_precisionlog",
    "delay6_mean", "delay6_shape"
  )
  
  set.seed(1)
  mock_pars <- matrix(
    data = NA, 
    nrow = length(param_names),
    ncol = 100, 
    dimnames = list(param_names, NULL)
  )
  
  mock_pars["delay1_mean", ] <- runif(100, min = 4, max = 6)
  mock_pars["delay1_shape", ] <- runif(100, min = 2, max = 4)
  mock_pars["delay2_meanlog", ] <- runif(100, min = 1, max = 2)
  mock_pars["delay2_precisionlog", ] <- runif(100, min = 2, max = 5)
  mock_pars["delay3_mean", ] <- runif(100, min = 5, max = 10)
  mock_pars["delay3_shape", ] <- runif(100, min = 1.5, max = 3)
  mock_pars["delay4_mean", ] <- runif(100, min = 3, max = 7)
  mock_pars["delay4_shape", ] <- runif(100, min = 2, max = 5)
  mock_pars["delay5_meanlog", ] <- runif(100, min = 1.5, max = 2.5)
  mock_pars["delay5_precisionlog", ] <- runif(100, min = 2, max = 4)
  mock_pars["delay6_mean", ] <- runif(100, min = 10, max = 15)
  mock_pars["delay6_shape", ] <- runif(100, min = 3, max = 6)
  
  mock_mcmc_output <- list(pars = mock_pars)
  
  p <- chronofix_plot_delays(
    mcmc_output = mock_mcmc_output, 
    delay_map = mock_delay_map, 
    n_points = 30,
    facet_by_group = FALSE,
    share_x_axis = FALSE
  )
  
  expect_s3_class(p, "ggplot")
  expect_length(p$layers, 2) # 2 geom layers (ribbon, line)
  expect_identical(p$labels$x, "Delay (Days)")
  expect_identical(p$labels$y, "Probability Density")
  expect_s3_class(p$facet, "FacetWrap")
})

test_that("chronofix_plot_delays generates a correct patchwork object (facet_by_group = TRUE)", {
  
  mock_delay_map <- data.frame(
    from = c("onset", "onset", "hospitalisation", "onset", "hospitalisation", "onset"),
    to = c("report", "hospitalisation", "discharge", "hospitalisation", "death", "death"),
    distribution = c("gamma", "log-normal", "gamma", "gamma", "log-normal", "gamma"),
    stringsAsFactors = FALSE
  )
  mock_delay_map$group <- as.list(c(
    "hospitalised-alive", "hospitalised-alive", "hospitalised-alive",
    "hospitalised-dead", "hospitalised-dead",
    "community-dead"
  ))
  
  param_names <- c(
    "delay1_mean", "delay1_shape",
    "delay2_meanlog", "delay2_precisionlog",
    "delay3_mean", "delay3_shape",
    "delay4_mean", "delay4_shape",
    "delay5_meanlog", "delay5_precisionlog",
    "delay6_mean", "delay6_shape"
  )
  
  set.seed(1)
  mock_pars <- matrix(
    data = NA, 
    nrow = length(param_names),
    ncol = 100, 
    dimnames = list(param_names, NULL)
  )
  
  mock_pars["delay1_mean", ] <- runif(100, min = 4, max = 6)
  mock_pars["delay1_shape", ] <- runif(100, min = 2, max = 4)
  mock_pars["delay2_meanlog", ] <- runif(100, min = 1, max = 2)
  mock_pars["delay2_precisionlog", ] <- runif(100, min = 2, max = 5)
  mock_pars["delay3_mean", ] <- runif(100, min = 5, max = 10)
  mock_pars["delay3_shape", ] <- runif(100, min = 1.5, max = 3)
  mock_pars["delay4_mean", ] <- runif(100, min = 3, max = 7)
  mock_pars["delay4_shape", ] <- runif(100, min = 2, max = 5)
  mock_pars["delay5_meanlog", ] <- runif(100, min = 1.5, max = 2.5)
  mock_pars["delay5_precisionlog", ] <- runif(100, min = 2, max = 4)
  mock_pars["delay6_mean", ] <- runif(100, min = 10, max = 15)
  mock_pars["delay6_shape", ] <- runif(100, min = 3, max = 6)
  mock_mcmc_output <- list(pars = mock_pars)
  
  p <- chronofix_plot_delays(
    mcmc_output = mock_mcmc_output, 
    delay_map = mock_delay_map, 
    n_points = 30,
    facet_by_group = TRUE,
    share_x_axis = TRUE
  )
  
  expect_s3_class(p, "patchwork")
  expect_s3_class(p, "ggplot")
  
  # ragged rows are padded with spacers
  # row 1: 3 delays, 0 spacers
  # row 2: 2 delays, 1 spacer
  # row 3: 1 delay, 2 spacers - 3 spacers total
  all_cells <- c(p$patches$plots, list(p))
  n_spacers <- sum(vapply(all_cells,
                          function(x) inherits(x, "spacer"), logical(1)))
  expect_equal(n_spacers, 3)
  
  # 6 delays + 3 spacers = 9 cells
  n_cells <- length(p$patches$plots) + 1
  expect_equal(n_cells, 9)
})

test_that("chronofix_plot_delays handles edge cases in group names gracefully", {
  
  mock_delay_map <- data.frame(
    from = "onset",
    to = "report",
    distribution = "gamma",
    stringsAsFactors = FALSE
  )
  mock_delay_map$group <- list("c(\"complex_group_name\")")
  
  param_names <- c("delay1_mean", "delay1_shape")
  mock_pars <- matrix(
    data = runif(100, min = 0.5, max = 5),
    nrow = 2,
    ncol = 50, # 50 iter * 1 chain
    dimnames = list(param_names, NULL)
  )
  mock_mcmc_output <- list(pars = mock_pars)
  
  p <- chronofix_plot_delays(mock_mcmc_output, mock_delay_map, n_points = 10,
                             facet_by_group = FALSE)
  
  # "c("complex_group_name")" should become "Complex Group Name"
  panel_titles <- unique(p$data$Panel_Title)
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
  
  param_names <- c("delay1_mean", "delay1_shape")
  set.seed(1)
  mock_pars <- matrix(
    data = runif(100, min = 0.5, max = 5), 
    nrow = 2,
    ncol = 50,
    dimnames = list(param_names, NULL)
  )
  mock_mcmc_output <- list(pars = mock_pars)
  
  p <- chronofix_plot_delays(mock_mcmc_output, mock_delay_map, n_points = 20,
                             facet_by_group = FALSE)
  
  # Check if the title correctly pasted and capitalised both groups
  panel_titles <- unique(p$data$Panel_Title)
  expect_true(any(grepl("Community Alive, Hospitalised Alive", panel_titles)))
})


test_that("chronofix_plot_delays correctly filters by select_group and select_delay", {
  
  mock_delay_map <- data.frame(
    from = c("onset", "onset", "hospitalisation", "onset"),
    to = c("report", "hospitalisation", "discharge", "death"),
    distribution = c("gamma", "log-normal", "gamma", "gamma"),
    stringsAsFactors = FALSE
  )
  
  mock_delay_map$group <- as.list(c(
    "shared",
    "hospitalised-alive", 
    "hospitalised-alive",
    "community-alive"
  ))
  # shared between two groups
  mock_delay_map$group[[1]] <- c("community-alive", "community-dead")
  
  param_names <- c(
    "delay1_mean", "delay1_shape",
    "delay2_meanlog", "delay2_precisionlog",
    "delay3_mean", "delay3_shape",
    "delay4_mean", "delay4_shape"
  )
  
  set.seed(1)
  mock_pars <- matrix(
    data = runif(800, min = 1, max = 5), 
    nrow = 8, ncol = 100, 
    dimnames = list(param_names, NULL)
  )
  mock_mcmc_output <- list(pars = mock_pars)
  
  # Test filtering by select_group
  # (this is also a group that shares a delay with another group)
  p_group <- chronofix_plot_delays(
    mcmc_output = mock_mcmc_output, 
    delay_map = mock_delay_map, 
    n_points = 10, 
    facet_by_group = FALSE,
    select_group = "community-dead"
  )
  
  expect_true(all(grepl("Community Dead", unique(p_group$data$Group_Title))))
  expect_false(any(grepl("Hospitalised Alive", unique(p_group$data$Group_Title))))
  
  # Test filtering by select_delay
  p_delay <- chronofix_plot_delays(
    mcmc_output = mock_mcmc_output, 
    delay_map = mock_delay_map, 
    n_points = 10, 
    facet_by_group = FALSE,
    select_delay = "hospitalisation to discharge"
  )
  
  expect_true(all(as.character(p_delay$data$Delay_Title) == "Hospitalisation to Discharge"))
  
  # Test filtering by multiple delays simultaneously
  p_multi_delay <- chronofix_plot_delays(
    mcmc_output = mock_mcmc_output, 
    delay_map = mock_delay_map, 
    n_points = 10, 
    facet_by_group = FALSE,
    select_delay = c("onset to report", "onset to hospitalisation")
  )
  
  plotted_delays <- as.character(unique(p_multi_delay$data$Delay_Title))
  
  expect_length(plotted_delays, 2)
  expect_true("Onset to Report" %in% plotted_delays)
  expect_true("Onset to Hospitalisation" %in% plotted_delays)
  expect_false("Hospitalisation to Discharge" %in% plotted_delays)
  
  # Test filtering by multiple groups simultaneously
  p_multi_group <- chronofix_plot_delays(
    mcmc_output = mock_mcmc_output, 
    delay_map = mock_delay_map, 
    n_points = 10, 
    facet_by_group = FALSE,
    select_group = c("community-alive", "hospitalised-alive")
  )
  
  plotted_groups <- as.character(unique(p_multi_group$data$Group_Title))
  
  expect_length(plotted_groups, 3)
  expect_true("Group: Community Alive" %in% plotted_groups)
  expect_true("Group: Community Alive, Community Dead" %in% plotted_groups)
  expect_true("Group: Hospitalised Alive" %in% plotted_groups)
  
  # Test empty filtering triggers error
  expect_error(
    chronofix_plot_delays(
      mcmc_output = mock_mcmc_output, 
      delay_map = mock_delay_map, 
      select_group = "nonexistent-group"
    ),
    "Filtering resulted in 0 distributions to plot"
  )
})

test_that("filtering preserves the mapping to MCMC parameters", {
  
  mock_delay_map <- data.frame(
    from = c("onset", "onset", "hospitalisation", "onset",
             "hospitalisation", "onset"),
    to = c("report", "hospitalisation", "discharge", "hospitalisation",
           "death", "death"),
    distribution = c("gamma", "log-normal", "gamma", "gamma",
                     "log-normal", "gamma"),
    stringsAsFactors = FALSE
  )
  mock_delay_map$group <- as.list(c(
    "hospitalised-alive", "hospitalised-alive", "hospitalised-alive",
    "hospitalised-dead", "hospitalised-dead",
    "community-dead"
  ))
  
  param_names <- c(
    "delay1_mean", "delay1_shape",
    "delay2_meanlog", "delay2_precisionlog",
    "delay3_mean", "delay3_shape",
    "delay4_mean", "delay4_shape",
    "delay5_meanlog", "delay5_precisionlog",
    "delay6_mean", "delay6_shape"
  )
  
  set.seed(1)
  mock_pars <- matrix(NA, nrow = length(param_names), ncol = 100,
                      dimnames = list(param_names, NULL))
  mock_pars["delay1_mean", ] <- runif(100, 4, 6)
  mock_pars["delay1_shape", ] <- runif(100, 2, 4)
  mock_pars["delay2_meanlog", ] <- runif(100, 1, 2)
  mock_pars["delay2_precisionlog", ] <- runif(100, 2, 5)
  mock_pars["delay3_mean", ] <- runif(100, 5, 10)
  mock_pars["delay3_shape", ] <- runif(100, 1.5, 3)
  mock_pars["delay4_mean", ] <- runif(100, 3, 7)
  mock_pars["delay4_shape", ] <- runif(100, 2, 5)
  mock_pars["delay5_meanlog", ] <- runif(100, 1.5, 2.5)
  mock_pars["delay5_precisionlog", ] <- runif(100, 2, 4)
  mock_pars["delay6_mean", ] <- runif(100, 10, 15)
  mock_pars["delay6_shape", ] <- runif(100, 3, 6)
  
  mock_mcmc_output <- list(pars = mock_pars)
  
  # unfiltered plot
  p_full <- chronofix_plot_delays(
    mock_mcmc_output, mock_delay_map, 
    n_points = 200, facet_by_group = FALSE, share_x_axis = FALSE
  )
  
  # peak x-value for "Onset to Death" from the full plot
  data_full <- p_full$data[p_full$data$Delay_Title == "Onset to Death", ]
  peak_full <- data_full$x[which.max(data_full$mean_density)]
  
  # filtered plot
  p_sub <- chronofix_plot_delays(
    mock_mcmc_output, mock_delay_map, 
    n_points = 200, facet_by_group = FALSE, share_x_axis = FALSE,
    select_delay = "onset to death"
  )
  
  # peak x-value for "Onset to Death" from the filtered plot
  data_sub <- p_sub$data[p_sub$data$Delay_Title == "Onset to Death", ]
  peak_sub <- data_sub$x[which.max(data_sub$mean_density)]
  
  # onset to death is the only delay with mean 10-15
  # check moving to position 1 hasn't pulled delay1's parameters (mean 4-6)
  expect_gt(peak_sub, 7)
  
  # peaks should be the same in both plots
  expect_equal(peak_sub, peak_full, tolerance = 1e-8)
  
})

test_that("chronofix_plot_delays respects plot_style argument", {
  
  mock_delay_map <- data.frame(
    from = "onset", to = "report", distribution = "gamma",
    stringsAsFactors = FALSE
  )
  mock_delay_map$group <- list("community")
  
  set.seed(1)
  # 100 draws
  mock_pars <- matrix(runif(200, 1, 5), nrow = 2, ncol = 100, 
                      dimnames = list(c("delay1_mean", "delay1_shape"), NULL))
  mock_mcmc_output <- list(pars = mock_pars)
  
  # "ribbon" default
  p_ribbon <- chronofix_plot_delays(
    mock_mcmc_output, mock_delay_map, 
    plot_style = "ribbon", facet_by_group = FALSE
  )
  
  pb <- ggplot_build(p_ribbon)
  expect_type(pb$layout$panel_params[[1]]$y$get_labels(), "character")
  
  # expect two layers: CrI ribbon, line
  expect_length(p_ribbon$layers, 2)
  expect_true(inherits(p_ribbon$layers[[1]]$geom, "GeomRibbon"))
  expect_true(inherits(p_ribbon$layers[[2]]$geom, "GeomLine"))
  expect_match(p_ribbon$labels$subtitle, "Shaded area: Pointwise 95% CrI")
  
  # "samples" style
  p_samples <- chronofix_plot_delays(
    mock_mcmc_output, mock_delay_map, 
    plot_style = "samples", facet_by_group = FALSE
  )
  
  expect_length(p_samples$layers, 2)
  has_ribbon <- any(vapply(p_samples$layers, 
                           function(l) inherits(l$geom, "GeomRibbon"), 
                           logical(1)))
  expect_false(has_ribbon)
  
  expect_true(inherits(p_samples$layers[[1]]$geom, "GeomLine"))
  expect_true(inherits(p_samples$layers[[2]]$geom, "GeomLine"))
  expect_match(p_samples$labels$subtitle, "Faint lines: Individual posterior draws")
  
  # every draw is plotted
  n_ids <- length(unique(p_samples$layers[[1]]$data$sample_id))
  expect_equal(n_ids, ncol(mock_pars))
  
  # rejects unknown plot style
  expect_error(
    chronofix_plot_delays(mock_mcmc_output, mock_delay_map,
                          plot_style = "spaghetti"),
    "should be one of"
  )
  
  # also works with facet_by_group = TRUE
  p_patch_samples <- chronofix_plot_delays(
    mock_mcmc_output, mock_delay_map, 
    plot_style = "samples", facet_by_group = TRUE
  )
  
  expect_s3_class(p_patch_samples, "patchwork")
  
  # default plot it ribbon
  p_default <- chronofix_plot_delays(mock_mcmc_output, mock_delay_map,
                                     facet_by_group = FALSE)
  expect_true(inherits(p_default$layers[[1]]$geom, "GeomRibbon"))
})
