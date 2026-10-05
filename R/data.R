##' Prepare data for use with `chronofix`. You do not have to use this
##' function if you name your data.frame with our standard column names
##' (i.e., `id` column containing individual IDs) as it will be called 
##' within other functions directly.  However, you can use this to
##' validate your data separately or to use different column names
##' than the defaults.
##'
##' @title Prepare data
##'
##' @param data A data.frame containing IDs and dates.  By
##'   default we expect a column `id` (or one with the name given as
##'   the argument `id`) and two or more date columns.
##' 
##' @param format A character string declaring the date format. The default
##'   is `"%Y-%m-%d"` (e.g. `"2015-06-21"`). For guidance on formats see
##'   `help(strptime)`
##'
##' @param id Optional name of a column within `data` to use for
##'   unique individual identifiers.
##'
##' @param group Optional name of a column within `data` to use for
##'   groups
##'
##' @return A data.frame, with the addition of the class attribute
##'   `chronofix_data`; once created you should not modify this object.
##'   
##' @importFrom rlang abort
##' @importFrom cli format_inline
##'
##' @export
chronofix_prepare_data <- function(data, format = "%Y-%m-%d", 
                                   id = NULL, group = NULL) {
  
  if (inherits(data, "chronofix_data")) {
    return(data)
  }
  
  if (!inherits(data, "data.frame")) {
    cli::cli_abort("Expected {.arg data} to be a 'data.frame' object")
  }
  
  if (nrow(data) == 0) {
    cli::cli_abort("Expected {.arg data} to have at least one row")
  }
  
  if (is.null(id)) {
    id <- "id"
    if (is.null(data[[id]])) {
      cli::cli_abort(
        "Did not find column {.col {id}} in {.arg data}")
    }
  } else {
    if (is.null(data[[id]])) {
      cli::cli_abort(
        c("Did not find column {.col {id}} in {.arg data}",
          i = "You provided the argument {.arg id}"))
    }
  }
  
  if (any(is.na(data[[id]]))) {
    na_rows <- which(is.na(data[[id]]))
    
    cli::cli_abort(c(
      "The {.col id} column in {.arg data} cannot contain missing values (`NA`).",
      "x" = "Found missing ID{?s} on row{?s}: {.val {na_rows}}"
    ))
  }
  
  if (any(duplicated(data[[id]]))) {
    duplicate_ids <- unique(data$id[duplicated(data[[id]])])
    
    cli::cli_abort(c(
      "The {.col id} column in {.arg data} must contain unique values.",
      "x" = "Found duplicate ID{?s}: {.val {duplicate_ids}}"
    ))
  }
  
  if (is.null(group)) {
    if ("group" %in% names(data)) {
      group <- "group"
    }
  } else {
    if (is.null(data[[group]])) {
      cli::cli_abort(
        c("Did not find column {.col {group}} in {.arg data}",
          i = "You provided the argument {.arg group}"))
    }
  }
  
  date_cols <- setdiff(names(data), c(id, group))
  
  if (length(date_cols) < 2) {
    cli::cli_abort(
      paste("Expected {.arg data} to have at least two columns in addition to",
            "{squote(c(id, group))}"))
  }
  
  prepared_data <- data
  for (nm in date_cols) {
    prepared_data[[nm]] <- as.Date(data[[nm]], format = format)
  }
  
  ex <- format(as.Date("2026-10-23"), format = format)
  date_format_correct <- !is.na(prepared_data[, date_cols])
  if (all(!date_format_correct)) {
    cli::cli_abort(
      c("All dates not matching declared format {.val {format}}"), 
        i = "Example valid date: {.val {ex}}")
  }
  
  date_format_error <- 
    !is.na(data[, date_cols]) & is.na(prepared_data[, date_cols])
  if (any(date_format_error)) {
    rows_error <- rowSums(date_format_error) > 0
    ex <- format(as.Date("2026-10-23"), format = format)
    msg <- cli::format_inline(
      "Some dates not matching declared format {.val {format}}")
    info <- cli::format_inline("Example valid date: {.val {ex}}")
    rlang::abort(
      c(msg, i = info, x = "Rows with incorrectly formatted dates:"),
      body = data[rows_error, ] %>% capture.output())
  }
  
  rownames(prepared_data) <- NULL
  attr(prepared_data, "id") <- id
  attr(prepared_data, "group") <- group
  attr(prepared_data, "format") <- format
  class(prepared_data) <- c("chronofix_data", class(data))
  prepared_data
}
