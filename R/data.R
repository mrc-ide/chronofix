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
##' @param id Optional name of a column within `data` to use for
##'   unique individual identifiers.
##'
##' @param group Optional name of a column within `data` to use for
##'   groups
##'
##' @return A data.frame, with the addition of the class attribute
##'   `chronofix_data`; once created you should not modify this object.
##'
##' @export
chronofix_prepare_data <- function(data, id = NULL, group = NULL) {
  
  if (inherits(data, "chronofix_data")) {
    return(data)
  }
  
  if (!inherits(data, "data.frame")) {
    cli::cli_abort("Expected 'data' to be a 'data.frame' object")
  }
  
  if (nrow(data) == 0) {
    cli::cli_abort("Expected 'data' to have at least one row")
  }
  
  if (is.null(id)) {
    id <- "id"
    if (is.null(data[[id]])) {
      cli::cli_abort(
        "Did not find column '{id}' in 'data'")
    }
  } else {
    if (is.null(data[[id]])) {
      cli::cli_abort(
        c("Did not find column '{id}' in 'data'",
          i = "You provided the argument 'id'"))
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
        c("Did not find column '{group}' in 'data'",
          i = "You provided the argument 'group'"))
    }
  }
  
  if (length(setdiff(names(data), c(id, group))) < 2) {
    cli::cli_abort(
      paste("Expected 'data' to have at least two columns in addition to",
            "{squote(c(id, group))}"))
  }
  
  rownames(data) <- NULL
  attr(data, "id") <- id
  attr(data, "group") <- group
  class(data) <- c("chronofix_data", class(data))
  data
}
