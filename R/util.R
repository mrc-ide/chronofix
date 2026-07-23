date_to_int <- function(date, origin = "1970-01-01") {
  int <- as.integer(as.Date(date) - as.Date(origin))
  
  d <- dim(date)
  if (!is.null(d)) {
    int <- array(int, d)
  }
  
  int
}


int_to_date <- function(int, origin = "1970-01-01") {
  int + as.Date(origin)
}


data_frame_to_array <- function(df) {
  array(unlist(df), dim(df))
}


`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


squote <- function(x) {
  sprintf("'%s'", x)
}


vlapply <- function(...) {
  vapply(..., FUN.VALUE = TRUE)
}


vnapply <- function(...) {
  vapply(..., FUN.VALUE = 1)
}
