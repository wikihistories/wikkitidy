#' Failed wikkitidy requests
#'
#' A `wikkitidy_failure` is returned by [gracefully()] when a request fails,
#' instead of throwing an error. It wraps either an `httr2_response` (HTTP
#' error) or a condition object (e.g. a connection failure).
#'
#' @param x An object to test.
#'
#' @return `is_wikkitidy_failure()` returns `TRUE` or `FALSE`.
#' @export
#' @keywords utility
#'
#' @examples
#' is_wikkitidy_failure(1)
is_wikkitidy_failure <- function(x) {
  inherits(x, "wikkitidy_failure")
}

new_wikkitidy_failure <- function(x) {
  stopifnot(inherits(x, c("httr2_response", "condition")))
  class(x) <- c("wikkitidy_failure", class(x))
  x
}

#' @export
print.wikkitidy_failure <- function(x, ...) {
  cli::cli_alert("The query you tried was unsuccessful. See details below.")
  print(unclass_failure(x), ...)
  invisible(x)
}

unclass_failure <- function(x) {
  class(x) <- setdiff(class(x), "wikkitidy_failure")
  x
}
