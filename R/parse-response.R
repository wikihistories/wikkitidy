#' Convert a response from a Wikipedia API into a convenient format
#'
#' Wikipedia's APIs provide data using a range of different json schemas.
#' This generic function converts the data into a convenient formats for use
#' in an R data frame.
#'
#' @param response The data retrieved from Wikipedia.
#'
#' @return A vector the same length as the response. Generally, this will be
#'  a simple vector, a [tibble::tbl_df] or a list of [tibble::tbl_df] objects.
#' @export
#'
#' @keywords internal
parse_response <- function(response) {
  UseMethod("parse_response")
}

#' @export
#' @describeIn parse_response By default, create a list of nested tbl_dfs
parse_response.default <- function(response) {
  parsed <- purrr::map(response, dplyr::bind_rows)
  parsed
}

flatten_bind <- function(response) {
  parsed <- purrr::map(response, purrr::list_flatten)
  parsed <- purrr::list_transpose(parsed)
  parsed <- tibble::tibble(!!!parsed)
  parsed
}
