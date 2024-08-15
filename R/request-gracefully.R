#' Gracefully request a resource from Wikipedia
#'
#' The main purpose of this function is to enable examples using live resources
#' in the documentation. Examples must not throw errors, according to CRAN
#' policy. If you wrap a requesting method in `gracefully`, then any
#' errors of type `httr2_http` will be caught and no error will be thrown.
#'
#'
#' @param request_object A `httr2_request` object describing a query to a
#'   Wikimedia Action API
#' @param request_method The desired function for performing the request,
#'   typically one of those in [get_query_results]
#'
#' @return The output of `request_method` called on `request_object`, if the
#'   request was successful. Otherwise a `httr2_response` object with details
#'   of the failed request.
#' @export
#'
#' @examplesIf rlang::is_installed("webfakes")
#' # This fails without throwing an error
#' req <- httr2::request(httr2::example_url()) |>
#'   httr2::req_url_path("/status/404")
#'
#' resp <- gracefully(req, httr2::req_perform)
#'
#' print(resp)
#'
#' # This request succeeds
#' req <- httr2::request(httr2::example_url())
#'
#' resp <- gracefully(req, httr2::req_perform)
#'
#' print(resp)
gracefully <- function(request_object, request_method) {
  tryCatch(
    request_method(request_object),
    httr2_http = function(cnd) show_bad_response(cnd)
  )
}

show_bad_response <- function(cnd) {
  cli::cli_alert("The query you tried was unsuccessful. See the response below.")
  print(cnd$resp)
  return(cnd$resp)
}
