#' Gracefully request a resource from Wikipedia
#'
#' The main purpose of this function is to enable examples using live resources
#' in the documentation. Examples must not throw errors, according to CRAN
#' policy. If you wrap a requesting method in `gracefully`, then any
#' errors of type `httr2_http` (HTTP errors) or `httr2_failure` (e.g. connection
#' failures) will be caught and no error will be thrown.
#'
#' If `request_object` is already a `wikkitidy_failure` (see
#' [is_wikkitidy_failure()]), it is returned unchanged without calling
#' `request_method`. This lets failures pass through a pipeline of
#' `gracefully()` calls.
#'
#' @param request_object A `httr2_request` object describing a query to a
#'   Wikimedia Action API
#' @param request_method The desired function for performing the request,
#'   typically one of those in [get_query_results]
#'
#' @return The output of `request_method` called on `request_object`, if the
#'   request was successful. Otherwise a `wikkitidy_failure` object wrapping
#'   the `httr2_response` (HTTP error) or the condition (connection failure).
#'   Test for it with [is_wikkitidy_failure()].
#' @export
#'
#' @keywords utility
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
#' # A connection failure is also caught
#' bad <- httr2::request("http://127.0.0.1:1") |>
#'   httr2::req_retry(max_tries = 1)
#'
#' failed <- gracefully(bad, httr2::req_perform)
#'
#' is_wikkitidy_failure(failed)
#'
#' # A failure short-circuits: the method is not called again
#' gracefully(failed, httr2::req_perform) |>
#'   is_wikkitidy_failure()
#'
#' # This request succeeds
#' req <- httr2::request(httr2::example_url())
#'
#' resp <- gracefully(req, httr2::req_perform)
#'
#' print(resp)
gracefully <- function(request_object, request_method) {
  if (is_wikkitidy_failure(request_object)) {
    return(request_object)
  }
  tryCatch(
    request_method(request_object),
    httr2_http = function(cnd) new_wikkitidy_failure(cnd$resp),
    httr2_failure = function(cnd) new_wikkitidy_failure(cnd)
  )
}
