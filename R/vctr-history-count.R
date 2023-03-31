#' Vector of history count objects returned from the [Core REST
#' API](https://www.mediawiki.org/wiki/API:REST_API/Reference#Response_schema_2)
#'
#' @param value Integer vector of history counts
#' @param limit Boolean vector indicating whether count is at upper limit
#' @param type Type of edits requested
#' @param request The httr2_request used to retrieve the counts
#'
#' @return An integer vector of history counts, which handles
#'
#' @keywords data_type
#'
history_count <- function(value, limit, type, request) {
  if (length(value) != length(limit)) {
    rlang::abort("value and limit have different lengths")
  }
  vctrs::new_vctr(value, limit = limit, type = type, request = request, class = "history_count")
}
