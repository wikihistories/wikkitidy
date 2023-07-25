#' Add required prefix to URL parameters for MediaWiki Action API request
#'
#' @param params A character vector
#' @param prefix A character vector
#'
#' @return A character vector
#' @keywords internal
prefix_params <- function(params, prefix) {
  unprefixed <- params[!startsWith(params, prefix)]
  prefixed <- paste0(prefix, unprefixed)
  params[!startsWith(params, prefix)] <- prefixed
  params
}
