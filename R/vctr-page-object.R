#' A page object returned from the [Core REST
#' API](https://www.mediawiki.org/wiki/API:REST_API/Reference#Page_object)
#'
#' @param x List of page objects in JSON format from the REST API
#'
#' @return A page object vector
#' @export
#'
#' @keywords data_type
#'
page_object <- function(x) {
  vctrs::new_vctr(x, class = "page_object")
}
