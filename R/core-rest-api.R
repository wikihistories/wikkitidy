#' Base request for [MediaWiki Core REST
#' API](https://www.mediawiki.org/wiki/API:REST_API)
#'
#' The [MediaWiki Core REST API](https://www.mediawiki.org/wiki/API:REST_API) is
#' the basic REST API available on all MediaWiki wikis.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Components to add to the URL.
#'  These are passed to [httr2::req_url_path_append]. If you pass named arguments,
#'  the names will be dropped.
#' @param language The two-letter language code for the Wikipedia edition
#'
#' @return A `core_rest_request`, an S3 vector that subclasses `httr2_request`
#'   (see [httr2::request])
#'
#' @export
#'
#' @examples
#' # Get the html of the 'Earth' article on English Wikipedia
#' response <- core_rest_request("page", "Earth", "html") %>%
#'   httr2::req_perform()
core_rest_request <- function(..., language = "en") {
  dots <- rlang::list2(...)
  if (length(dots) == 0) {
    rlang::abort("Invalid request: you have not provided any URL components")
  }
  request <- rlang::inject(
    httr2::request(
      glue::glue("https://{language}.wikipedia.org/w/rest.php/v1")
    ) %>%
      wikkitidy_user_agent() %>%
      httr2::req_url_path_append(!!!dots)
  )
  class(request) <- c("core_rest_request", class(request))
  request
}
