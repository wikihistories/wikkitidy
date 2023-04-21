#' Build a REST request
#'
#' @description `core_request_request()` builds a request for the [MediaWiki
#' Core REST API](https://www.mediawiki.org/wiki/API:REST_API) is the basic REST
#' API available on all MediaWiki wikis.
#'
#' `wikimedia_rest_request()` builds a request for the [Wikimedia REST
#' API](https://www.mediawiki.org/wiki/Wikimedia_REST_API) is an additional
#' endpoint just for Wikipedia and other wikis managed by the Wikimedia
#' Foundation
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Components to add to the URL.
#'   Unnamed arguments are added to the path of the request, while named
#'   arguments are added as query parameters. See above.
#' @param language The two-letter language code for the Wikipedia edition
#'
#' @return A `core/rest` or `wikimedia/rest` object, an S3 vector that
#'   subclasses `httr2_request` (see [httr2::request])
#'
#' @export
#'
#' @examples
#' # Get the html of the 'Earth' article on English Wikipedia
#' response <- core_rest_request("page", "Earth", "html") %>%
#'   httr2::req_perform()
#'
#' response <- wikimedia_rest_request("page", "html", "Earth") %>%
#'   httr2::req_perform()
core_rest_request <- function(..., language = "en") {
  request <- rest_request(..., endpoint = "w/rest.php/v1", language = language)
  class(request) <- c("core", class(request))
  request
}

#' @rdname core_rest_request
wikimedia_rest_request <- function(..., language = "en") {
  request <- rest_request(..., endpoint = "api/rest_v1", language = language) %>%
    httr2::req_throttle(199 / 1)
  class(request) <- c("wikimedia", class(request))
  request
}

rest_request <- function(..., endpoint = character(), language = "en") {
  dots <- rlang::dots_list(..., .named=FALSE)
  path_components <- dots[!rlang::have_name(dots)]
  if (length(path_components) == 0) {
    rlang::abort("Invalid request: you have not provided any URL components")
  }
  query_params <- dots[rlang::have_name(dots)]
  url <- glue::glue("https://{language}.wikipedia.org/")
  rlang::inject(
    request <- httr2::request(url) %>%
      wikkitidy_user_agent() %>%
      httr2::req_url_path_append(!!!endpoint) %>%
      httr2::req_url_path_append(!!!path_components) %>%
      httr2::req_url_query(!!!query_params)
  )
  class(request) <- c("rest", class(request))
  request
}
