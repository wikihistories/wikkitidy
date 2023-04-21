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

#' Base request for [Wikimedia REST API](https://www.mediawiki.org/wiki/Wikimedia_REST_API)
#'
#' The [Wikimedia REST API](https://www.mediawiki.org/wiki/Wikimedia_REST_API) is one of
#' the three main APIs for WikiPedia and its sister projects. This particular API
#' is unique because its results are cached around the world can be downloaded with
#' low latency. As a result, it is also the only one of the main APIs that requires
#' users to throttle their usage.
#'
#' @inheritParams core_rest_request
#'
#' @return A `wikimedia_rest_request`, an S3 vector that subclasses
#'  `httr2_request` (see [httr2::request])
#' @export
#'
#' @examples
#' # Get the html of the 'Earth' article
#' response <- wikimedia_rest_request("page", "html", "Earth") %>%
#'   httr2::req_perform()
wikimedia_rest_request <- function(..., language = "en") {
  dots <- rlang::list2(...)
  if (length(dots) == 0) {
    rlang::abort("Invalid request: you have not provided any URL components")
  }
  rlang::inject(
    request <- httr2::request(
      glue::glue("https://{language}.wikipedia.org/api/rest_v1")
    ) %>%
      httr2::req_throttle(199 / 1) %>%
      httr2::req_url_path_append(!!!dots) %>%
      wikkitidy_user_agent()
  )
  class(request) <- c("wikimedia_rest_request", class(request))
  request
}

