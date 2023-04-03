#' Base request for [Wikimedia REST API](https://www.mediawiki.org/wiki/Wikimedia_REST_API)
#'
#' The [Wikimedia REST API](https://www.mediawiki.org/wiki/Wikimedia_REST_API) is one of
#' the three main APIs for WikiPedia and its sister projects. This particular API
#' is unique because its results are cached around the world can be downloaded with
#' low latency. As a result, it is also the only one of the main APIs that requires
#' users to throttle their usage.
#'
#' @param language The language edition of Wikipedia you wish to access
#'
#' @return A `wikimedia_rest_request`, an S3 vector that subclasses
#'  `httr2_request` (see [httr2::request])
#' @export
#'
#' @examples
#' # Get the html of the 'Earth' article
#' response <- wikimedia_rest_request() %>%
#'   httr2::req_url_path_append("page", "html", "Earth") %>%
#'   httr2::req_perform()
wikimedia_rest_request <- function(language = "en") {
  request <- httr2::request(
    glue::glue("https://{language}.wikipedia.org/api/rest_v1")
  ) %>%
    httr2::req_throttle(199 / 1) %>%
    wikkitidy_user_agent()
  class(request) <- c("wikimedia_rest_request", class(request))
  request
}
