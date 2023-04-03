#' Base request for [MediaWiki Core REST
#' API](https://www.mediawiki.org/wiki/API:REST_API)
#'
#' The [MediaWiki Core REST API](https://www.mediawiki.org/wiki/API:REST_API) is
#' the basic REST API available on all MediaWiki wikis.
#'
#' @inheritParams wikimedia_rest_request
#'
#' @return A `core_rest_request`, an S3 vector that subclasses `httr2_request`
#'   (see [httr2::request])
#'
#' @export
#'
#' @examples
#' # Get the html of the 'Earth' article
#' response <- core_rest_request() %>%
#'   httr2::req_url_path_append("page", "Earth", "html") %>%
#'   httr2::req_perform()
core_rest_request <- function(language = "en") {
  request <- httr2::request(
    glue::glue("https://{language}.wikipedia.org/w/rest.php/v1")
  ) %>%
    wikkitidy_user_agent()
  class(request) <- c("core_rest_request", class(request))
  request
}

.get_one_resource <- function(..., language) {
  core_rest_request(language = language) %>%
    httr2::req_url_path_append(...) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()
}
