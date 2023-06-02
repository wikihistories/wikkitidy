#' Build a REST request to one of Wikipedia's specific REST APIs
#'
#' @description `core_request_request()` builds a request for the [MediaWiki
#' Core REST API](https://www.mediawiki.org/wiki/API:REST_API), the basic REST
#' API available on all MediaWiki wikis.
#'
#' `wikimedia_rest_request()` builds a request for the [Wikimedia REST
#' API](https://www.mediawiki.org/wiki/Wikimedia_REST_API), an additional
#' api just for Wikipedia and other wikis managed by the Wikimedia
#' Foundation
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Components to add to the URL.
#'   Unnamed arguments are added to the path of the request, while named
#'   arguments are added as query parameters.
#' @param language The two-letter language code for the Wikipedia edition
#'
#' @return A `core/rest`, `wikimedia/rest`, object, an S3 vector that subclasses
#'   `httr2_request` (see [httr2::request]). The request needs to be passed to
#'   [httr2::req_perform] to retrieve data from the API.
#'
#' @name wikipedia_rest_apis
#'
#' @examples
#' # Get the html of the 'Earth' article on English Wikipedia
#' response <- core_rest_request("page", "Earth", "html") %>%
#'   httr2::req_perform()
#'
#' response <- wikimedia_rest_request("page", "html", "Earth") %>%
#'   httr2::req_perform()
#'
#' # Some REST requests take query parameters. Pass these as named arguments.
#' # To search German Wikipedia for articles about Goethe
#' response <- core_rest_request("search/page", q = "Goethe", limit = 2, language = "de") %>%
#'   httr2::req_perform() %>%
#'   httr2::resp_body_json()
NULL

#' @rdname wikipedia_rest_apis
#' @export
core_rest_request <- function(..., language = "en") {
  request <- wp_rest_request(..., api = "w/rest.php/v1", language = language)
  class(request) <- c("core", class(request))
  request
}

#' @rdname wikipedia_rest_apis
#' @export
wikimedia_rest_request <- function(..., language = "en") {
  request <- wp_rest_request(..., api = "api/rest_v1", language = language) %>%
    httr2::req_throttle(199 / 1, realm = "wikimedia_rest")
  class(request) <- c("wikimedia", class(request))
  request
}

#' Build a REST request to one of the Wikimedia Foundation's central APIs
#'
#' @description `wikimedia_org_rest_request()` builds a request for the
#'   [wikimedia.org REST API](https://wikimedia.org/api/rest_v1/), which
#'   provides statistical data about Wikimedia Foundation projects
#'
#'   `xtools_rest_request()` builds a request to the [XTools
#'   API](https://www.mediawiki.org/wiki/XTools/API), which provides additional
#'   statistical data about Wikimedia foundation projects
#'
#' @param endpoint The endpoint for the specific kind of request; for wikimedia
#'   apis, this comprises the path components in between the general API
#'   endpoint and the component specifying the project to query
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Components to add to the URL.
#'   Unnamed arguments are added to the path of the request, while named
#'   arguments are added as query parameters.
#' @param language Two-letter language code for the desired Wikipedia edition.
#'
#' @return A `wikimedia_org/rest` or `xtools/rest` object, an S3 vector that
#'   subclasses [httr2::request].
#'
#' @name wikimedia_rest_apis
NULL

#' @rdname wikimedia_rest_apis
#' @export
wikimedia_org_rest_request <- function(endpoint, ..., language = "en") {
  base_url <- "https://wikimedia.org/api/rest_v1/"
  project <- glue::glue("{language}.wikipedia.org")
  request <- wm_rest_request(..., base_url = base_url, project = project, endpoint = endpoint, language = language) %>%
    httr2::req_throttle(200, realm = "wikmedia_org")
  class(request) <- c("wikmedia_org", class(request))
  request
}

#' @rdname wikimedia_rest_apis
#' @export
xtools_rest_request <- function(endpoint, ..., language = "en") {
  base_url <- "https://xtools.wmflabs.org/api/"
  project <- glue::glue("{language}.wikipedia")
  request <- wm_rest_request(..., base_url = base_url, project = project, endpoint = endpoint, language = language)
  class(request) <- c("xtools_api", class(request))
  request
}

wp_rest_request <- function(..., api = character(), language = "en") {
  dots <- rest_dots(...)
  url <- glue::glue("https://{language}.wikipedia.org/")
  rlang::inject(
    request <- httr2::request(url) %>%
      wikkitidy_user_agent() %>%
      httr2::req_url_path_append(!!!api) %>%
      httr2::req_url_path_append(!!!dots$path) %>%
      httr2::req_url_query(!!!dots$query)
  )
  class(request) <- c("rest", class(request))
  request
}

wm_rest_request <- function(..., base_url, project, endpoint, language) {
  dots <- rest_dots(...)
  rlang::inject(
    request <- httr2::request(base_url) %>%
      wikkitidy_user_agent() %>%
      httr2::req_url_path_append(!!!endpoint) %>%
      httr2::req_url_path_append(project) %>%
      httr2::req_url_path_append(!!!dots$path) %>%
      httr2::req_url_query(!!!dots$query)
  )
  class(request) <- c("rest", class(request))
  request
}

rest_dots <- function(...) {
  dots <- rlang::dots_list(..., .named = FALSE)
  if (length(dots) == 0) {
    rlang::abort(
      "no path components provided for REST request"
    )
  }
  path_components <- dots[!rlang::have_name(dots)]
  query_params <- dots[rlang::have_name(dots)]
  list("path" = path_components, "query" = query_params)
}
