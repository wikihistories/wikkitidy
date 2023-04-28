#' Get data about pages from their titles
#'
#' @description `get_page_metadata()` returns [metadata about the latest
#'   revision of each
#'   page](https://en.wikipedia.org/api/rest_v1/#/Page%20content/get_page_title__title_).
#'
#'   `get_page_html()` returns [the rendered html for each
#'   page](https://en.wikipedia.org/api/rest_v1/#/Page%20content/get_page_html__title_).
#'
#'   `get_page_summary()` returns [metadata about the latest revision, along
#'   with the page description and a summary extracted from the opening
#'   paragraph](https://en.wikipedia.org/api/rest_v1/#/Page%20content/get_page_summary__title_)
#'
#'   `get_page_related()` returns summaries for [20 related pages for each
#'   passed
#'   page](https://en.wikipedia.org/api/rest_v1/#/Page%20content/getRelatedPages)
#'
#'   `get_page_talk()` returns [structured talk page content for each
#'   title](https://en.wikipedia.org/api/rest_v1/#/Talk%20pages/get_page_talk__title_)
#'
#'   `get_page_langlinks()` returns [interwiki links for each
#'   title](https://www.mediawiki.org/wiki/API:REST_API/Reference#Get_languages)
#'
#' @param title A character vector of page titles.
#' @param language A character vector of two-letter language codes, either of length 1 or the same length as `title`
#'
#' @return A list, vector or tibble, the same length as `title`, with the desired data.
#' @export
#'
#' @examples
#' # Get language links for a known page on English Wikipedia
#' get_page_langlinks("Charles Harpur")
get_page_metadata <- function(title, language = "en") {
  get_rest_resource(
    "page", "title", title,
    language = language, endpoint = "wikimedia"
  )
}

#' @rdname get_page_metadata
#' @export
get_page_html <- function(title, language = "en") {
  get_rest_resource(
    "page", "html", title,
    language = language, endpoint = "wikimedia", response_format = "html"
  )
}

#' @rdname get_page_metadata
#' @export
get_page_summary <- function(title, language = "en") {
  get_rest_resource(
    "page", "summary", title,
    language = language, endpoint = "wikimedia"
  )
}

#' @rdname get_page_metadata
#' @export
get_page_related <- function(title, language = "en") {
  get_rest_resource(
    "page", "related", title,
    language = language, endpoint = "wikimedia"
  )
}

#' @rdname get_page_metadata
#' @export
get_page_talk <- function(title, language = "en") {
  get_rest_resource(
    "page", "talk", title,
    language = language, endpoint = "wikimedia"
  )
}

#' @rdname get_page_metadata
#' @export
get_page_langlinks <- function(title, language = "en") {
  get_rest_resource(
    "page", title, "links", "language",
    language = language, response_type = "page_language_object"
  )
}
