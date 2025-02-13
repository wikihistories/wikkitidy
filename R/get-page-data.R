#' Get data about pages from their titles
#'
#' @description `get_latest_revision()` returns metadata about the latest
#'   revision of each
#'   page.
#'
#'   `get_page_html()` returns the rendered html for each
#'   page.
#'
#'   `get_page_summary()` returns metadata about the latest revision, along
#'   with the page description and a summary extracted from the opening
#'   paragraph
#'
#'   `get_page_related()` returns summaries for 20 related pages for each
#'   passed page
#'
#'   `get_page_talk()` returns structured talk page content for each
#'   title. You must ensure to use the title for the Talk page itself, e.g.
#'   "Talk:Earth" rather than "Earth"
#'
#'   `get_page_langlinks()` returns interwiki links for each
#'   title
#'
#' @param title A character vector of page titles.
#' @param language A character vector of two-letter language codes, either of
#'   length 1 or the same length as `title`
#' @param failure_mode Either "quiet" or "error." See [get_rest_resource()]
#'
#' @return A list, vector or tibble, the same length as `title`, with the
#'   desired data.
#'
#' @name page_vector_functions
#'
#' @examples
#' # Get language links for a known page on English Wikipedia
#' get_page_langlinks("Charles Harpur", failure_mode = "quiet")
#'
#' # The functions are vectorised over title and language
#' # Find all articles about Joanna Baillie, and retrieve summary data for
#' # the first two.
#' baillie <- get_page_langlinks("Joanna Baillie") %>%
#'   dplyr::slice(1:2) %>%
#'   dplyr::mutate(get_page_summary(title = title, language = code, failure_mode = "quiet"))
#' baillie
NULL

#' @rdname page_vector_functions
#' @export
get_latest_revision <- function(title, language = "en", failure_mode = "error") {
  get_rest_resource(
    "page", "title", title,
    language = language, api = "wikimedia", response_type = "revision_metadata",
    failure_mode = failure_mode
  )
}

#' @export
parse_response.revision_metadata <- function(response) {
  purrr::map(response, "items") %>%
    purrr::map(1) %>%
    purrr::list_transpose() %>%
    tibble::as_tibble()
}

#' @rdname page_vector_functions
#' @export
get_page_html <- function(title, language = "en", failure_mode = "error") {
  get_rest_resource(
    "page", "html", title,
    language = language, api = "wikimedia", response_format = "html",
    failure_mode = failure_mode
  )
}

#' @rdname page_vector_functions
#' @export
get_page_summary <- function(title, language = "en", failure_mode = "error") {
  get_rest_resource(
    "page", "summary", title,
    language = language, api = "wikimedia",
    response_type = "summary",
    failure_mode = failure_mode
  )
}

#' @export
parse_response.summary <- function(response) {
  flatten_bind(response)
}

#' @rdname page_vector_functions
#' @export
get_page_talk <- function(title, language = "en", failure_mode = "error") {
  talk_pattern <- "^\\w+:"
  if (!all(stringr::str_detect(title, talk_pattern))) {
    rlang::abort("One or more titles do not begin with 'Talk:' or similar",
                 class="bad_title")
  }
  get_rest_resource(
    "page", "talk", title,
    language = language, api = "wikimedia",
    failure_mode = failure_mode
  )
}

#' @rdname page_vector_functions
#' @export
get_page_langlinks <- function(title, language = "en", failure_mode = "error") {
  get_rest_resource(
    "page", title, "links", "language",
    language = language, response_type = "page_language_object",
    failure_mode = failure_mode
  )
}

#' @export
parse_response.page_language_object <- function(response) {
  purrr::map(response, dplyr::bind_rows)
}
