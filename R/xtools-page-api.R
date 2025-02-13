#' Access page-level statistics from the [XTools Page API endpoint](https://www.mediawiki.org/wiki/XTools/API/Page)
#'
#' @description `get_xtools_page_info()` returns [basic
#'   statistics](https://www.mediawiki.org/wiki/XTools/API/Page#Article_info)
#'   about articles' history and quality, including their total edits, creation
#'   date, and assessment value (good, featured etc.)
#'
#'   `get_xtools_page_prose()` returns [statistics about the word counts and
#'   referencing](https://www.mediawiki.org/wiki/XTools/API/Page#Prose) of
#'   articles
#'
#'   `get_xtools_page_links()` returns [the number of ingoing and outgoing links
#'   to articles, including
#'   redirects](https://www.mediawiki.org/wiki/XTools/API/Page#Links)
#'
#'   `get_xtools_page_top_editors()` returns the [list of top editors for
#'   articles](https://www.mediawiki.org/wiki/XTools/API/Page#Top_editors), with
#'   optional filters by date range and non-bot status
#'
#'   `get_xtools_page_assessment()` returns more detailed [statistics about
#'   articles' assessment status and Wikiproject importance
#'   levels](https://www.mediawiki.org/wiki/XTools/API/Page#Assessments)
#'
#' @param title Character vector of page titles
#' @param language Language code for the version of Wikipedia to query
#' @param start A character vector or date object (optional): the start date for
#'   calculating top editors
#' @param end A character vector or date object (optional): the end date for
#'   calculating top editors
#' @param limit An integer: the maximum number of top editors to return
#' @param nobots TRUE or FALSE: if TRUE, bots are excluded from the top editor
#'   calculation
#' @param classonly TRUE or FALSE: if TRUE, only return the article's assessment
#'   status, without Wikiproject information
#' @param failure_mode What to do if no data is found. See [get_rest_resource()]
#'
#' @name xtools_page
#'
#' @return A list or tbl of results, the same length as `title`. **NB:** The
#'   results for `get_xtools_page_assessment` are still not parsed properly.
#'
#' @examples
#' # Get basic statistics about Erich Auerbach on German Wikipedia
#' auerbach <- get_xtools_page_info("Erich Auerbach", language = "de", failure_mode = "quiet")
#' auerbach
NULL

#' @rdname xtools_page
#' @export
get_xtools_page_info <- function(title, language = "en", failure_mode = "error") {
  get_rest_resource(
    endpoint = "page/articleinfo",
    title,
    api = "xtools", language = language, response_type = "row_list", failure_mode = failure_mode
  )
}

#' @rdname xtools_page
#' @export
get_xtools_page_prose <- function(title, language = "en", failure_mode = c("error", "quiet")) {
  get_rest_resource(
    endpoint = "page/prose",
    title,
    api = "xtools", language = language, response_type = "row_list", failure_mode = failure_mode
  )
}

#' @rdname xtools_page
#' @export
get_xtools_page_links <- function(title, language = "en", failure_mode = c("error", "quiet")) {
  get_rest_resource(
    endpoint = "page/links",
    title,
    api = "xtools", language = language, response_type = "row_list", failure_mode = failure_mode
  )
}

#' @rdname xtools_page
#' @export
get_xtools_page_top_editors <- function(title, start = NULL, end = NULL, limit = 1000, nobots = FALSE, language = "en", failure_mode = c("error", "quiet")) {
  start <- datetime_for_url(start, .default = "/")
  end <- datetime_for_url(end, .default = "/")
  check_limit(limit, 1000)
  nobots <- one_if_true(nobots)
  get_rest_resource(
    endpoint = "page/top_editors",
    title, start, end, limit, nobots = nobots,
    language = language, api = "xtools",
    response_type = "row_list",
    failure_mode = failure_mode
  )
}

#' @rdname xtools_page
#' @export
get_xtools_page_assessment <- function(title, classonly = FALSE, language = "en", failure_mode = c("error", "quiet")) {
  classonly <- one_if_true(classonly)
  get_rest_resource(
    endpoint = "page/assessments",
    title, classonly = classonly,
    language = language, api = "xtools",
    response_type = "assessment_table",
    failure_mode = failure_mode
  )
}

#' @export
parse_response.assessment_table <- function(response) {
  # TODO: This one's hard!
  response
}
