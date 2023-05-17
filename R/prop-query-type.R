#' Query the [MediaWiki Action
#' API](https://www.mediawiki.org/wiki/API:Main_page) using a vector of
#' Wikipedia pages
#'
#' These functions help you to build a query for the [MediaWiki Action
#' API](https://www.mediawiki.org/wiki/API:Main_page) if you already have a set
#' of pages that you wish to investigate. These functions can be combined with
#' [query_page_properties] to choose which properties to return for the passed
#' pages.
#'
#' If you don't already know which pages you wish to examine, you can build a
#' query to find pages that meet certain criteria using [query_list_pages] or
#' [query_generate_pages].
#'
#' @param .req A [wiki_action_request] query to modify
#' @param title A character vector of page titles
#' @param pageid A character or numeric vector of page ids
#' @param revid A character or numeric vector of revision ids
#'
#' @name query_by_
#'
#' @return A request object of type `pages/query/action_api/httr2_request`. To
#'   perform the query, pass the object to [next_batch] or [retrieve_all]
#'
#' @examples
#' # Retrieve the categories for Charles Harpur's Wikipedia page
#'  resp <- wiki_action_request() %>%
#'   query_by_title("Charles Harpur") %>%
#'   query_page_properties("categories") %>%
#'   next_batch()
NULL

#' @rdname query_by_
#' @export
query_by_title <- function(.req, title) {
  # TODO: check `title` parameter
  new_prop_query(.req, "titles", title)
}

#' @rdname query_by_
#' @export
query_by_pageid <- function(.req, pageid) {
  # TODO: check `pageid` parameter
  new_prop_query(.req, "pageids", pageid)
}

#' @rdname query_by_
#' @export
query_by_revid <- function(.req, revid) {
  # TODO: check `revid` parameter
  new_prop_query(.req, "revids", revid)
}

#' Constructor for the property query type
#'
#' The intended use for this query is to set the 'titles', 'pageids' or 'revids'
#' parameter, and enforce that only one of these is set. All [property modules
#' API](https://www.mediawiki.org/wiki/API:Properties) in the Action API require
#' this parameter to be set, or they require a
#' [`generator`][new_generator_query] parameter to be set instead. The
#' `prop/query` type is an abstract type representing the three possible kinds
#' of property query that do not rely on a generator (see below on the return
#' value). A complication is that a `prop/query` can *itself* be used as the
#' basis for a generator.
#'
#' @param .req A [`query/action_api/httr2_request`][wiki_action_request] object,
#'   or a `prop` query object as returned by this function. This parameter is
#'   covariant on the type, so you can also pass all subtypes of `prop`.
#' @param by The type of page. Allowed values are: `r PROP_SUBTYPES`
#' @param pages A string, the pages to query by, corresponding to the 'by'
#'   parameter. Multiple values should be seperated with "|"
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Further parameters to the
#'   query
#'
#' @keywords low_level_action_api
#'
#' @return A properly qualified `prop/query` object. There are six
#'   possibilities:
#' * `titles/prop/query`
#' * `pageids/prop/query`
#' * `revids/prop/query`
#' * `generator/titles/prop/query`
#' * `generator/pageids/prop/query`
#' * `generator/revids/prop/query`
#' @export
new_prop_query <- function(.req, by, pages, ...) {
  UseMethod("new_prop_query")
}

PROP_SUBTYPES <- c("pageids", "titles", "revids")

#' @export
new_prop_query.prop <- function(.req, by, pages, ...) {
  check_prop_subtype(.req, by)
  NextMethod()
}

#' @export
new_prop_query.generator <- function(.req, by, pages, ...) {
  check_prop_subtype(.req, by, .default = "non-prop generator")
  req <- set_action(.req, by, pages, ...)
  req
}

#' @export
new_prop_query.list <- function(.req, by, pages, ...) {
  incompatible_query_error(paste0(by, "prop", collapse = "/"), "list")
}

#' @export
new_prop_query.query <- function(.req, by, pages, ...) {
  by <- rlang::arg_match(by, PROP_SUBTYPES)
  req <- rlang::inject(
    set_action(.req, !!by, pages, ...)
    )
  class(req) <- c(by, "prop", class(req))
  req
}

check_prop_subtype <- function(.req, by, .default = NULL) {
  if (prop_subtype(.req, .default) != by) {
    incompatible_query_error(by, prop_subtype(.req, .default))
  }
}

prop_subtype <- function(.req, .default = NULL) {
  subtype <- class(.req)[which(class(.req) == "prop")-1]
  if (rlang::is_empty(subtype)) {
    .default
  } else {
    subtype
  }
}

is_prop_query <- function(.req) {
  rlang::inherits_all(.req, c("prop", BASE_QUERY_CLASS))
}
