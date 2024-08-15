#' Query Wikipedia using the [MediaWiki Action
#' API](https://www.mediawiki.org/wiki/API:Main_page)
#'
#' Wikipedia exposes a To build up a query, you first call
#' [wiki_action_request()] to create the basic request object, then use the
#' helper functions [query_page_properties()], [query_list_pages()] and
#' [query_generate_pages()] to modify the request, before calling [next_batch()]
#' or [retrieve_all()] to perform the query and download results from the
#' server.
#'
#' [wikkitidy] provides an ergonomic API for the Action API's [Query
#' modules](https://www.mediawiki.org/wiki/API:Query). These modules are most
#' useful for researchers, because they allow you to explore the structure of
#' Wikipedia and its back pages. You can obtain a list of available modules in
#' your R console using [list_all_property_modules()], [list_all_list_modules()]
#' and [list_all_generators()],
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Parameters for the request
#' @param action The action to perform, typically 'query'
#' @param language The language edition of Wikipedia to request, e.g. 'en' or
#'   'fr'
#'
#' @return An `action_api` object, an S3 list that subclasses [httr2::request].
#'   The dependencies between different aspects of the Action API are complex.
#'   At the time of writing, there are five major subclasses of
#'   `action_api/httr2_request`:
#'
#' * `generator/action_api/httr2_request`, returned (sometimes) by [query_generate_pages]
#' * `list/action_api/httr2_request`, returned by [query_list_pages]
#' * `titles`, `pageids` and `revids/action_api/httr2_request`, returned by the various [query_by_] functions
#'
#'   You can use [query_page_properties] to modify any kind of query *except*
#'   for `list` queries: indeed, the central limitation of the `list` queries is
#'   that you cannot choose what properties to return for the pages the meet the
#'   given criterion. The concept of a `generator` is complex. If the
#'   `generator` is based on a
#'   [property](https://www.mediawiki.org/wiki/API:Properties) module, then it
#'   must be combined with a [query_by_] function to produce a valid query. If
#'   the generator is based on a [list
#'   module](https://www.mediawiki.org/wiki/API:Lists), then it *cannot* be
#'   combined with a [query_by_] query.
#' @export
#'
#' @seealso [gracefully()]
#'
#' @examples
#' # List the first 10 pages in the category 'Australian historians'
#' historians <- wiki_action_request() %>%
#'   query_list_pages(
#'     "categorymembers",
#'     cmtitle = "Category:Australian_historians",
#'     cmlimit = 10
#'   ) %>%
#'   gracefully(next_batch)
#' historians
wiki_action_request <- function(..., action = "query", language = "en") {
  base_url <- glue::glue("https://{language}.wikipedia.org/w/api.php")
  params <- rlang::list2(
    action = action,
    format = "json",
    formatversion = "2",
    ...
  )
  req <- httr2::request(base_url) %>%
    httr2::req_url_query(!!!params) %>%
    wikkitidy_user_agent()
  structure(
    req,
    class = c(action, "action_api", class(req))
  )
}

# Helpers for enforcing type restrictions on Action API request objects
BASE_QUERY_CLASS <- c("query", "action_api", "httr2_request")

is_action_query <- function(.req) {
  rlang::inherits_all(.req, BASE_QUERY_CLASS)
}

check_is_action_query <- function(.req) {
  if (!is_action_query(.req)) {
    rlang::abort(
      "this is not a request to a query module of the MediaWiki Action API",
      class = "wrong_request_type"
    )
  }
}

is_base_query <- function(.req) {
  rlang::inherits_only(.req, BASE_QUERY_CLASS)
}

is_query_subtype <- function(.req, subtype) {
  rlang::inherits_all(.req, c(subtype, BASE_QUERY_CLASS))
}

incompatible_query_error <- function(new_type, old_type) {
  rlang::abort(
    glue::glue("you cannot combine a `{new_type}` query with an `{old_type}` query in the Action API"),
    class = "incompatible_query_error")
}

# Helpers for validating existence of query modules
is_module <- function(module, group) {
  schema_query_modules %>%
    dplyr::filter(group == group) %>%
    dplyr::summarise(is_module = module %in% name) %>%
    .$is_module
}

check_module <- function(module, group) {
  if (!is_module(module, group)) {
    rlang::abort(
      glue::glue("`{module}` is not a known `{group}` query to the Action API"),
      class = "unknown_module"
    )
  }
}

# Helpers for modifying Action API query objects
set_action <- function(.req, action_type, action, ...) {
  action_sym <- rlang::ensym(action_type)
  action_string <- combine_query_params(.req, action_type, action)
  action_params <- rlang::list2(...)
  httr2::req_url_query(.req = .req, !!action_sym := action_string, !!!action_params)
}

combine_query_params <- function(.req, param_type, param) {
  url <- httr2::url_parse(.req$url)
  existing <- purrr::pluck(url, "query", param_type)
  if (!is.null(existing)) {
    paste0(c(existing, param), collapse = "|")
  } else {
    paste0(param, collapse = "|")
  }
}
