#' Generate request for the [MediaWiki Action
#' API](https://www.mediawiki.org/wiki/API:Main_page)
#'
#' @param ... Parameters for the request
#' @param action The action to perform, typically 'query'
#' @param language The language edition of Wikipedia to request, e.g. 'en' or
#'   'fr'
#'
#' @return A httr2_response
#' @export
#'
#' @examples
#' # List the first 10 pages in the category 'Australian historians'
#'
#' resp <- wiki_action_request(
#'   list = "categorymembers",
#'   cmtitle = "Category:Australian_historians",
#'   cmlimit = 10
#' )
#' httr2::resp_body_json(resp)$query$categorymembers
#'
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
    httr2::req_user_agent("wikkidity R package (https://github.com/wikihistories/wikkidity") %>%
    httr2::req_perform()
}
