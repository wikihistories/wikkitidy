#' Find out which other-language wikis have linked pages
#'
#' If the function fails to find any interwiki links for the passed title, it
#' will fail silently.
#'
#' @param titles A character vector of page titles
#' @param language A character vector of two-letter language codes. If it has
#'   a length of 1, it will be recycled.
#'
#' @return A list of [page_language_object] vectors.
#' @export
#'
#' @examples
#' # Get language links for a single page on English Wikipedia
#' get_langlinks("Charles_Harpur")
#'
#' # Get a tbl of pages from French Wikipedia using the Action API, then
#' # find their interwiki links
#' albums_des_beatles <- wiki_action_request(language = "fr") %>%
#'   query_generate_pages(
#'     "categorymembers",
#'     gcmtitle = "Catégorie:Album_des_Beatles",
#'     gcmtype = "page"
#'     ) %>%
#'   query_page_properties("title") %>%
#'   retrieve_all() %>%
#'   dplyr::mutate(links = get_langlinks(title, language = "fr"))
#' albums_des_beatles
#'
#' # Sort pages by number of links
#' albums_des_beatles %>%
#'   dplyr::mutate(count = purrr::map(links, length)) %>%
#'   dplyr::arrange(links, desc = TRUE)
get_langlinks <- function(titles, language = "en") {
  titles <- str_for_rest(titles)
  langlinks <-
    get_rest_resource("page", titles, "links", "language", language = language) %>%
    purrr::map(purrr::list_transpose) %>%
    purrr::map(
      \(x)
      rlang::inject(new_page_language_object(!!!x))
    )
  langlinks
}
