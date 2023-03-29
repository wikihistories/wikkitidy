#' Find out which other-language wikis have linked pages
#'
#' If the function fails to find any interwiki links for the passed title, it
#' will fail silently.
#'
#' @param titles A character vector of page titles
#' @param language The language edition of Wikipedia that the titles come from
#'
#' @return A list of arrays of [page language
#'   objects](https://www.mediawiki.org/wiki/API:REST_API/Reference#Page_language_object),
#'   the same length as the passed list of `titles`. Each page language object
#'   has keys: `code` and `name` describe the language edition in which the
#'   linked page was found; while `key` and `title` identify the page on the
#'   linked wiki.
#' @export
#'
#' @examples
#' # Get language links for a single page on English Wikipedia
#' get_langlinks("Charles_Harpur")
#'
#' # Get a tbl of pages from French Wikipedia using the Action API, then
#' # find their interwiki links
#' albums_des_beatles <- wiki_action_request(language = "fr") %>%
#'   query_generate_pages("categorymembers", gcmtitle = "Catégorie:Album_des_Beatles", gcmtype = "page") %>%
#'   query_page_properties("title") %>%
#'   retrieve_all() %>%
#'   dplyr::mutate(links = get_langlinks(title, language = "fr"))
#'
#' # links is a list column, so [tidyr] and [purrr] are useful
#' # Count the number of langlinks for each page
#' albums_des_beatles %>% dplyr::mutate(
#'   count = purrr::map_dbl(links, length)
#' )
#' # Unnest the data frame to get one row per interwiki link, and extract
#' # the language code and page title for each interwiki link
#' albums_des_beatles %>%
#'   tidyr::unnest(links) %>%
#'   tidyr::hoist(links, "code", linked_title = "title")
get_langlinks <- function(titles, language = "en") {
  titles <- str_for_rest(titles)
  params <- vctrs::vec_recycle_common(titles, language)
  langlinks <-
    purrr::pmap(
      params,
      \(title, lang) .get_one_resource("page", title, "links", "language", language=lang)
    )
}
