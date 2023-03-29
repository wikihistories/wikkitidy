#' Base request for [MediaWiki Core REST API](https://www.mediawiki.org/wiki/API:REST_API)
#'
#' The [MediaWiki Core REST API](https://www.mediawiki.org/wiki/API:REST_API) is
#' the basic REST API available on all MediaWiki wikis.
#'
#' @inheritParams wikimedia_rest_request
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
}

#' Search for insertions, deletions or relocations of text between two versions
#' of a Wikipedia page
#'
#' Any two revisions of a Wikipedia page can be compared using the 'diff' tool.
#' The tool compares the 'from' revision to the 'to' revision, looking for
#' insertions, deletions or relocations of text. This operation can be performed
#' in any order, across any span of revisions.
#'
#' @param from Vector of revision ids
#' @param to Vector of revision ids
#' @param language Two-letter language code for Wikipedia
#'
#' @return A list the same length as `from` and `to`, comprising
#'   [wikidiff2
#'   responses](https://www.mediawiki.org/wiki/API:REST_API/Reference#Response_schema_3),
#'   represented as R lists
#' @export
#'
#' @examples
#' # Compare revision 847170467 to 851733941 on English Wikipedia
#' diffs <- get_diff(847170467, 851733941)
#'
#' # Each wikidiff2 response contains three main keys:
#' names(diffs[[1]])
#' # [1] "from" "to" "diff"
#'
#' # To see the text that has been inserted, deleted, or moved, look in the
#' # `diff` key, which contains a numbered list of differences
#' diffs[[1]]$diff[[2]]$text
#'
#' # In this case, the diff is of type `0`, which means an insertion
#' diffs[[1]]$diff[[2]]$type
#'
#' # The function is vectorised, so you can compare multiple pairs of revisions
#' # in a single call
#' # See diffs for the last five revisions of the Main Page
#' revisions <- wiki_action_request() %>%
#'   query_page_properties(
#'     "revisions",
#'     titles = "Main_Page", rvlimit = 5, rvprop = "ids", rvdir = "older"
#'   ) %>%
#'   perform_query_once() %>%
#'   tidyr::hoist(revisions, "parentid", "revid")
#' diffs <- get_diff(from = revisions$parentid, to = revisions$revid)
get_diff <- function(from, to, language = "en") {
  if (length(from) != length(to)) {
    stop(
      "Arguments must be the same length: length(rev_from) == ",
      length(from),
      " length(rev_to) == ",
      length(to)
    )
  }
  get_one_diff <- purrr::partial(.get_one_diff, lang = language)
  purrr::map2(from, to, get_one_diff)
}

.get_one_diff <- function(from, to, lang) {
  core_rest_request(language = lang) %>%
    httr2::req_url_path_append("revision", from, "compare", to) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()
}

#' Find out which other-language wikis have linked pages
#'
#' All pages passed to the function must be from the same language edition of
#' Wikipedia. For example, if you have a set of English pages, you can find all
#' the interwiki links for those pages. If you have a set of pages from
#' disparate language editions, you can segment the data using [dplyr::group_by]
#' or similar.
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

.get_one_resource <- function(..., language) {
  core_rest_request(language = language) %>%
    httr2::req_url_path_append(...) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()
}

.process_rest_params <- function(...) {

}
