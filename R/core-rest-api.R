#' Base request for [MediaWiki Core REST API](https://www.mediawiki.org/wiki/API:REST_API)
#'
#' The [MediaWiki Core REST API](https://www.mediawiki.org/wiki/API:REST_API) is
#' the basic REST API available on all MediaWiki wikis. It is not as performant
#' as the [Wikimedia REST API](https://www.mediawiki.org/wiki/Wikimedia_REST_API),
#' which is used by [wikimedia_rest_request].
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
#'     "revisions", titles="Main_page", rvlimit=5, rvprop="ids", rvdir="older"
#'   ) %>%
#'   perform_query_once() %>%
#'   tidyr::hoist(revisions, "parentid", "revid")
#' diffs <- get_diff(from=revisions$parentid, to=revisions$revid)
get_diff <- function(from, to, language = "en") {
  if (length(from) != length(to)) {
    stop("Arguments must be the same length: length(rev_from) == ",
         length(from),
         " length(rev_to) == ",
         length(to))
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
