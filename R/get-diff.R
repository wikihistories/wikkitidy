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
#' @param language Vector of two-letter language codes (will be recycled if
#'   length==1)
#' @param simplify logical: should R simplify the result (see [return])
#'
#' @return The return value depends on the `simplify` parameter.
#' * If `simplify` == TRUE: A list of [tibble::tbl_df] objects the same
#'   length as `from` and `to`. Most of the response data is stripped away,
#'   leaving just the textual differences between the revisions, their location,
#'   type and 'highlightRanges' if the textual differences are complicated.
#'  * If `simplify` == FALSE: A list the same length as `from` and `to`
#'   containing the full [wikidiff2
#'   response](https://www.mediawiki.org/wiki/API:REST_API/Reference#Response_schema_3)
#'   for each pair of revisions. This response includes additional data for
#'   displaying diffs onscreen.
#' @export
#'
#' @examples
#' # Compare revision 847170467 to 851733941 on English Wikipedia
#' get_diff(847170467, 851733941)
#'
#' # The function is vectorised, so you can compare multiple pairs of revisions
#' # in a single call
#' # See diffs for the last two revisions of the Main Page
#' revisions <- wiki_action_request() %>%
#'   query_by_title("Main Page") %>%
#'   query_page_properties(
#'     "revisions",
#'     rvlimit = 2, rvprop = "ids", rvdir = "older"
#'   ) %>%
#'   next_result() %>%
#'   tidyr::unnest(cols = c(revisions)) %>%
#'   dplyr::mutate(diffs = get_diff(from = parentid, to = revid))
#' revisions
get_diff <- function(from, to, language = "en", simplify = TRUE) {
  if (!rlang::is_scalar_logical(simplify)) {
    rlang::abort("`simplify` must be either TRUE or FALSE")
  }
  response_type <- if (simplify) "wikidiff2" else NULL
  get_rest_resource(
    "revision", from, "compare", to,
    language = language, response_type = response_type)
}

diff_to_tbl <- function(diff_list) {
  purrr::map(diff_list, simplify_diff) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(type != 0)
}

simplify_diff <- function(diff) {
  diff <- purrr::modify_at(diff, "highlightRanges", dplyr::bind_rows)
  diff <- purrr::list_flatten(diff)
}

#' @export
#' @describeIn parse_response Simplify a wikidiff2 response to a dataframe of
#'  textual differences, discarding display data
parse_response.wikidiff2 <- function(response) {
  diff_list <- purrr::map(response, "diff")
  diffs <- purrr::map(diff_list, diff_to_tbl)
  diffs
}
