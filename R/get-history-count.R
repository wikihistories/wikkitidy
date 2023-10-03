#' Count how many times Wikipedia articles have been edited
#'
#' @param title A vector of article titles
#' @param type The [type of edit to
#'   count](https://www.mediawiki.org/wiki/API:REST_API/Reference#Parameters_12)
#' @param from Optional: a vector of revision ids
#' @param to Optional: a vector of revision ids
#' @param language Vector of two-letter language codes for Wikipedia editions
#'
#' @return A [tibble::tbl_df] with two columns:
#'  * 'count': integer, the number of edits of the given type
#'  * 'limit': logical, whether the 'count' exceeds the API's limit. Each type of
#'   edit has a different limit. If the 'count' exceeds the limit, then the
#'   limit is returned as the count and 'limit' is set to TRUE
#' @export
#'
#' @examples
#' # Get the number of edits made by auto-confirmed editors to a page between
#' # revisions 384955912 and 406217369
#' get_history_count("Jupiter", "editors", 384955912, 406217369)
#'
#' # Compare which authors have the most edit activity
#' authors <- tibble::tribble(
#'   ~author,
#'   "Jane Austen",
#'   "William Shakespeare",
#'   "Emily Dickinson"
#' ) %>%
#'   dplyr::mutate(get_history_count(author))
#' authors
get_history_count <- function(
    title,
    type = c("edits", "anonymous", "bot", "editors", "minor", "reverted"),
    from = NULL,
    to = NULL,
    language = "en") {
  type <- rlang::arg_match(type)
  if (xor(is.null(from), is.null(to))) {
    rlang::abort("If using `from` and `to`, then both must be supplied")
  }
  if (!is.null(from) && !(type == "edits" || type == "editors")) {
    rlang::abort("If using `from` and `to`, you can only request counts for 'edits' or 'editors'")
  }
  get_rest_resource(
    "page", title, "history", "counts", type, from = from, to = to,
    language = language, response_type = "history_count_object"
  )
}

#' @exportS3Method
parse_response.history_count_object <- function(response) {
  dplyr::bind_rows(!!!response)
}
