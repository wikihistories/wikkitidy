#' A data frame representing the differences between two revisions, following
#' the [Wikidiff2 schema](https://www.mediawiki.org/wiki/Wikidiff2#JSON)
#'
#' @param x The data. A list of diffs to be formatted as a tbl
#' @param request The httr2_request used to retrieve the data
#'
#' @return A data.frame of class [wikidiff_tbl], inheriting from [wiki_tbl]
#'
#' @keywords data_type
#'
#' @examples
#' #TODO
wikidiff_tbl <- function(x, request) {
  new_wiki_tbl(
    x, request = request, continue = NA, batchcomplete = NA, class = "wikidiff_tbl"
  )
}
