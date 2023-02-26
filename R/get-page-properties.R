#' Find out more about a page or vector of pages
#'
#' This function will make one API call per page. So to beware of requesting
#' data about too many pages.
#'
#' @param .x A vector of pageids, titles or revisionids
#' @param by The type of information contained in .x
#' @param properties The desired properties; can be either a string delimited by
#'  "|" or a character vector
#' @param ... Name-value pairs of additional parameters for the particular
#'  properties requested
#'
#' @return A list of the same length as .x, containing the returned properties.
#' @export
#'
#' @examples
get_page_properties <- function(.x, by=c("pageid","title","revid"), properties, ...) {
  rlang::arg_match(by)
  by <- paste0(by, "s")
  purrr::map(.x, \(x) one_page_properties(x, !!by, properties, ...))
}

one_page_properties <- function(id, id_type, properties, ...) {
  id_type <- rlang::ensym(id_type)
  wiki_action_request() %>%
    query_page_properties(properties, !!id_type := id, ...) %>%
    retrieve_all()
}
