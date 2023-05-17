#' Find out more about a page or vector of pages
#'
#' This function will make one API call per page. Beware of requesting
#' data about many pages!
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
#' # List all members of a category, then retrieve page information
#' page_ids <- wiki_action_request() %>%
#' query_list_pages("categorymembers", cmtitle="Category:ATP_Cup") %>%
#'   retrieve_all()
#' page_info <- get_page_properties(page_ids$pageid, by="pageid", "info")
get_page_properties <- function(.x, by=c("pageid","title","revid"), properties, ...) {
  by <- rlang::arg_match(by)
  by <- paste0(by, "s")
  purrr::map(.x, \(x) one_page_properties(x, !!by, properties, ...)) %>%
    dplyr::bind_rows()
}

one_page_properties <- function(id, id_type, properties, ...) {
  id_type <- rlang::ensym(id_type)
  wiki_action_request() %>%
    query_page_properties(properties, !!id_type := id, ...) %>%
    retrieve_all()
}
