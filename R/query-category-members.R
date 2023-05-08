#' Build a query to enumerate the members of a category in Wikipedia
#'
#' This function provides access to the
#' [CategoryMembers](https://www.mediawiki.org/wiki/API:Categorymembers)
#' endpoint of the Action API. Categories in Wikipedia contain two main kinds of
#' 'members': subcategories and pages.
#'
#' @param category The category to start from. Can be named by title or pageid.
#' @param namespace Only return category members from the provided namespace
#' @param type Alternative to `namespace`: the type of category member to return
#' @param limit The number to return each batch. Max 500.
#' @param sort How to sort the returned category members. 'timestamp' sorts them
#'   by the date they were included in the category; 'sortkey' by the category
#'   member's unique hexadecimal code
#' @param dir The direction in which to sort them
#' @param start If `sort` == 'timestamp', only return category members from
#'   after this date. The argument is parsed by [lubridate::as_date()]
#' @param end If `sort` == 'timestamp', only return category members included in
#'   the category from before this date. The argument is parsed by
#'   [lubridate::as_date()]
#' @param language The language edition of Wikipedia to query
#'
#' @return A request object of type `generator/query/action_api/httr2_request`,
#'   which can be passed to [next_batch()] or [retrieve_all()]. You can specify
#'   which properties to retrieve for each page using [query_page_properties()].
#' @export
#'
#' @examples
#' # Get the first 10 pages in 'Category:Physics' on English Wikipedia
#' physics_members <- query_category_members("Physics") %>% next_batch()
#' physics_members
#'
#' # Retrieve interwiki links for the first 10 pages in the 'Beatles Albums'
#' # category on Danish Wikipedia
#' # TODO: This currently doesn't work, because incomplete batches aren't properly handled
#' beatles <- query_category_members("Beatles-album", language = "da") %>%
#'   query_page_properties("langlinks") %>%
#'   next_batch()
query_category_members <- function(
  category,
  namespace = NULL,
  type = c("file", "page", "subcat"),
  limit = 10,
  sort = c("sortkey", "timestamp"),
  dir = c("ascending", "descending", "newer", "older"),
  start = NULL,
  end = NULL,
  language = "en"
) {
  category <- id_or_title(category, prefix = "Category")
  namespace <- check_namespace(namespace)
  type <- rlang::arg_match(type, multiple = T) %>%
    paste0(collapse = "|")
  limit <- check_limit(limit, max = 500)
  sort <- rlang::arg_match(sort)
  dir <- rlang::arg_match(dir)
  if (!is.null(start) || !is.null(end)) {
    if (!sort == "timestamp") {
      rlang::abort("If using `start` or `end`, you must use sort = 'timestamp'",
                   class = "incompatible_arguments")
    }
  }
  timestamp_args <- process_timestamps(start, end)
  query_params <- rlang::dots_list(
    !!!category,
    namespace,
    type,
    limit,
    sort,
    dir,
    !!!timestamp_args,
    .named = T
  )
  names(query_params) <- stringr::str_c("gcm", names(query_params))
  wiki_action_request(language = language) %>%
    query_generate_pages("categorymembers", !!!query_params)
}
