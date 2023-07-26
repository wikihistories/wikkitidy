#' Perform a query using the [MediaWiki Action
#' API](https://www.mediawiki.org/wiki/Special:MyLanguage/API:Main_page)
#'
#' @description `next_result()` sends exactly one request to the server.
#'
#'   `next_batch()` requests results from the server until data is complete the
#'   latest batch of pages in the result.
#'
#'   `retrieve_all()` keeps requesting data until all the pages from the query
#'   have been returned.
#'
#' @details It is rare that a query can be fulfilled in a single request to the
#'   server. There are two ways a query can be incomplete. All queries return a
#'   list of pages as their result. The result may be incomplete because not all
#'   the data for each page has been returned. In this case the *batch* is
#'   incomplete. Or the data may be complete for all pages, but there are more
#'   pages available on the server. In this case the query can be *continued*.
#'   Thus the three functions for `next_result()`, `next_batch()` and
#'   `retrieve_all()`.
#'
#' @name get_query_results
#'
#' @param x The query. Either a [wiki_action_request] or a [query_tbl].
#'
#' @return A [query_tbl] containing results of the query. If `x` is a
#'   [query_tbl], then the function will return a new data with the new data
#'   appended to it. If `x` is a [wiki_action_request], then the returned
#'   [query_tbl] will contain the necessary data to supply future calls to
#'   `next_result()`, `next_batch()` or `retrieve_all()`.
#'
#' @examples
#' # Try out a request using next_result(), then retrieve the rest of the
#' # results. The clllimt limits the first request to 40 results.
#' preview <- wiki_action_request() %>%
#'   query_by_title("Steve Wozniak") %>%
#'   query_page_properties("categories", cllimit = 40) %>%
#'   next_result()
#' preview
#'
#' all_results <- retrieve_all(preview)
#' all_results
#'
#' # tidyr is useful for list-columns.
#' all_results %>%
#'   tidyr::unnest(cols=c(categories), names_sep = "_")
NULL

#' @rdname get_query_results
#' @export
next_result <- function(x) {
  UseMethod("next_result")
}

#' @export
next_result.query_tbl <- function(x) {
  continue <- get_continue(x)
  request <- get_request(x)
  result_tbl <- perform_query(request, continue)
  append_query_result(old = x, new = result_tbl)
}

#' @export
next_result.query <- function(x) {
  perform_query(x, continue = NULL)
}

#' @rdname get_query_results
#' @export
next_batch <- function(x) {
  UseMethod("next_batch")
}

#' @export
next_batch.query <- function(x) {
  first_result <- perform_query(x, continue = NULL)
  complete_batch <- continue_query(first_result, is_incomplete)
  complete_batch
}

#' @export
next_batch.query_tbl <- function(x) {
  complete_batch <- continue_query(x, is_incomplete)
}

#' @rdname get_query_results
#' @export
retrieve_all <- function(x) {
  UseMethod("retrieve_all")
}

#' @export
retrieve_all.query <- function(x) {
  first_result <- perform_query(x, continue = NULL)
  all_results <- continue_query(first_result, is_not_final)
  all_results
}

#' @export
retrieve_all.query_tbl <- function(x) {
  all_results <- continue_query(x, is_not_final)
}

#' Query the Action API continually until a continuation condition no longer
#' holds.
#'
#' @keywords internal
#'
#' @param last_result The query_tbl of results to complete
#' @param predicate The while condition. Results will be continually
#'  requested until this evaluates 'false'.
#'
#' @return A query_tbl: an S3 dataframe that is a subclass of tibble::tibble
continue_query <- function(last_result, predicate, max_requests = 1000) {
  results_inc <- 100
  results <- vector("list", results_inc)
  max_idx <- results_inc
  next_idx <- 1
  results[[next_idx]] <- last_result

  while (predicate(last_result)) {
    next_idx <- next_idx + 1
    if (next_idx > max_requests) {
      rlang::inform(
        glue::glue("Query halted after {max_requests} requests Continue the query with `next_result`, `next_batch` or `retrieve_all`.")
      )
      break()
    }
    if (next_idx > max_idx) {
      results <- c(results, vector("list", results_inc))
      max_idx <- max_idx + results_inc
    }
    request <- get_request(last_result)
    continue <- get_continue(last_result)
    check_continue(continue)
    last_result <- perform_query(request, continue)
    results[[next_idx]] <- last_result
  }
  results %>%
    purrr::keep(is_not_null) %>%
    purrr::reduce(append_query_result, .init = empty_query_tbl())
}

check_continue <- function(continue) {
  if (
    !rlang::is_list(continue) &&
    length(continue) > 1 &&
    all(stringr::str_detect(names(continue), "continue"))
  ) {
    rlang::abort(
      "Invalid continue parameters",
      class = "no_continue",
      wikkitidy_continue = continue
    )
  }
}

is_not_final <- function(query_tbl) {
  query_tbl_subclass(query_tbl) != "final"
}

is_incomplete <- function(query_tbl) {
  query_tbl_subclass(query_tbl) == "incomplete"
}
