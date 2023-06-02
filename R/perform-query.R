#' Perform a single request to the Action API.
#'
#' This function is the workhorse behind the user-facing [next_result()],
#' [next_batch()] and [retrieve_all()].
#'
#' @seealso [append_query_result()]
#'
#' @param request The request object
#' @param continue The continue parameter returned by the previous request
#'
#' @return A [query_tbl()] of the results
#' @keywords internal
perform_query <- function(request, continue) {
  UseMethod("perform_query")
}

#' @export
perform_query.prop <- function(request, continue) {
  result <- get_result(request, continue, c("query", "pages"))
  result$x <- result$x %>%
    purrr::list_transpose() %>%
    tibble::tibble(!!!.)
  result_to_query_tbl(result)
}

#' @export
perform_query.list <- function(request, continue) {
  result <- get_result(request, continue, c("query"))
  # If more than one list module has been queried, preserve the name of the
  # module. Otherwise drop it.
  if (length(result$x) > 1) {
    result$x <- purrr::list_flatten(result$data, name_spec = "{outer}") %>%
      dplyr::bind_rows(.id = "list_module")
  } else {
    result$x <- result$x[[1]] %>% dplyr::bind_rows()
  }
  result_to_query_tbl(result)
}

#' @export
perform_query.generator <- function(request, continue) {
  result <- get_result(request, continue, c("query", "pages"))
  result$x <- result$x %>%
    purrr::list_transpose() %>%
    tibble::tibble(!!!.)
  result_to_query_tbl(result)
}

get_result <- function(request, continue, pluck_params) {
  resp <- request %>% httr2::req_url_query(!!!continue) %>% httr2::req_perform()
  body <- httr2::resp_body_json(resp)
  x <- purrr::pluck(body, !!!pluck_params)
  new_continue <- purrr::pluck(body, "continue", .default = NA)
  batchcomplete <- purrr::pluck(body, "batchcomplete", .default = FALSE)
  class <- infer_result_type(new_continue, batchcomplete)
  rlang::dots_list(x, request, continue = new_continue, batchcomplete, class, .named = TRUE)
}

infer_result_type <- function(continue, batchcomplete) {
  if (rlang::is_na(continue)) {
    "final"
  } else if (rlang::is_false(batchcomplete)) {
    "incomplete"
  } else {
    "complete"
  }
}

result_to_query_tbl <- function(result) {
  result$x <- dplyr::mutate(
    result$x,
    dplyr::across(
      dplyr::where(rlang::is_list),
      \(col) purrr::map(col, dplyr::bind_rows)
  ))
  rlang::inject(new_query_tbl(!!!result))
}
