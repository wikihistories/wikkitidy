#' Combine new results for a query with previously downloaded results
#'
#' @param old The [query_tbl] of previous results
#' @param new The [query_tbl] of new results from the server
#'
#' @return A new [query_tbl] of the appropriate subclass, depending on whether
#'  the batch is complete.
#'
#' @keywords internal
append_query_result <- function(old, new) {
  UseMethod("append_query_result")
}

#' @export
append_query_result.complete <- function(old, new) {
  new_query_tbl(
    dplyr::bind_rows(old, new),
    request = get_request(old),
    continue = get_continue(new),
    batchcomplete = get_batchcomplete(new),
    class = query_tbl_subclass(new)
  )
}

#' @export
append_query_result.incomplete <- function(old, new) {
  new_query_tbl(
    merge_tbl_cols(old, new),
    request = get_request(old),
    continue = get_continue(new),
    batchcomplete = get_batchcomplete(new),
    class = query_tbl_subclass(new)
  )
}

#' @export
append_query_result.final <- function(old, new) {
  rlang::abort(
    "Attempting to append new results to a final query. There shouldn't be new results!",
    old = old,
    new = new
  )
}

#' @export
append_query_result.query_tbl <- function(old, new) {
  if (nrow(old) > 0) {
    rlang::abort(
      glue::glue(
      "`append_query_result.query_tbl` called on non-empty query_tbl.",
      "This method should only be called on the initial condition of the ",
      "`continue_query` final loop."
      )
    )
  }
  new
}

merge_tbl_cols <- function(old, new) {
  cols_to_merge <- intersect(list_cols(old), list_cols(new))
  new_cols <- purrr::map(cols_to_merge, \(col) merge_col(col, old, new))
  names(new_cols) <- cols_to_merge
  dplyr::mutate(old, !!!new_cols)
}

merge_col <- function(col, old, new) {
  start <- nrow(old) - nrow(new) + 1
  end <- nrow(old)
  bounds <- start:end
  old[[col]][bounds] <- purrr::map2(old[[col]][bounds], new[[col]], dplyr::bind_rows)
  old[[col]]
}

list_cols <- function(tbl) {
  tbl |>
    purrr::map(rlang::is_list) |>
    purrr::keep(rlang::is_true) |>
    names()
}
