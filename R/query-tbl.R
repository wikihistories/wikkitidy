#' Representation of Wikipedia data returned from an [Action API Query
#' module](https://www.mediawiki.org/wiki/API:Query) as tibble, with request
#' metadata stored as attributes.
#'
#' @param x A tibble
#' @param request The httr2_request object used to generate the tibble
#' @param continue The continue parameter returned by the API
#' @param batchcomplete The batchcomplete parameter returned by the API
#'
#' @return A tibble: an S3 data.frame with class `query_tbl`.
#'
#' @keywords data_type
query_tbl <- function(x, request, continue, batchcomplete) {
  request <- if (is.null(request)) NA else request
  continue <- if(is.null(continue)) NA else continue
  batchcomplete <- if(is.null(batchcomplete)) FALSE else batchcomplete
  new_query_tbl(x, request, continue, batchcomplete)
}

QUERY_TBL_CLASS = c("query_tbl", "tbl_df", "tbl", "data.frame")

query_tbl_subclass <- function(x) {
  setdiff(class(x), QUERY_TBL_CLASS)
}

# The constructor
new_query_tbl <- function(x, request, continue, batchcomplete, class=NULL) {
  tibble::new_tibble(
    x,
    request = request,
    continue = continue,
    batchcomplete = batchcomplete,
    class = c(class, "query_tbl")
  )
}

#' @export
tbl_sum.query_tbl <- function(x, ...) {
  url <- get_request(x)$url
  c(
    cli::cli_text("{.cls {paste0(class(x)[1:2], collapse = '/')}}"),
    NextMethod()
  )
}

#' @export
tbl_format_footer.query_tbl <- function(x, ...) {
  default_footer <- NextMethod()
  query_message <- if (rlang::is_na(get_continue(x))) {
    cli::cli_alert_success("All results downloaded from server")
  } else {
    cli::cli_alert_info("There are more results on the server. Retrieve them with `next_batch()` or `retrieve_all()`")
  }
  batch_message <- if (rlang::is_true(get_batchcomplete(x))) {
    cli::cli_alert_success("Data complete for all records")
  } else {
    cli::cli_alert_warning("Data not fully downloaded for last batch. Retrieve it with `next_batch()` or `retrieve_all()`.")
  }
  default_footer
}

validate_query_tbl <- function(x) {
  tbl_var <- rlang::ensym(x)
  if (!tibble::is_tibble(x)) {
    rlang::abort(
      glue::glue("`{tbl_var}` is not a tibble"),
      class = "invalid"
    )
  }
  continue <- get_continue(x)
  if (
    !(
      rlang::is_na(continue) ||
      (rlang::has_name(continue, "continue") && length(continue) > 1)
    )
  ) {
    rlang::abort(
      glue::glue("`{tbl_var}` lacks a valid `continue` attribute"),
      class = "invalid"
    )
  }
  if (!rlang::is_scalar_logical(get_batchcomplete(x))) {
    rlang::abort(
      glue::glue("`{tbl_var}` lacks a valid `batchcomplete` attribute"),
      class = "invalid"
    )
  }
  if (!is_action_query(get_request(x))) {
    rlang::abort(
      glue::glue("`{tbl_var} lacks a valid `request` attribute"),
      class = "invalid"
    )
  }
  x
}

get_request <- purrr::attr_getter("request")

get_continue <- purrr::attr_getter("continue")

get_batchcomplete <- purrr::attr_getter("batchcomplete")

set_continue <- function(query_tbl, x) {
  x <- if (is.null(x)) NA else x
  attr(query_tbl, "continue") <- x
}

get_batchcomplete <- function(query_tbl) {
  attr(query_tbl, "batchcomplete")
}

set_batchcomplete <- function(query_tbl, x) {
  x <- if (is.null(x)) NA else x
  attr(query_tbl, "batchcomplete") <- x
}

# A placeholder. The returned item should raise an error nearly everywhere.
empty_query_tbl <- function() {
  new_query_tbl(tibble::tibble(), request = NA, continue = NA, batchcomplete = NA)
}
