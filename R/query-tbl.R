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
  batchcomplete <- if(is.null(batchcomplete)) NA else batchcomplete
  new_query_tbl(x, request, continue, batchcomplete)
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
    cli::cli_text("{.cls {class(x)[1]}}"),
    cli::cli_text("Request URL: {url}"),
    NextMethod()
  )
}

#' @export
tbl_format_footer.query_tbl <- function(x, ...) {
  # Unfortunately the messages appear above the body of the tbl, rather than
  # underneath it... TODO: Report issue!
  default_footer <- NextMethod()
  query_message <- if (anyNA(get_continue(x))) {
    cli::cli_alert_success("All results downloaded from server")
  } else {
    cli::cli_alert_info("There are more results on the server. Retrieve them with `next_batch()` or `retrieve_all()`")
  }
  batch_message <- if (!is.na(get_batchcomplete(x))) {
    cli::cli_alert_success("Data complete for all records")
  } else {
    cli::cli_alert_warning("Data not fully downloaded for last batch. Retrieve it with `next_batch()` or `retrieve_all()`.")
  }
  default_footer
}

validate_query_tbl <- function(x) {
  if (!tibble::is_tibble(x)) {
    stop("`x` is not a valid tibble",
         .call = FALSE)
  }
  if (!all(c("request", "continue", "batchcomplete") %in% names(attributes(x)))) {
    stop(
      "`x` is missing one or more of 'request', 'continue' or 'batchcomplete' attributes",
      .call = FALSE
    )
  }
  x
}

get_request <- function(query_tbl) {
  attr(query_tbl, "request")
}

get_continue <- function(query_tbl) {
  attr(query_tbl, "continue")
}

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
