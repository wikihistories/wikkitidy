#' Representation of Wikipedia data as tibble, with request metadata stored as
#' attributes
#'
#' @param x A tibble
#' @param request The httr2_request object used to generate the tibble
#' @param continue The continue parameter returned by the API
#' @param batchcomplete The batchcomplete parameter returned by the API
#'
#' @return A tibble: an S3 data.frame with class `wiki_tbl`.
as_wiki_tbl <- function(x, request, continue, batchcomplete) {
  request <- if (is.null(request)) NA else request
  continue <- if(is.null(continue)) NA else continue
  batchcomplete <- if(is.null(batchcomplete)) NA else batchcomplete
  tibble::new_tibble(
    x,
    request = request,
    continue = continue,
    batchcomplete = batchcomplete,
    class = "wiki_tbl"
  )
}

validate_wiki_tbl <- function(x) {
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

get_request <- function(wiki_tbl) {
  attr(wiki_tbl, "request")
}

get_continue <- function(wiki_tbl) {
  attr(wiki_tbl, "continue")
}

set_continue <- function(wiki_tbl, x) {
  x <- if (is.null(x)) NA else x
  attr(wiki_tbl, "continue") <- x
}

get_batchcomplete <- function(wiki_tbl) {
  attr(wiki_tbl, "batchcomplete")
}

set_batchcomplete <- function(wiki_tbl, x) {
  x <- if (is.null(x)) NA else x
  attr(wiki_tbl, "batchcomplete") <- x
}
