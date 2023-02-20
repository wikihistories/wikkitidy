#' Get first batch of results for a MediaWiki Action API request
#'
#' @param .req
#'
#' @return A wiki_tbl: an S3 dataframe that is a subclass of tibble::tibble
#' @export
#'
#' @examples
perform_query_once <- function(.req) {
  resp <- httr2::req_perform(.req) %>%
    httr2::resp_body_json()
  continue <- purrr::pluck(resp, "continue")
  batchcomplete <- purrr::pluck(resp, "batchcomplete")
  resp %>%
    .$query %>%
    unname() %>%
    dplyr::bind_rows() %>%
    as_wiki_tbl(request = .req,
                continue = continue,
                batchcomplete = batchcomplete)
}

#' Continue requesting data from the MediaWiki Action API until there is none
#' left
#'
#' Typically a request to the [MediaWiki Action
#' API](https://www.mediawiki.org/wiki/API:Main_page) will only return a limited
#' number of results. The maximum results vary by query, but typically is set at
#' 500. If there are more results on the server, the API will return a [continue
#' object](https://www.mediawiki.org/wiki/API:Continue) allowing you to request
#' further batches of data. This function handles the continue object and
#' retrieves all the data for the desired query.
#'
#' @param x An object describing the desired request. Either a wiki_tbl or a
#'   httr2_request
#'
#' @return A wiki_tbl: an S3 dataframe that is a subclass of tibble::tibble
#' @export
#'
#' @examples
retrieve_all <- function(x) {
  UseMethod("retrieve_all")
}

#' @describeIn retrieve_all Get all available data from a constructed request
#' @export
retrieve_all.httr2_request <- function(x) {
  continue_query(.req = x, data = NULL)
}

#' @describeIn retrieve_all Complete a query from an incomplete wiki_tbl
#' @export
retrieve_all.wiki_tbl <- function(x) {
  request = get_request(x)
  continue = get_continue(x)
  continue_query(.req = request,
                 data = x,
                 continue = continue)
}

#' Inner loop of `retrieve_all`
#'
#' This function drives `retrieve_all`, by repeatedly appending the continue
#' parameters returned by the MediaWiki Action API to the request and retrieving
#' the next batch.
#'
#' @param .req
#' @param data
#' @param continue
#'
#' @return A wiki_tbl: an S3 dataframe that is a subclass of tibble::tibble
#'
#' @examples
continue_query <- function(.req,
                           data = NULL,
                           continue = NULL) {
  base_request <- .req
  response_list <- vector("list", 100)
  if (!is.null(data)) {
    response_list[[1]] <- data
  }
  if (!is.list(continue)) {continue <- TRUE}
  while (incomplete(continue)) {
    new_request <- append_continue(base_request, continue)
    next_idx <- purrr::detect_index(response_list, is.null)
    if (next_idx == 0) {
      next_idx <- length(response_list) + 1
      response_list <- c(response_list, vector("list", 100))
    }
    response_list[[next_idx]] <- perform_query_once(new_request)
    continue <- get_continue(response_list[[next_idx]])
    batchcomplete <- get_batchcomplete(response_list[[next_idx]])
  }
  dplyr::bind_rows(response_list) %>% as_wiki_tbl(request = base_request,
                                                  continue = continue,
                                                  batchcomplete = batchcomplete)
}

incomplete <- function(continue) {
  !is.null(continue) && !anyNA(continue)
}

append_continue <- function(.req, continue) {
  if (!is.null(names(continue))) {
    httr2::req_url_query(.req, !!!continue)
  } else {
    .req
  }
}
