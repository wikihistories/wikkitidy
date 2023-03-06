#' Get first batch of results for a MediaWiki Action API request
#'
#' @param .req A httr2_request object describing the API query
#'
#' @return A wiki_tbl: an S3 dataframe that is a subclass of tibble::tibble
#' @export
#'
#' @examples
#' # List the first batch of categories that Albert Einstein belongs to
#' response <- wiki_action_request(
#'   prop = "categories",
#'   titles = "Albert Einstein"
#'   ) %>%
#'   perform_query_once()
perform_query_once <- function(.req) {
  resp <- httr2::req_perform(.req)
  body <- httr2::resp_body_json(resp)
  continue <- purrr::pluck(body, "continue")
  batchcomplete <- purrr::pluck(body, "batchcomplete")
  results <- .get_query_results(resp$url, body)
  results %>%
    dplyr::bind_rows() %>%
    as_wiki_tbl(request = .req,
                continue = continue,
                batchcomplete = batchcomplete)
}

# Get a flat, unnamed list of results for dplyr::bind_rows()
# The results are in the `query` key of the response body
# For 'generator' or 'prop' calls, the results are in `pages`.
# For 'list' calls, the results are stored under the name of the list
.get_query_results <- function(url, body) {
  params <- httr2::url_parse(url) %>% purrr::pluck("query")
  if ("prop" %in% names(params) | "generator" %in% names(params)) {
    purrr::pluck(body, "query", "pages")
  } else if ("list" %in% names(params)) {
    list_types = stringr::str_split_1(params$list, "\\|")
    purrr::map(list_types, \(x) purrr::pluck(body, "query", x))
  }
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
#' # Create a request object, then repeatedly retrieve all the pages
#' # NB: the 'uclimit' parameter sets the number of pages retrieved by each
#' # API call.
#' jimbos_contributions <- wiki_action_request() %>%
#'   query_list_of("usercontribs", ucuser="Jimbo_Wales", uclimit=500) %>%
#'   retrieve_all()
#' # To confirm, the resulting tibble has more than 500 rows
#' nrow(jimbos_contributions)
#'
#' # Alternatively, you can try out a request using perform_query_once(), and
#' # if you are happy with the resulting data, retrieve the rest of the results
#' # from the returned data frame
#' preview <- wiki_action_request() %>%
#'   query_page_properties("categories", titles="Albert Einstein") %>%
#'   perform_query_once()
#' print(preview)
#' all_results <- retrieve_all(preview)
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
#' @param .req A httr2_request object describing the API query
#' @param data Data returned from a previous iteration of the query, if any
#' @param continue [Continue](https://www.mediawiki.org/wiki/API:Continue)
#'   parameter returned from previous iteration of the query, if any
#'
#' @return A wiki_tbl: an S3 dataframe that is a subclass of tibble::tibble
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
