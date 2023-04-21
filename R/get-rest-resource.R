#' Get resources from one of Wikipedia's [two REST
#' APIs](https://www.mediawiki.org/wiki/API)
#'
#' This function is intended for developer use. It makes it easy to quickly
#' generate vectorised calls to the different API endpoints.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> The URL components of the
#'   desired resources. Names of the arguments are ignored. The function follows
#'   the [tidyverse vector recycling
#'   rules](https://vctrs.r-lib.org/reference/vector_recycling_rules), so all
#'   vectors must have the same length or be of length one.
#' @param language Character vector of two-letter language codes
#' @param endpoint The desired REST endpoint, either
#'   "[core](https://www.mediawiki.org/wiki/API:REST_API)" or
#'   "[wikimedia](https://www.mediawiki.org/wiki/Wikimedia_REST_API)"
#' @param response_type The expected Content-Type of the response. Currently
#'  "html" and "json" are supported.
#'
#' @return A list of responses. If `response_type` == "json", then the responses
#'   will be simple R lists. If `response_type` == "html", then the responses
#'   will `xml_document` objects.
#'
get_rest_resource <- function(
    ..., language = "en",
    endpoint = c("core", "wikimedia"),
    response_type = c("json", "html")) {
  dots <- rlang::list2(...)
  dots <- purrr::map_if(dots, is.character, str_for_rest)
  req_fn <- switch(rlang::arg_match(endpoint),
    "core" = core_rest_request,
    "wikimedia" = wikimedia_rest_request
  )
  resp_fn <- switch(rlang::arg_match(response_type),
    "json" = httr2::resp_body_json,
    "html" = httr2::resp_body_html
  )
  params <- vctrs::vec_recycle_common(!!!dots, language = language)
  get_one <- purrr::compose(req_fn, httr2::req_perform, resp_fn, .dir = "forward")
  purrr::pmap(params, get_one)
}
