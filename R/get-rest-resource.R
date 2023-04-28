#' Get resources from one of Wikipedia's [two REST
#' APIs](https://www.mediawiki.org/wiki/API)
#'
#' This function is intended for developer use. It makes it easy to quickly
#' generate vectorised calls to the different API endpoints.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> The URL components and query
#'   parameters of the desired resources. Names of the arguments are ignored.
#'   The function follows the [tidyverse vector recycling
#'   rules](https://vctrs.r-lib.org/reference/vector_recycling_rules), so all
#'   vectors must have the same length or be of length one. Unnamed arguments
#'   will be appended to the URL path; named arguments will be added as query
#'   parameters
#' @param language Character vector of two-letter language codes
#' @param endpoint The desired REST endpoint, either
#'   "[core](https://www.mediawiki.org/wiki/API:REST_API)" or
#'   "[wikimedia](https://www.mediawiki.org/wiki/Wikimedia_REST_API)"
#' @param response_format The expected Content-Type of the response. Currently "html" and
#'   "json" are supported.
#' @param response_type The schema of the response. If supplied, the results will
#'   be parsed using the schema.
#'
#' @return A list of responses. If `response_format` == "json", then the responses
#'   will be simple R lists. If `response_format` == "html", then the responses
#'   will `xml_document` objects. If `response_type` is supplied, the response
#'   will be coerced into a [tibble::tbl_df] or vector using the relevant schema.
#'
get_rest_resource <- function(
    ..., language = "en",
    endpoint = c("core", "wikimedia"),
    response_format = c("json", "html"),
    response_type = NULL) {
  dots <- rlang::list2(...) %>%
    purrr::keep(\(x) !is.null(x)) %>%
    purrr::map_if(is.character, str_for_rest)
  req_fn <- switch(rlang::arg_match(endpoint),
    "core" = core_rest_request,
    "wikimedia" = wikimedia_rest_request
  )
  resp_fn <- switch(rlang::arg_match(response_format),
    "json" = httr2::resp_body_json,
    "html" = httr2::resp_body_html
  )
  if (!xor(is.null(response_type), rlang::is_scalar_character(response_type))) {
    rlang::abort("`response_type` must be NULL or length 1")
  }
  params <- vctrs::vec_recycle_common(!!!dots, language = language)
  get_one <- purrr::compose(req_fn, httr2::req_perform, resp_fn, .dir = "forward")
  response <- purrr::pmap(params, get_one)
  if (!is.null(response_type)) {
    class(response) <- response_type
    response <- parse_response(response)
  }
  response
}
