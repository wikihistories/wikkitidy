#' Get resources from one of Wikipedia's [two REST
#' APIs](https://www.mediawiki.org/wiki/API)
#'
#' This function is intended for developer use. It makes it easy to quickly
#' generate vectorised calls to the different APIs.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> The URL components and query
#'   parameters of the desired resources. Names of the arguments are ignored.
#'   The function follows the [tidyverse vector recycling
#'   rules](https://vctrs.r-lib.org/reference/vector_recycling_rules), so all
#'   vectors must have the same length or be of length one. Unnamed arguments
#'   will be appended to the URL path; named arguments will be added as query
#'   parameters
#' @param language Character vector of two-letter language codes
#' @param api The desired REST api:
#'   "[core](https://www.mediawiki.org/wiki/API:REST_API)",
#'   "[wikimedia](https://www.mediawiki.org/wiki/Wikimedia_REST_API)",
#'   "[wikimedia_org](https://wikimedia.org/api/rest_v1/)", or
#'   "[xtools](https://www.mediawiki.org/wiki/XTools/API)"
#' @param response_format The expected Content-Type of the response. Currently "html" and
#'   "json" are supported.
#' @param response_type The schema of the response. If supplied, the results will
#'   be parsed using the schema.
#' @param failure_mode How to respond if a request fails
#'   "error", the default: raise an error
#'   "quiet", silently return NA
#'
#' @return A list of responses. If `response_format` == "json", then the responses
#'   will be simple R lists. If `response_format` == "html", then the responses
#'   will `xml_document` objects. If `response_type` is supplied, the response
#'   will be coerced into a [tibble::tbl_df] or vector using the relevant schema.
#'   If the response is a 'scalar list' (i.e. a list of length == 1), then it is
#'   silently unlisted, returning a simple list or vector.
get_rest_resource <- function(
    ..., language = "en",
    api = c("core", "wikimedia", "wikimedia_org", "xtools"),
    response_format = c("json", "html"),
    response_type = NULL,
    failure_mode = c("error", "quiet")) {
  dots <- rlang::list2(...) %>%
    purrr::keep(\(x) !is.null(x)) %>%
    purrr::map_if(is.character, str_for_rest)
  pipeline <- list()
  api <- rlang::arg_match(api)
  pipeline$req_fn <- switch(api,
    "core" = core_rest_request,
    "wikimedia" = wikimedia_rest_request,
    "wikimedia_org" = wikimedia_org_rest_request,
    "xtools" = xtools_rest_request
  )
  failure_mode <- rlang::arg_match(failure_mode)
  pipeline$error_fn <- switch(failure_mode,
    "error" = NULL,
    "quiet" = \(req) httr2::req_error(req, is_error = \(x) FALSE)
  )
  pipeline$perform_fn <- httr2::req_perform
  response_format <- rlang::arg_match(response_format)
  pipeline$resp_fn <- new_response_function(response_format, failure_mode)
  if (!xor(is.null(response_type), rlang::is_scalar_character(response_type))) {
    rlang::abort("`response_type` must be NULL or length 1")
  }
  params <- vctrs::vec_recycle_common(!!!dots, language = language)
  get_one <- purrr::compose(!!!pipeline, .dir = "forward")
  response <- purrr::pmap(params, get_one, .progress = T)
  if (!is.null(response_type)) {
    class(response) <- c(response_type, class(response))
    response <- parse_response(response)
  }
  response <- if (rlang::is_scalar_list(response)) response[[1]] else response
  response
}

new_response_function <- function(response_format, failure_mode) {
  handler <- switch(
    response_format,
    "html" = httr2::resp_body_html,
    "json" = httr2::resp_body_json
  )
  switch(
    failure_mode,
    "error" = handler,
    "quiet" = handle_without_error(handler)
    )
}

handle_without_error <- function(handler) {
  function(resp) {
    if (httr2::resp_is_error(resp)) {
      list()
    } else {
      handler(resp)
    }
  }
}
