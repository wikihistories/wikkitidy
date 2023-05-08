wikkitidy_user_agent <- function(.req) {
  httr2::req_user_agent(.req, "wikkitidy R package (https://github.com/wikihistories/wikkitidy)")
}

str_for_rest <- function(titles) {
  stringr::str_replace_all(titles, " ", "_")
}

#' Determine if a page parameter comprises titles or pageids, and prefix
#' accordingly.
#'
#' @param page Either a character or numeric vector. If a character vector, it
#'   is interpreted as a vector of page titles. If a numeric vector, of pageids.
#' @param prefix Optional: A prefix to affix to the page titles if it is missing
#'
#' @return A list
#' @keywords internal
id_or_title <- function(page, prefix = NULL) {
  UseMethod("id_or_title")
}

#' @rdname id_or_title
#' @export
id_or_title.character <- function(page, prefix = NULL) {
  numeric_page <- suppressWarnings(as.numeric(page))
  if (anyNA(numeric_page)) {
    prefixed <- add_missing_prefix(page, prefix = prefix)
    list("title" = prefixed)
  } else {
    list("pageid" = numeric_page)
  }
}

#' @rdname id_or_title
#' @export
id_or_title.numeric <- function(page, prefix = NULL) {
  page <- as.integer(page)
  list("pageid" = page)
}

add_missing_prefix <- function(string, prefix = NULL) {
  if (is.null(prefix)) {
    string
  } else {
    pattern <- paste0(prefix, ":")
    unprefixed <- !stringr::str_detect(string, pattern)
    string[unprefixed] <- stringr::str_c(prefix, string[unprefixed], sep = ":")
    string
  }
}

#' Ensure that the limit is correct for the endpoint. Raise an error if not.
#'
#' @param limit The limit to be added to the query
#' @param max The maximum allowed for the given endpoint
#'
#' @return `limit`, assuming no errors
#' @keywords internal
check_limit <- function(limit, max) {
  UseMethod("check_limit")
}

#' @export
check_limit.character <- function(limit, max) {
  if (!rlang::is_scalar_character(limit)) {
    rlang::abort("`limit` must be a scalar (length == 1)", class="non_scalar_arg")
  }
  if (limit == "max") {
    limit
  } else {
    rlang::abort("`limit` must be 'max' or an integer", class="wrong_arg_type")
  }
}

#' @export
check_limit.numeric <- function(limit, max) {
  if (!rlang::is_scalar_integerish(limit)) {
    rlang::abort("`limit` must be 'max' or a scalar integer", class="non_scalar_arg")
  }
  if (!limit < max) {
    rlang::abort("`limit` is greater than the allowable maximum for this endpoint ({max})",
                 class = "exceed_max")
  }
  limit
}

#' Convert passed objects into ISO8601 strings for API requests
#'
#' @param ... Dynamic dots: the objects to be coerced
#'
#' @return A named list of ISO strings, the same length as `...`
#' @keywords internal
process_timestamps <- function(...) {
  dots <- rlang::dots_list(..., .named = T)
  purrr::map(dots, \(x) lubridate::as_date(x) %>% lubridate::format_ISO8601())
}

#' Ensure namespace arguments are valid
#' @param namespace An integer vector of namespace ids, or NULL
#' @return A character vector of namespace, spliced together with a `|`, or NULL
#' @keywords internal
check_namespace <- function(namespace) {
  if (is.null(namespace)) {
    return(NULL)
  }
  if (!rlang::is_integerish(namespace)) {
    rlang::abort("`namespace` must be an integer vector", class="wrong_arg_type")
  }
  paste0(namespace, collapse = "|")
}
