#' A page language object as returned from the [Core REST
#' API](https://www.mediawiki.org/wiki/API:REST_API/Reference#Page_language_object)
#'
#' @param code Two-letter language code
#' @param name The name of the language
#' @param key The URL-friendly title of the page
#' @param title The human-readable title of the page
#'
#' @return A vector of page_language_objects.
#' @export
#'
#' @keywords data_type
#'
#' @examples
#' # This is the return type of [get_page_langlinks]:
#' austen <- get_page_langlinks("Jane Austen")
#' class(austen)
page_language_object <- function(code, name, key, title) {
  new_page_language_object(code, name, key, title)
}

# The constructor
new_page_language_object <- function(code = character(), name = character(), key = character(), title = character()) {
  if (!is.character(code)) {
    rlang::abort("`code` must be a character vector")
  }
  if (!is.character(name)) {
    rlang::abort("`name` must be a character vector")
  }
  if (!is.character(key)) {
    rlang::abort("`key` must be a character vector")
  }
  if (!is.character(title)) {
    rlang::abort("`title` must be a character vector")
  }

  vctrs::new_rcrd(fields = list(code = code, name = name, key = key, title = title), class = "page_language_object")
}

#' @exportS3Method
format.page_language_object <- function(x, ...) {
  code <- vctrs::field(x, "code")
  title <- vctrs::field(x, "title")

  out <- paste0(code, ":", title)
  out[is.na(code) | is.na(title)] <- NA

  out
}

#' @export
vec_ptype_abbr.page_language_object <- function(x, ...) "lng_lnk"

#' @export
vec_ptype_full.page_language_object <- function(x, ...) "page_language_object"
