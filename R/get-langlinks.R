#' Find out which other-language wikis have linked pages
#'
#' If the function fails to find any interwiki links for the passed title, it
#' will fail silently.
#'
#' @param titles A character vector of page titles
#' @param language A character vector of two-letter language codes. If it has
#'   a length of 1, it will be recycled.
#'
#' @return A list of [page_language_object] vectors.
#' @export
#'
#' @examples
#' # Get language links for a single page on English Wikipedia
#' get_langlinks("Charles_Harpur")
#'
#' # Get a tbl of pages from French Wikipedia using the Action API, then
#' # find their interwiki links
#' albums_des_beatles <- wiki_action_request(language = "fr") %>%
#'   query_generate_pages(
#'     "categorymembers",
#'     gcmtitle = "Catégorie:Album_des_Beatles",
#'     gcmtype = "page"
#'     ) %>%
#'   query_page_properties("title") %>%
#'   retrieve_all() %>%
#'   dplyr::mutate(links = get_langlinks(title, language = "fr"))
#' albums_des_beatles
#'
#' # Sort pages by number of links
#' albums_des_beatles %>%
#'   dplyr::mutate(count = purrr::map(links, length)) %>%
#'   dplyr::arrange(links, desc = TRUE)
get_langlinks <- function(titles, language = "en") {
  titles <- str_for_rest(titles)
  params <- vctrs::vec_recycle_common(titles, language)
  langlinks <-
    purrr::pmap(
      params,
      \(title, lang) .get_one_resource("page", title, "links", "language", language = lang)
    ) %>%
    purrr::map(purrr::list_transpose) %>%
    purrr::map(
      \(x)
      rlang::inject(new_page_language_object(!!!x))
    )
  langlinks
}

# resp_body <- purrr::list_transpose(resp_body[[1]])
# rlang::inject(new_page_language_object(!!!resp_body))

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

#' A page in a given language edition of Wikipedia
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
#' # This is the return type of [get_langlinks]:
#' austen <- get_langlinks("Jane Austen")
#' class(austen)
page_language_object <- function(code, name, key, title) {
  new_page_language_object(code, name, key, title)
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
