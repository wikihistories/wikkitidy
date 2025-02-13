# Keep pages from an XML dump that match a condition
#
# Wikimedia XML dumps are very large, and can easily exceed your computer's
# memory. This function allows you to filter an XML file, to only keep the text
# of pages that match a certain condition.
#
# @param page_generator The stream of pages from an XML dump, as returned by
#   [xml_iterate_pages()]
# @param title A function that takes a page title, and returns a logical value
# @param ns A function that takes a namespace number, and returns a logical
#   value
# @param pageid A function that takes a pageid, and returns a logical value
# @param text A function that takes wikitext as input, and returns a logical
#   value
#
# @return A [coro::generator], which will only yield pages matching the passed
#   conditions. Use [xml_collect_pages()] to pull the pages into a
#   [tibble::tbl_df].
#
# @examples
# # Get the text of all pages that contain the string "native_name"
# pages <- xml_iterate_pages(sample_xml) |>
#   xml_filter_pages(text = \(text) grepl('native_name', text, fixed = TRUE)) |>
#   xml_collect_pages()
#
# @keywords internal
xml_filter_pages <- function(
    page_generator,
    title = NULL,
    ns = NULL,
    id = NULL,
    text = NULL
    ) {
  filters <- list(title = title, ns = ns, id = id, text = text)

  if (is.null(purrr::detect(filters, rlang::is_function))) {
    rlang::abort("You must provide at least one predicate function.")
  }

  predicate <- combine_filters(filters)

  coro::gen(for (page in page_generator) if (predicate(page)) coro::yield(page))
}

# This is totally inelegant. TODO: Make it nicer!
combine_filters <- function(filters) {
  fil <- purrr::map(filters, \(.f) .f %||% \(x) TRUE)
  function(page) {
    fil$title(page$title) & fil$ns(page$ns) & fil$id(page$id) & fil$text(page$text)
  }
}

