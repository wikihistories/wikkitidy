# Collect pages from a Wikimedia XML dump
#
# Use this function to draw pages from an XML file,
#
# @param page_generator A [coro::generator] that yields Wikipedia pages, as
#   returned by [xml_generate_pages()] or [xml_filter_pages()]
#
# @return A [tibble::tbl_df] with five columns: `title`, `ns`, `pageid`,
#   `revisionid` and `text`
#
# @keywords internal
xml_collect_pages <- function(page_generator) {
  collected_pages <- coro::collect(page_generator)
  return(dplyr::bind_rows(collected_pages))
}
