test_that("`get_page_properties` retrieves data for a known query", {
  page_ids <- wiki_action_request() %>%
    query_list_pages("categorymembers", cmtitle="Category:ATP_Cup") %>%
    retrieve_all()
  page_info <- get_page_properties(page_ids$pageid, by="pageid", "info")
  expect_equal(nrow(page_ids), nrow(page_info))
  expect_equal(page_ids$pageid, page_info$pageid)
})
