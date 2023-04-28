test_that("`perform_query_once` returns a tibble of results", {
  resp <- wiki_action_request() %>%
    query_generate_pages("categorymembers", gcmtitle = "Category:Physics") %>%
    perform_query_once()
  expect_no_error(validate_query_tbl(resp))
  expect_gt(nrow(resp), 1)
})

test_that("`retrieve_all` continues the query from a httr2_request", {
  limit <- 10
  resp <- wiki_action_request() %>%
    query_list_of("usercontribs", ucuser="michaelgfalk", uclimit=limit) %>%
    retrieve_all()
  expect_no_error(validate_query_tbl(resp))
  expect_gt(nrow(resp), limit)
})

test_that("`retrieve_all` completes the query from a query_tbl", {
  limit <- 10
  once <- wiki_action_request() %>%
    query_list_of("usercontribs", ucuser="michaelgfalk", uclimit=limit) %>%
    perform_query_once()
  complete <- retrieve_all(once)
  expect_equal(complete$revid[1:limit], once$revid[1:limit])
  expect_gt(nrow(complete), nrow(once))
})
