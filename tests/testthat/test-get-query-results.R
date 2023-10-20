test_that("`next_batch` returns a tibble of results", {
  skip_on_cran()
  resp <- wiki_action_request() %>%
    query_generate_pages("categorymembers", gcmtitle = "Category:Physics") %>%
    next_batch()
  expect_no_error(validate_query_tbl(resp))
  expect_gt(nrow(resp), 1)
})

test_that("`retrieve_all` continues the query from a httr2_request", {
  skip_on_cran()
  limit <- 10
  resp <- wiki_action_request() %>%
    query_list_pages("usercontribs", ucuser="michaelgfalk", uclimit=limit) %>%
    retrieve_all()
  expect_no_error(validate_query_tbl(resp))
  expect_gt(nrow(resp), limit)
})

test_that("`retrieve_all` completes the query from a query_tbl", {
  skip_on_cran()
  limit <- 10
  once <- wiki_action_request() %>%
    query_list_pages("usercontribs", ucuser="michaelgfalk", uclimit=limit) %>%
    next_batch()
  complete <- retrieve_all(once)
  expect_equal(complete$revid[1:limit], once$revid[1:limit])
  expect_gt(nrow(complete), nrow(once))
})
