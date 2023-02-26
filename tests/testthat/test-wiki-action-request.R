test_that("MediaWiki Action API request receives a 200 response code and json body", {
  resp <- wiki_action_request(list = "categorymembers", cmtitle = "Category:Australian_historians") %>% httr2::req_perform()
  expect_equal(httr2::resp_status(resp), 200)
  expect_equal(httr2::resp_content_type(resp), "application/json")
})

test_that("query_list_of generates a valid query string with data in the query attribute", {
  resp <- wiki_action_request() %>%
    query_list_of("usercontribs", ucuser="Michaelgfalk") %>%
    httr2::req_perform()
  expect_equal(httr2::resp_status(resp), 200)
  data <- httr2::resp_body_json(resp)
  expect_gt(length(data$query$usercontribs), 1)
})
