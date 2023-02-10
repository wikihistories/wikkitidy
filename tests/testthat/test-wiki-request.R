test_that("MediaWiki Action API request receives a 200 response code and json body", {
  resp <- wiki_action_request(list = "categorymembers", cmtitle = "Category:Australian_historians")
  expect_equal(httr2::resp_status(resp), 200)
  expect_equal(httr2::resp_content_type(resp), "application/json")
})
