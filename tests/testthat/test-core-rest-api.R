test_that("`core_rest_request` returns a 200 for a basic request", {
  response <- core_rest_request() %>%
    httr2::req_url_path_append("page", "Charles_Harpur", "html") %>%
    httr2::req_perform()
  expect_equal(httr2::resp_status(response), 200)
})
