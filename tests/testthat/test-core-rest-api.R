test_that("`core_rest_request` returns a 200 for a basic request", {
  response <- core_rest_request("page", "Charles_Harpur", "html") %>%
    httr2::req_perform()
  expect_equal(httr2::resp_status(response), 200)
})

test_that("`core_rest_request` throws an 'Invalid request' error if no URL components are provided", {
  expect_error(core_rest_request(), "Invalid request")
})
