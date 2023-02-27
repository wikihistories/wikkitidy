test_that("`wikimedia_rest_request` returns a 200 for a basic request", {
  response <- wikimedia_rest_request() %>%
    httr2::req_url_path_append("page", "html", "Charles%20Harpur") %>%
    httr2::req_perform()
  expect_equal(httr2::resp_status(response), 200)
})

test_that("`core_rest_request` returns a 200 for a basic request", {
  response <- core_rest_request() %>%
    httr2::req_url_path_append("page", "Charles%20Harpur", "html") %>%
    httr2::req_perform()
  expect_equal(httr2::resp_status(response), 200)
})
