test_that("`wikimedia_rest_request` returns a 200 for a basic request", {
  response <- wikimedia_rest_request() %>%
    httr2::req_url_path_append("page", "html", "Charles%20Harpur") %>%
    httr2::req_perform()
  expect_equal(httr2::resp_status(response), 200)
})
