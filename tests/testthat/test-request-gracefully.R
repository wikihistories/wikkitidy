test_that("A bad request fails gracefully", {
  skip_if_not_installed("webfakes")
  expect_no_error(
    httr2::request(httr2::example_url()) |>
      httr2::req_url_path("/status/404") |>
      gracefully(httr2::req_perform)
  )
})

test_that("A good request succeeds silently", {
  skip_if_not_installed("webfakes")
  expect_equal(
    httr2::request(httr2::example_url()) |>
      gracefully(httr2::req_perform) |>
      httr2::resp_status(),
    200
  )
})
