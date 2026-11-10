test_that("A bad request fails gracefully", {
  skip_if_not_installed("webfakes")
  expect_no_error(
    httr2::request(httr2::example_url()) |>
      httr2::req_url_path("/status/404") |>
      gracefully(httr2::req_perform)
  )
})

test_that("an HTTP error returns a wikkitidy_failure wrapping the response", {
  skip_if_not_installed("webfakes")
  failure <- httr2::request(httr2::example_url()) |>
    httr2::req_url_path("/status/404") |>
    gracefully(httr2::req_perform)
  expect_true(is_wikkitidy_failure(failure))
  expect_s3_class(failure, "httr2_response")
  expect_equal(httr2::resp_status(failure), 404)
})

test_that("a connection failure returns a wikkitidy_failure wrapping the condition", {
  failure <- httr2::request("http://127.0.0.1:1") |>
    httr2::req_retry(max_tries = 1) |>
    gracefully(httr2::req_perform)
  expect_true(is_wikkitidy_failure(failure))
  expect_s3_class(failure, "httr2_failure")
})

test_that("a failure short-circuits without calling the method again", {
  failure <- new_wikkitidy_failure(simpleError("boom"))
  called <- FALSE
  result <- gracefully(failure, \(x) {
    called <<- TRUE
    x
  })
  expect_identical(result, failure)
  expect_false(called)
})

test_that("successful results pass through unchanged", {
  expect_equal(gracefully(1, \(x) x + 1), 2)
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
