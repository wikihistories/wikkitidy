test_that("constructor adds class to responses and conditions", {
  resp <- new_wikkitidy_failure(httr2::response(status_code = 404))
  expect_s3_class(resp, c("wikkitidy_failure", "httr2_response"))

  cnd <- new_wikkitidy_failure(simpleError("boom"))
  expect_s3_class(cnd, c("wikkitidy_failure", "simpleError", "condition"))
})

test_that("constructor rejects other objects", {
  expect_error(new_wikkitidy_failure(1))
  expect_error(new_wikkitidy_failure(tibble::tibble()))
})

test_that("is_wikkitidy_failure identifies failures", {
  expect_true(is_wikkitidy_failure(new_wikkitidy_failure(simpleError("boom"))))
  expect_false(is_wikkitidy_failure(httr2::response(status_code = 404)))
  expect_false(is_wikkitidy_failure(NULL))
})

test_that("print shows alert then underlying object, returns invisibly", {
  resp <- new_wikkitidy_failure(httr2::response(status_code = 404))
  expect_snapshot(print(resp))
  expect_invisible(print(resp))

  cnd <- new_wikkitidy_failure(simpleError("boom"))
  expect_snapshot(print(cnd))
})
