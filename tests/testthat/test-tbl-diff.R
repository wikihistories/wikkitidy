test_that("wikidiff_tbl returns a valid wiki_tbl of the right subclass", {
  data <- tibble::tibble(x = 1:5)
  request <- httr2::request("example.com")
  test_tbl <- wikidiff_tbl(data, request=request)
  expect_s3_class(test_tbl, c("wikidiff_tbl", "wiki_tbl"))
  expect_no_error(validate_wiki_tbl(test_tbl))
})
