test_that("`wikkitidy_example()` can find valid path to fatwiki index", {
  testthat::expect_no_error(wikkitidy_example("fatwiki_index"))
})

test_that("`wikkitidy_example()` returns a character vector of length 1 when called with valid arg", {
  testthat::expect_length(wikkitidy_example("fatwiki_dump"), 1)
})

test_that("`wikkitidy_example()` returns vector of the same length as .fn_map when no args", {
  testthat::expect_length(wikkitidy_example(), length(.fn_map))
})
