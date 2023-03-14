test_that("`core_rest_request` returns a 200 for a basic request", {
  response <- core_rest_request() %>%
    httr2::req_url_path_append("page", "Charles_Harpur", "html") %>%
    httr2::req_perform()
  expect_equal(httr2::resp_status(response), 200)
})

test_that("`get_diff` throws an error for mismatched vectors", {
  from = 1:3
  to = 4
  expect_error(get_diff(from, to))
})

test_that("`get_diff` returns a diff object for a known request", {
  # request taken from example in API documentation
  diffs <- get_diff(847170467, 851733941)
  expect_equal(length(diffs), 1)
  expect_named(diffs[[1]], c("from", "to", "diff"))
  expect_length(diffs[[1]]$diff, 6)
})

test_that("`get_diff` works properly with length(from | to) > 1", {
  test_length <- 3
  from <- rep(847170467, test_length)
  to <- rep(851733941, test_length)
  diffs <- get_diff(from, to)
  expect_length(diffs, test_length)
  expect_named(diffs[[1]], c("from", "to", "diff"))
  expect_length(diffs[[1]]$diff, 6)
})

test_that("`get_langlinks` returns data for a known request", {
  titles <- c("Charles Harpur", "Percy_Bysshe_Shelley")
  links <- get_langlinks(titles)
  expect_length(links, 2)
  expect_named(links[[2]][[1]], c("code","name", "key", "title"))
})
