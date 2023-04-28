test_that("`as_wiki_tibble` creates a valid `query_tbl`", {
  test_tbl <- query_tbl(list(x=1:3, y=4:6), request = NULL, batchcomplete = NULL, continue = NULL)
  expect_no_error(validate_query_tbl(test_tbl))
})

test_that("`validate_query_tbl` throws an error if the data frame is not a tibble", {
  not_a_tibble <- data.frame(a=1:5, b=1:5)
  expect_error(validate_query_tbl(not_a_tibble), regexp = "not a valid tibble")
})

test_that("`validate_query_tbl` throws an error if key metadata is missing", {
  no_request <- query_tbl(list(x=1:3, y=4:6), request = NULL, batchcomplete = NULL, continue = NULL)
  attr(no_request, "request") <- NULL
  expect_error(validate_query_tbl(no_request), regexp = "missing one or more")
  no_continue <- query_tbl(list(x=1:3, y=4:6), request = NULL, batchcomplete = NULL, continue = NULL)
  attr(no_continue, "continue") <- NULL
  expect_error(validate_query_tbl(no_continue), regexp = "missing one or more")
  no_batchcomplete <- query_tbl(list(x=1:3, y=4:6), request = NULL, batchcomplete = NULL, continue = NULL)
  attr(no_batchcomplete, "batchcomplete") <- NULL
  expect_error(validate_query_tbl(no_batchcomplete), regexp = "missing one or more")
})

test_that("`get_request` retrieves request object from query_tbl", {
  test_request <- httr2::request("www.example.com")
  test_tbl <- query_tbl(list(x=1:3, y=4:6), request = test_request, batchcomplete = NULL, continue = NULL)
  expect_equal(test_request, get_request(test_tbl))
})

test_that("`get_continue` retrieves continue list from query_tbl", {
  test_continue <- list(continue = "||=", querycontinue = "example_text")
  test_tbl <- query_tbl(list(x = 1:3, y = 4:6), continue = test_continue, request = NULL, batchcomplete = NULL)
  expect_equal(test_continue, get_continue(test_tbl))
})

test_that("`get_batchcomplete` retrieves batchcomplete flag from query_tbl", {
  test_batchcomplete = "NO"
  test_tbl <- query_tbl(list(x = 1:3, y = 4:6), batchcomplete = test_batchcomplete, request = NULL, continue = NULL)
  expect_equal(test_batchcomplete, get_batchcomplete(test_tbl))
})
