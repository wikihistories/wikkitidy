test_that("Request is correctly modified by `wikkitidy_user_agent`", {
  request <- httr2::request("example.com") %>% wikkitidy_user_agent()
  expect_equal(request$options$useragent, "wikkitidy R package (https://github.com/wikihistories/wikkitidy)")
})


test_that("`str_for_rest` ensure that REST request works for malformed title", {
  bad_title <- "Abbey Road (album)"
  langlinks <- core_rest_request("page", str_for_rest(bad_title), "links", "language", language = "fr") %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()
  expect_gt(length(langlinks[[1]]), 1)
})

test_that("`id_or_title` correctly prefixes a string argument", {
  expect_equal(id_or_title("bar", "Talk"), list(title = "Talk:bar"))
  expect_equal(id_or_title("foo"), list(title = "foo"))
})

test_that("`id_or_title` correctly identified pageid arguments", {
  expect_equal(id_or_title(1000), list("pageid" = 1000))
  expect_equal(id_or_title("1000"), list("pageid" = 1000))
})

test_that("`check_limit` throws an error if the input has length > 0", {
  expect_error(check_limit(1:10), class = "non_scalar_arg")
  expect_error(check_limit(c("10", "20")), class = "non_scalar_arg")
})

test_that("`check_limit` throws an error for a character vector that is not 'max'", {
  expect_error(check_limit("foo"), class = "wrong_arg_type")
})

test_that("`check_limit` correctly handles string and numeric input", {
  expect_error(check_limit(50, 10), class = "exceed_max")
  expect_equal(check_limit(50, 100), 50)
  expect_equal(check_limit("max", 100), "max")
})

test_that("`process_timestamps` returns an appropriate list of results for a valid input", {
  expect_equal(process_timestamps(one = "1970-1-1", two = "30-12-30"), list(one = "1970-01-01", two = "2030-12-30"))
})

test_that("`check_namespace` returns expected values", {
  expect_null(check_namespace(NULL))
  expect_equal(check_namespace(1:5), "1|2|3|4|5")
})

test_that("`check_namspace` throws an error if a non-integer argument is passed", {
  expect_error(check_namespace("green eggs"), class = "wrong_arg_type")
})
