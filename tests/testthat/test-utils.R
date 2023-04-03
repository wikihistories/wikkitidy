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
