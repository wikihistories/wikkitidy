test_that("Request is correctly modified by wikkitidy_user_agent", {
  request <- httr2::request("example.com") %>% wikkitidy_user_agent()
  expect_equal(request$options$useragent, "wikkitidy R package (https://github.com/wikihistories/wikkitidy)")
})
