test_that("`history_count` returns a vector of the right type", {
  val <- 1:10
  lim <- rep(F, 10)
  type <- "edit"
  req <- httr2::request("example.com")
  expect_s3_class(history_count(val, lim, type, req), "history_count")
})
