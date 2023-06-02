test_that("all xtools page endpoints retrieve data for known requests", {
  expect_gt(nrow(get_xtools_page_info("Albert Einstein")), 0)
  expect_gt(length(get_xtools_page_assessment("Albert Einstein")), 1)
  expect_gt(nrow(get_xtools_page_prose("Albert Einstein", language = "de")), 0)
  expect_gt(nrow(get_xtools_page_links("Albert Einstein", language = "fr")), 0)
  expect_gt(nrow(
    get_xtools_page_top_editors(
      "Albert Einstein",
      start = "2020-01-01",
      end = as.Date("2021-01-01"),
      nobots = T,
      limit = 500)
    ), 0)
})
