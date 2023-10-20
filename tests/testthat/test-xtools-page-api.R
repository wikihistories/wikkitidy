test_that("all xtools page endpoints retrieve data for known requests", {
  skip_on_cran()
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
      limit = 20)
    ), 0)
})

test_that("responses are parsed correctly when failure_mode == 'quiet'", {
  skip_on_cran()
  titles <- c("Albert Einstein", "chuck doodlemacdoodle7", "Sydney")
  expect_equal(nrow(get_xtools_page_info(titles, failure_mode = "quiet")), 3)
  expect_equal(length(get_xtools_page_assessment(titles, failure_mode = "quiet")), 3)
  expect_equal(nrow(get_xtools_page_prose(titles, failure_mode = "quiet")), 3)
  expect_equal(nrow(get_xtools_page_links(titles, failure_mode = "quiet")), 3)
  expect_equal(nrow(
    get_xtools_page_top_editors(
      titles,
      start = "2020-01-01",
      end = as.Date("2021-01-01"),
      nobots = T,
      limit = 20,
      failure_mode = "quiet")
  ), 3)
})
