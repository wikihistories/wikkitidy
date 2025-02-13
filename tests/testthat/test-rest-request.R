test_that("`core_rest_request` returns a 200 for a basic request", {
  skip_on_cran()
  response <- core_rest_request("page", "Charles_Harpur", "html") %>%
    httr2::req_perform()
  expect_equal(httr2::resp_status(response), 200)
})

test_that("`wikimedia_rest_request` returns a 200 for a basic request", {
  skip_on_cran()
  response <- wikimedia_rest_request("page", "html", "Charles%20Harpur") %>%
    httr2::req_perform()
  expect_equal(httr2::resp_status(response), 200)
})

test_that("`wikimedia_org_rest_request` returns a 200 for a known request", {
  skip_on_cran()
  response <- wikimedia_org_rest_request(
    endpoint = "/metrics/pageviews/aggregate/",
    "all-access", "all-agents", "monthly", "2022010100", "2022033123",
    language = "en"
  ) %>% httr2::req_perform()
  expect_equal(httr2::resp_status(response), 200)
})

test_that("`xtools_rest_request` returns a 200 for a known request", {
  skip_on_cran()
  response <- xtools_rest_request(
    endpoint = c("user", "simple_editcount"),
    "michaelgfalk",
    language = "en"
  ) %>% httr2::req_perform()
  expect_equal(httr2::resp_status(response), 200)
})

test_that("Named arguments are handled correctly", {
  skip_on_cran()
  lim <- 2
  core_resp <- core_rest_request("search", "page", q="Goethe", limit=lim, language="de") %>%
    httr2::req_perform()
  expect_equal(httr2::resp_status(core_resp), 200)
  expect_length(httr2::resp_body_json(core_resp)$pages, lim)
})

test_that("appropriate error if no path components provided", {
  expect_error(core_rest_request(), "no path components")
})

