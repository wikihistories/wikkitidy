test_that("`get-rest-resource` obtains data for known valid requests", {
  skip_on_cran()
  titles <- c("Charles_Harpur", "Earth")
  response <- get_rest_resource("page", titles, "html", response_format = "html")
  expect_length(response, 2)
})

test_that("`get-rest-resource` vectorises correctly over languages", {
  skip_on_cran()
  ttl <- c("Charles_Harpur", "Charles_Baudelaire")
  lng <- c("en", "fr")
  response <- get_rest_resource("page", ttl, "html", language = lng, response_format = "html")
  expect_length(response, 2)
})

test_that("`get_rest_resource` correctly encodes URLS", {
  skip_on_cran()
  ttl <- c("Charles Harpur", "Charles_Baudelaire", "Earth")
  response <- get_rest_resource("page", ttl)
  expect_length(response, 3)
})

test_that("`get_rest_resource` is intolerant of failure by default", {
  skip_on_cran()
  ttl <- c("Kharles Harpur", "Charles_Baaaudelaire", "Eurth")
  expect_error(get_rest_resource("page", ttl), class = "purrr_error_indexed")
})

local_unreachable_api <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    core_rest_request = function(..., language = "en") {
      request <- httr2::request("http://127.0.0.1:1") |>
        httr2::req_retry(max_tries = 1)
      class(request) <- c("core", class(request))
      request
    },
    .env = env
  )
}

test_that("quiet mode records a connection failure as a placeholder row", {
  local_unreachable_api()
  response <- get_rest_resource("page", c("Earth", "Mars", "Venus"), failure_mode = "quiet")
  expect_length(response, 3)
  expect_true(all(purrr::map_lgl(response, \(x) is.na(x$error_code))))
  expect_true(all(purrr::map_lgl(response, \(x) is.character(x$error_message))))
})

test_that("quiet mode keeps length for mixed success and failure", {
  skip_on_cran()
  response <- get_rest_resource(
    "page", c("Earth", "Kharles Harpur", "Mars"), failure_mode = "quiet"
  )
  expect_length(response, 3)
})

test_that("error mode still throws on a connection failure", {
  local_unreachable_api()
  expect_error(get_rest_resource("page", "Earth"))
})

test_that("`get_rest_resource` is tolerant of failure when failure_mode == 'quiet'", {
  skip_on_cran()
  ttl <- c("Kharles Harpur", "Charles Baudelaire")
  response <- get_rest_resource("page", ttl, failure_mode = "quiet")
  expect_length(response, 2)
})
