test_that("`get-rest-resource` obtains data for known valid requests", {
  titles <- c("Charles_Harpur", "Earth")
  response <- get_rest_resource("page", titles, "html", response_format = "html")
  expect_length(response, 2)
})

test_that("`get-rest-resource` vectorises correctly over languages", {
  ttl <- c("Charles_Harpur", "Charles_Baudelaire")
  lng <- c("en", "fr")
  response <- get_rest_resource("page", ttl, "html", language = lng, response_format = "html")
  expect_length(response, 2)
})

test_that("`get_rest_resource` correctly encodes URLS", {
  ttl <- c("Charles Harpur", "Charles_Baudelaire", "Earth")
  response <- get_rest_resource("page", ttl)
  expect_length(response, 3)
})

test_that("`get_rest_resource` is intolerant of failure by default", {
  ttl <- c("Kharles Harpur", "Charles_Baaaudelaire", "Eurth")
  expect_error(get_rest_resource("page", ttl), class = "purrr_error_indexed")
})

test_that("`get_rest_resource` is tolerant of failure when failure_mode == 'quiet'", {
  ttl <- c("Kharles Harpur", "Charles Baudelaire")
  response <- get_rest_resource("page", ttl, failure_mode = "quiet")
  expect_length(response, 2)
})
