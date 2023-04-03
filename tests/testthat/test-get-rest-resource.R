test_that("`get-rest-resource` obtains data for known valid requests", {
  titles <- c("Charles_Harpur", "Earth")
  response <- get_rest_resource("page", titles, "html", response_type = "html")
  expect_length(response, 2)
})

test_that("`get-rest-resource` vectorises correctly over languages", {
  ttl <- c("Charles_Harpur", "Charles_Baudelaire")
  lng <- c("en", "fr")
  response <- get_rest_resource("page", ttl, "html", language = lng, response_type = "html")
  expect_length(response, 2)
})
