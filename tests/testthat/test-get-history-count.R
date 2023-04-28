test_that("the rules about `from` and `to` are enforced correctly", {
  expect_error(get_history_count("Jupiter", from = 1000))
  expect_error(get_history_count("Juppiter", to = 1000))
  expect_error(get_history_count("Jupiter", from = 1000, to = 1000, type = "bot"))
})

test_that("`get_history_count` returns properly-structured data for known request", {
  authors <- tibble::tribble(
    ~author,
    "Jane Austen",
    "William Shakespeare",
    "Emily Dickinson"
  ) %>%
    dplyr::mutate(get_history_count(author))
  expect_named(authors, c("author", "count", "limit"))
  expect_equal(nrow(authors), 3L)
})
