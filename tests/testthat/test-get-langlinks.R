test_that("`get_langlinks` returns data for a known request", {
  titles <- c("Charles Harpur", "Percy_Bysshe_Shelley")
  links <- get_langlinks(titles)
  expect_length(links, 2)
  expect_named(links[[2]][[1]], c("code","name", "key", "title"))
})
