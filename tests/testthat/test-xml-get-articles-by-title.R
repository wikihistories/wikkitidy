test_that("index is built correctly with multiple out-of-order titles", {
  index <- make_index(
    titles = c("Nana Akua Addo", "Sophia Horner-Sam", "Nana Darkoa Sekyiamah"),
    index_file = wikkitidy_example("fatwiki_index")
  )
  expect_named(index, c("499966","570049"))
  expect_equal(index[[1]], c("Nana Akua Addo", "Nana Darkoa Sekyiamah"))
  expect_equal(index[[2]], "Sophia Horner-Sam")
})
