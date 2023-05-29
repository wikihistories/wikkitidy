test_that("`merge_tbl_cols` merges lists in the correct places", {
  old <- tibble::tribble(
    ~x, ~y, ~z,
    0, "zero", data.frame(q = 1:10),
    1, "one", list(),
  )
  new <- tibble::tribble(
    ~x, ~y, ~z,
    1, "one", data.frame(q = 11:15),
  )
  expect_equal(
    merge_tbl_cols(old, new),
    tibble::tribble(
      ~x, ~y, ~z,
      0, "zero", data.frame(q = 1:10),
      1, "one", data.frame(q = 11:15),
    )
  )
})
