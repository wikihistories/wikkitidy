test_that("`query_category_members` returns data for a known request", {
  physics_members <- query_category_members("Physics", limit = 10) %>% next_batch()
  expect_equal(nrow(physics_members), 10)
})

test_that("An appropriate error is thrown if `start` or `end` are used without `sort` == 'timestamp'", {
  expect_error(query_category_members("Physics", start = "2002-01-10"))
})
