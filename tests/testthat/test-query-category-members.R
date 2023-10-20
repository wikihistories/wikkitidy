test_that("`query_category_members` returns data for a known request", {
  skip_on_cran()
  physics_members <- wiki_action_request() %>% query_category_members("Physics", limit = 10) %>% next_batch()
  expect_equal(nrow(physics_members), 10)
})

test_that("An appropriate error is thrown if `start` or `end` are used without `sort` == 'timestamp'", {
  expect_error(
    wiki_action_request() %>%
      query_category_members("Physics", start = "2002-01-10")
    )
})

test_that("`build_category_tree` returns data for a known request", {
  skip_on_cran()
  tree <- build_category_tree("Category:Trees (data structures)")
  expect_named(tree, c("nodes", "edges"))
  expect_gt(nrow(tree$nodes), 1)
  expect_gt(nrow(tree$edges), 1)
  expect_named(tree$nodes, c("pageid", "ns", "title", "type"))
  expect_named(tree$edges, c("source", "target", "timestamp"))
})
