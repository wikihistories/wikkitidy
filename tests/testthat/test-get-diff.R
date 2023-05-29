test_that("`get_diff` returns a single `tbl_df` for a known request", {
  # request taken from example in API documentation
  diffs <- get_diff(847170467, 851733941)
  expect_named(diffs, c('type', 'lineNumber', 'text', 'offset_from', 'offset_to'))
  expect_s3_class(diffs, "tbl_df")
})

test_that("`get_diff` returns a list of the right kind when simplify=FALSE", {
  diffs <- get_diff(847170467, 851733941, simplify = FALSE)
  expect_named(diffs, c("from", "to", "diff"))
  expect_true(rlang::is_bare_list(diffs))
})

test_that("`get_diff` throws an error with simplify is the wrong type", {
  expect_error(get_diff(100, 200, simplify = 7), "must be either TRUE or FALSE")
})

test_that("`get_diff` vectorises correctly over `from` and `to`", {
  revisions <- wiki_action_request() %>%
    query_by_title("Main Page") %>%
    query_page_properties(
      "revisions", rvlimit = 5, rvprop = "ids", rvdir = "older"
    ) %>%
    next_result() %>%
    tidyr::unnest(cols = c(revisions))
  diffs <- get_diff(revisions$parentid, revisions$revid)
  expect_equal(nrow(revisions), length(diffs))
  expect_true(rlang::is_bare_list(diffs))
})
