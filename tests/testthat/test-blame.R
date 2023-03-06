test_that("`.compute_boundaries` moves the search window appropriately", {
  boundaries <- list(upper=10000, lower=1)
  left_branch <- .compute_boundaries(boundaries, "branch_left")
  expect_equal(left_branch, list(upper=5000, lower=1))
  expect_equal(.compute_boundaries(left_branch, "try_right"), list(upper=10000, lower=5001))
})

