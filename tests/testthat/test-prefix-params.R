test_that("All and only unprefixed params are prefixed", {
  expect_equal(
    prefix_params(c("limit", "uccontinue", "userids"), "uc"),
    c("uclimit", "uccontinue", "ucuserids")
  )
})
