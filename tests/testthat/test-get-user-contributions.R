test_that("Requesting a users' contributions receives a 200", {
  resp <- get_user_contributions("michaelgfalk", dir="newer")
  expect_equal(httr2::resp_status(resp), 200)
})
