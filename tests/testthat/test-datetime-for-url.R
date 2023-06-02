test_that("`datetime_for_url` returns the default when x is NULL", {
  expect_equal(datetime_for_url(NULL, .default = "/"), "/")
})

test_that("`datetime_for_url` precision parameter controls output", {
  expect_equal(datetime_for_url("2023-06-02 12:08:21 AEST"), "2023-06-02")
  expect_equal(datetime_for_url("2023-06-02 12:08:21 AEST", .precision = "y"), "2023")
  expect_equal(datetime_for_url("2023-06-02 12:08:21 AEST", .precision = "ymdh"), "2023-06-02T12")
})

test_that("`datetime_for_url` raises an error if there are unparseable dates", {
  expect_error(
    datetime_for_url("2020-20-33"),
    regexp = "Unparseable dates"
  )
})
