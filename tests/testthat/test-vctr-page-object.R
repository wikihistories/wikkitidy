test_that("`page_object` returns a vector of class page_object", {
  expect_s3_class(page_object(1:3), "page_object")
})
