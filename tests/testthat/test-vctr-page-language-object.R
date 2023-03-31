test_that("`page_language_object` returns a vector of right class", {
  expect_s3_class(
    page_language_object("fr","french","monde","monde"),
    "page_language_object"
  )
})
