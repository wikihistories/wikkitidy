test_that("`.get_checksums()` can find the checksums file", {
  path <- wikkitidy_example("akan_wiki")
  expect_no_error(.get_checksums(path))
  expect_type(.get_checksums(path), "character")
})

test_that("`.checksum_tbl()` correctly formats a known checksum file", {
  path <- wikkitidy_example("akan_wiki")
  tbl <- .get_checksums(path) %>% .checksum_tbl()
  expect_equal(ncol(tbl), 2)
  expect_equal(nrow(tbl), 39)
})

test_that("`verify_xml_integrity() validates a known file", {
  path <- wikkitidy_example("akan_wiki")
  expect_no_error(verify_xml_integrity(path))
})
