test_that("wikimedia API page endpoints return data for known requests", {
  skip_on_cran()
  title <- c("Negative Dialektik", "Germaine_de_Staël")
  language <- c("de", "fr")

  metadata <- get_latest_revision(title, language)
  expect_equal(nrow(metadata), length(title))
  expect_s3_class(metadata, "tbl_df")

  html <- get_page_html(title, language)
  expect_length(html, 2)
  expect_s3_class(html[[1]], "xml_document")

  summary <- get_page_summary(title, language)
  expect_equal(nrow(summary), length(title))
  expect_s3_class(summary, "tbl_df")

  related <- get_page_related(title, language)
  expect_length(related, 2)
  expect_true(rlang::is_bare_list(related))
  expect_s3_class(related[[1]], "tbl_df")
})

test_that("`get_page_talk` throws a `bad_title` error in the right circumstances", {
  expect_error(get_page_talk("Ballarat"), class="bad_title")
})

test_that("`get_page_langlinks` returns data for known requests", {
  skip_on_cran()
  titles <- c("Charles Harpur", "Percy_Bysshe_Shelley")
  links <- get_page_langlinks(titles)
  expect_length(links, 2)
  expect_s3_class(links[[1]], "tbl_df")
  expect_s3_class(links[[2]], "tbl_df")
})
