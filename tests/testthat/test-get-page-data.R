test_that("wikimedia API page endpoints return data for known requests", {
  title <- c("Negative Dialektik", "Germaine_de_Staël")
  language <- c("de", "fr")

  metadata <- get_page_metadata(title, language)
  expect_length(metadata, 2)

  html <- get_page_html(title, language)
  expect_length(html, 2)
  expect_s3_class(html[[1]], "xml_document")

  summary <- get_page_summary(title, language)
  summary_names <- c('type', 'title', 'displaytitle', 'namespace', 'wikibase_item', 'titles', 'pageid', 'thumbnail', 'originalimage', 'lang', 'dir', 'revision', 'tid', 'timestamp', 'description', 'description_source', 'content_urls', 'extract', 'extract_html')
  expect_length(summary, 2)
  expect_named(summary[[1]], summary_names)
  expect_named(summary[[2]], summary_names)

  related <- get_page_related(title, language)
  expect_length(related, 2)
  expect_length(related[[1]]$pages, 20)
  expect_length(related[[2]]$pages, 20)

  talk <- get_page_talk(title, language)
  expect_length(talk, 2)
  expect_named(talk[[1]], c("revision", "topics"))
  expect_named(talk[[2]], c("revision", "topics"))
})

test_that("`get_page_langlinks` returns data for known requests", {
  titles <- c("Charles Harpur", "Percy_Bysshe_Shelley")
  links <- get_page_langlinks(titles)
  expect_length(links, 2)
  expect_s3_class(links[[1]], "tbl_df")
  expect_s3_class(links[[2]], "tbl_df")
})
