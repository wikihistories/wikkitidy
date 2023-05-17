test_that("`query_generate_pages` throws an error for a made-up generator", {
  expect_error(wiki_action_request() %>% query_generate_pages("nuclear"))
})

test_that("`query_generate_pages` throws an error if list and property generators are ill-combined", {
  req <- wiki_action_request()
  expect_error(req %>% query_generate_pages("categories"), class = "malformed_generator")
  expect_s3_class(
    req %>% query_by_title("Charles Harpur") %>% query_generate_pages("categories"),
    c("generator", "titles", "prop", BASE_QUERY_CLASS)
    )
})

test_that("`query_generate_pages` returns data for a known request", {
  seagulls <- wiki_action_request() %>%
    query_generate_pages("search", gsrsearch = "seagull") %>%
    next_batch()
  expect_true(tibble::is_tibble(seagulls))
  expect_equal(nrow(seagulls), 10)
  expect_named(seagulls, c("pageid", "ns", "title", "index"))
})

test_that("`list_all_generators` returns a tibble of the right form", {
  expect_named(list_all_generators(), c("name", "group"))
})

test_that("`new_generator_query` returns a object of the correct type", {
  expect_s3_class(wiki_action_request() %>% new_generator_query("categorymembers"), c("generator", "query"))
  expect_s3_class(
    wiki_action_request() %>% new_prop_query("titles", "Main Page") %>% new_generator_query("categories"),
    c("generator", "titles", "prop", BASE_QUERY_CLASS)
    )
  expect_s3_class(
    wiki_action_request() %>%
      new_generator_query("categorymembers") %>%
      new_generator_query("allcategories"),
    c("generator", BASE_QUERY_CLASS)
  )
})

test_that("`new_generator_query` will not combine with list queries or non-query requests", {
  expect_error(wiki_action_request(action = "madeup") %>% new_generator_query("categorymembers"))
  expect_error(wiki_action_request() %>% new_list_query("categorymembers") %>% new_generator_query("allcategories"),
               class = "incompatible_query_error")
})

test_that("the return value of `new_generator_query` returns TRUE for `is_generator_query`", {
  expect_true(wiki_action_request() %>% new_generator_query("categorymembers") %>% is_generator_query())
})
