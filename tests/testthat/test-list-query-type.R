test_that("`query_list_pages` returns data for a known request", {
  physics_pages <- wiki_action_request() %>%
    query_list_pages("categorymembers",
                     cmsort = "timestamp",
                     cmdir = "desc", cmtitle = "Category:Physics"
    ) %>%
    next_batch()
  expect_true(tibble::is_tibble(physics_pages))
  expect_gt(nrow(physics_pages), 1)
  expect_named(physics_pages, c("pageid", "ns", "title"))
})

test_that("`list_all_list_modules() returns a tibble of one column", {
  expect_named(list_all_list_modules(), "name")
  expect_gt(nrow(list_all_list_modules()), 50)
})

test_that("type composition rules are enforced correctly", {
  req <- wiki_action_request()
  expect_error(
    req %>% new_prop_query("titles", "foo") %>% new_list_query("categorymembers"),
    class = "incompatible_query_error"
  )
  expect_error(
    req %>% new_generator_query("categorymembers") %>% new_list_query("categorymembers"),
    class = "incompatible_query_error"
  )
  expect_no_error(
    req %>% new_list_query("usercontribs") %>% new_list_query("categorymembers"),
    class = "incompatible_query_error"
  )
})

test_that("new_list_query() returns an object of the correct type", {
  expect_true(
    wiki_action_request() %>% new_list_query("categorymembers") %>% is_list_query()
  )
  expect_s3_class(
    wiki_action_request() %>% new_list_query("categorymembers"),
    c("list", BASE_QUERY_CLASS)
  )
})
