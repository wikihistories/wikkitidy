test_that("`query_by_title` returns data for a known request", {
  resp <- wiki_action_request() %>%
    query_by_title("Charles Harpur") %>%
    query_by_title("Henry Lawson") %>%
    query_page_properties("iwlinks") %>%
    query_page_properties("categories") %>%
    next_batch()
  expect_named(resp, c("pageid", "ns", "title", "iwlinks", "categories"))
  expect_gt(nrow(resp), 0)
})

test_that("`query_by_` functions returns objects of the correct type", {
  req <- wiki_action_request()
  expect_s3_class(
    req %>% query_by_pageid("1|200"),
    c("pageids", "prop", BASE_QUERY_CLASS)
  )
  expect_s3_class(
    req %>% query_by_title("1|200"),
    c("titles", "prop", BASE_QUERY_CLASS)
  )
  expect_s3_class(
    req %>% query_by_revid("1|200"),
    c("revids", "prop", BASE_QUERY_CLASS)
  )
  expect_true(
    req %>% query_by_revid(10000) %>% is_prop_query()
  )
})

test_that("type composition rules are properly enforced", {
  req <- wiki_action_request()
  expect_error(
    req %>% new_list_query("categorymembers") %>% new_prop_query("titles", "foo"),
    class = "incompatible_query_error"
  )
  expect_error(
    req %>% new_generator_query("categorymembers") %>% new_prop_query("titles", "foo"),
    class = "incompatible_query_error"
  )
  expect_error(
    req %>% new_prop_query("titles", "foo") %>% new_prop_query("pageids", "bar"),
    class = "incompatible_query_error"
  )
  expect_error(
    req %>% new_prop_query("pageids", "foo") %>% new_prop_query("revids", "bar"),
    class = "incompatible_query_error"
  )
  expect_error(
    req %>% new_prop_query("revids", "foo") %>% new_prop_query("titles", "bar"),
    class = "incompatible_query_error"
  )
  expect_no_error(
    req %>% new_prop_query("titles", "foo") %>% new_generator_query("iwlinks") %>% new_prop_query("titles", "bar"),
    class = "incompatible_query_error"
  )
})
