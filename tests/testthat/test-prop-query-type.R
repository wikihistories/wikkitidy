test_that("`query_by_title` returns data for a known request", {
  resp <- wiki_action_request() %>%
    query_by_title("Charles Harpur") %>%
    query_by_title("Henry Lawson") %>%
    query_page_properties("iwlinks") %>%
    query_page_properties("categories") %>%
    next_batch()
  expect_named(resp, c("pageid", "ns", "title", "iwlinks", "categories"))
  expect_equal(nrow(resp), 2)
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

test_that("Cases where single properties or missing properties are requested still work", {
  batch_w_nulls <- c(3215834, 3916199, 5491364, 5541057, 5658396, 6631781, 7238180, 8170822, 9446340, 9950907, 10771873, 10850302, 12017474, 12097820, 18445575, 20341950, 25603362, 31049868, 32065309, 35912171, 38257800)
  nulls_req <- wiki_action_request() %>% query_by_pageid(batch_w_nulls) %>% query_page_properties("pageprops", ppprop = "wikibase_item")

  batch_no_nulls <- c(693740, 1054729, 2225029, 3616199, 4070847, 4430625, 4689323, 4689448, 5159977, 5655643)
  no_nulls_req <- wiki_action_request() %>% query_by_pageid(batch_no_nulls) %>% query_page_properties("pageprops", ppprop = "wikibase_item")

  expect_equal(nrow(next_result(nulls_req)), length(batch_w_nulls))
  expect_equal(nrow(next_result(no_nulls_req)), length(batch_no_nulls))
})
