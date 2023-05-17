test_that("`wiki_action_request()` returns an object of the correct type", {
  action_req <- wiki_action_request()
  expect_s3_class(action_req, BASE_QUERY_CLASS)
  expect_no_error(check_is_action_query(action_req))
  expect_true(is_base_query(action_req))
})

test_that("`check_is_action_query() throws the right error with the wrong request", {
  expect_error(check_is_action_query(httr2::request("example.com")), class = "wrong_request_type")
  expect_error(check_is_action_query(wiki_action_request(action = "other")), class = "wrong_request_type")
  expect_no_error(check_is_action_query(wiki_action_request()), class = "wrong_request_type")
})

test_that("MediaWiki Action API request receives a 200 response code and json body", {
  resp <- wiki_action_request(list = "categorymembers", cmtitle = "Category:Australian_historians") %>% httr2::req_perform()
  expect_equal(httr2::resp_status(resp), 200)
  expect_equal(httr2::resp_content_type(resp), "application/json")
})

test_that("query_list_pages generates a valid query string with data in the query attribute", {
  resp <- wiki_action_request() %>%
    query_list_pages("usercontribs", ucuser="Michaelgfalk") %>%
    httr2::req_perform()
  expect_equal(httr2::resp_status(resp), 200)
  data <- httr2::resp_body_json(resp)
  expect_gt(length(data$query$usercontribs), 1)
})

test_that("`is_query_subtype` discriminates correctly", {
  expect_false(is_query_subtype(wiki_action_request(), "list"))
  expect_false(is_query_subtype(wiki_action_request(action = "list"), "list"))
  expect_true(
    wiki_action_request() %>% new_list_query("categorymembers") %>% is_query_subtype("list")
  )
})

test_that("`incompatible_query_error` throws the right error", {
  expect_error(
    incompatible_query_error("new", "old"),
    regexp = "you cannot combine a `new` query with an `old` query",
    class = "incompatible_query_error"
  )
})

test_that("`check_module()` works for known cases", {
  expect_error(
    check_module("healthbars", "lists"),
    class = "unknown_module"
  )
  expect_no_error(
    check_module("usercontribs", "lists"),
    class = "unknown_module"
  )
})
