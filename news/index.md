# Changelog

## wikkitidy (development version)

- New `wikkitidy_failure` class and
  [`is_wikkitidy_failure()`](https://wikihistories.github.io/wikkitidy/reference/is_wikkitidy_failure.md).
  [`gracefully()`](https://wikihistories.github.io/wikkitidy/reference/gracefully.md)
  now returns a `wikkitidy_failure` on HTTP errors and connection
  failures, and short-circuits if passed one

- `failure_mode = "quiet"` in
  [`get_rest_resource()`](https://wikihistories.github.io/wikkitidy/reference/get_rest_resource.md)
  now records connection failures as a placeholder row
  (`error_code = NA`, with `error_message`)

- [`get_diff()`](https://wikihistories.github.io/wikkitidy/reference/get_diff.md)
  gains `failure_mode`

- Live API examples now always fail gracefully

- Added `skip_on_cran()` to remaining live Action API tests
  ([`query_generate_pages()`](https://wikihistories.github.io/wikkitidy/reference/query_generate_pages.md),
  [`query_list_pages()`](https://wikihistories.github.io/wikkitidy/reference/query_list_pages.md),
  [`query_by_title()`](https://wikihistories.github.io/wikkitidy/reference/query_by_.md)),
  which were failing on CRAN

- [`gracefully()`](https://wikihistories.github.io/wikkitidy/reference/gracefully.md)
  tests now skip if `webfakes` is not installed

- Fixed `failure_mode = "quiet"` in
  [`get_rest_resource()`](https://wikihistories.github.io/wikkitidy/reference/get_rest_resource.md):
  `error_code` column was dropped when some requests succeeded, and a
  vector (not tibble) was returned when all failed

## wikkitidy 0.1.14

CRAN release: 2025-02-13

- Added `failure_mode` parameter to
  [`get_history_count()`](https://wikihistories.github.io/wikkitidy/reference/get_history_count.md)
  and `page_vector_functions`
- Changed behaviour of
  [`get_rest_resource()`](https://wikihistories.github.io/wikkitidy/reference/get_rest_resource.md)
  so that the number of output rows is guaranteed to be the same as the
  number of input rows. Now if a request fails, a new column is added to
  the output `error_code`, with the http response code.
- Removed `get_page_related()` due to deprecation of API endpoint
- `wikkitidy` now depends on R 4.1, due to use of the native pipe
- Removed use of “redirect” parameter in `wikimedia_rest_request` tests,
  due to deprecation

## wikkitidy 0.1.13

CRAN release: 2024-08-17

- New
  [`gracefully()`](https://wikihistories.github.io/wikkitidy/reference/gracefully.md)
  function added to allow graceful failure of http requests, as per CRAN
  policy. All API calls to Wikipedia query modules are now wrapped in
  [`gracefully()`](https://wikihistories.github.io/wikkitidy/reference/gracefully.md)
  in the examples.

## wikkitidy 0.1.12

CRAN release: 2024-02-09

- `tidyr` moved from Imports to Suggests
- Automatic unnesting, introduced in 0.1.8, removed. It was causing bugs
  in certain edge cases, when nested data frames duplicated columns from
  the enclosing data frame. Unnesting is now left to the user, as shown
  in the examples.

## wikkitidy 0.1.11

CRAN release: 2023-10-20

- Live API calls removed from test suite on CRAN; these are still tested
  in the package’s CI pipeline.
- Fixed failing test due to Wikimedia server error

## wikkitidy 0.1.10

CRAN release: 2023-10-06

- First release
- Documentation errors removed

## wikkitidy 0.1.9

- Release candidate
- Updated documentation

## wikkitidy 0.1.8

- Fixed
  [`perform_query()`](https://wikihistories.github.io/wikkitidy/reference/perform_query.md)
  bug again, to deal with results that return nothing for the given
  query for given pages
- Fewer queries return nested data frames now

## wikkitidy 0.1.7

- Fixed bug where
  [`perform_query()`](https://wikihistories.github.io/wikkitidy/reference/perform_query.md)
  would fail if only a single property was requested from an
  API:Properties module (i.e. using
  [`new_prop_query()`](https://wikihistories.github.io/wikkitidy/reference/new_prop_query.md)
  or
  [`query_page_properties()`](https://wikihistories.github.io/wikkitidy/reference/query_page_properties.md))

## wikkitidy 0.1.6

- “endpoint” parameter of
  [`get_rest_resource()`](https://wikihistories.github.io/wikkitidy/reference/get_rest_resource.md)
  renamed to “api”
- New
  [`xtools_page()`](https://wikihistories.github.io/wikkitidy/reference/xtools_page.md)
  functions, allowing programmatic access to statistics about pages’
  edit histories, links and quality markers
- [`get_rest_resource()`](https://wikihistories.github.io/wikkitidy/reference/get_rest_resource.md)
  now supports a ‘quiet’ failure mode, where 4xx and 5xx errors quietly
  return no data. Currently implemented for the
  [`xtools_page()`](https://wikihistories.github.io/wikkitidy/reference/xtools_page.md)
  functions

## wikkitidy 0.1.5

- The functions that perform queries to the Action API have been
  rewritten. There is now a family of three functions:
  [`next_result()`](https://wikihistories.github.io/wikkitidy/reference/get_query_results.md),
  [`next_batch()`](https://wikihistories.github.io/wikkitidy/reference/get_query_results.md)
  and
  [`retrieve_all()`](https://wikihistories.github.io/wikkitidy/reference/get_query_results.md).
- Improvements to the formatting of results. Raw list columns are
  replaced with lists of tibbles, to enable easy navigation of the
  results using
  [`tidyr::unnest()`](https://tidyr.tidyverse.org/reference/unnest.html).

## wikkitidy 0.1.4

- [`build_category_tree()`](https://wikihistories.github.io/wikkitidy/reference/query_category_members.md):
  recur through category system to build up graph of subcategories and
  pages

## wikkitidy 0.1.3

- [`query_category_members()`](https://wikihistories.github.io/wikkitidy/reference/query_category_members.md):
  the first of a set of new helper functions to provide user-friendly
  access to the [Action API](https://www.mediawiki.org/wiki/API).
- `query_list_of` renamed to
  [`query_list_pages()`](https://wikihistories.github.io/wikkitidy/reference/query_list_pages.md)
- New functions `query_by_title`,
  [`query_by_pageid()`](https://wikihistories.github.io/wikkitidy/reference/query_by_.md)
  and
  [`query_by_revid()`](https://wikihistories.github.io/wikkitidy/reference/query_by_.md)
  to build new kinds of [Action API](https://www.mediawiki.org/wiki/API)
  queries
- New type system for Action API: see constructors
  [`new_prop_query()`](https://wikihistories.github.io/wikkitidy/reference/new_prop_query.md),
  [`new_generator_query()`](https://wikihistories.github.io/wikkitidy/reference/new_generator_query.md)
  and
  [`new_list_query()`](https://wikihistories.github.io/wikkitidy/reference/new_list_query.md)

## wikkitidy 0.1.2

- [`get_history_count()`](https://wikihistories.github.io/wikkitidy/reference/get_history_count.md):
  find out how many times a page has been edited
- Improved handling of
  [`get_diff()`](https://wikihistories.github.io/wikkitidy/reference/get_diff.md)
  response data
- Improved handling of
  [`page_vector_functions()`](https://wikihistories.github.io/wikkitidy/reference/page_vector_functions.md)
  response data. Most functions now return a tbl or list of tbls
- Basic support for
  [`wikimedia_rest_apis()`](https://wikihistories.github.io/wikkitidy/reference/wikimedia_rest_apis.md)
- ‘perform_query_once()’ renamed to
  [`next_batch()`](https://wikihistories.github.io/wikkitidy/reference/get_query_results.md)

## wikkitidy 0.1.1

- Better parsing of response values. All functions will now return a
  simple vector, tbl_df, or a list of tbl_dfs, with the exception of the
  Action API Query modules, whose response object has been renamed
  [`query_tbl()`](https://wikihistories.github.io/wikkitidy/reference/query_tbl.md)
  for clarity

## wikkitidy 0.1.0

- Underlying interface to the three main APIs:
  [`core_rest_request()`](https://wikihistories.github.io/wikkitidy/reference/wikipedia_rest_apis.md),
  [`wiki_action_request()`](https://wikihistories.github.io/wikkitidy/reference/wiki_action_request.md)
  and
  [`wikimedia_rest_request()`](https://wikihistories.github.io/wikkitidy/reference/wikipedia_rest_apis.md)
- First few vector functions for getting page-level data
- First few helper functions to build more complex Action API requests
- First few data types for response values
- Added a `NEWS.md` file to track changes to the package.
- New low-level helper,
  [`get_rest_resource()`](https://wikihistories.github.io/wikkitidy/reference/get_rest_resource.md),
  for generating REST API calls; refactored existing calls to use this
  function
- REST requests now accept query parameters
- New family of `page_vector_functions` to retrieve data about a vector
  of page titles,
  e.g. [`get_latest_revision()`](https://wikihistories.github.io/wikkitidy/reference/page_vector_functions.md)
