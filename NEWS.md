# wikkitidy 0.1.14

* Added `failure_mode` parameter to `get_history_count()` and `page_vector_functions`
* Changed behaviour of `get_rest_resource()` so that the number of output rows is guaranteed to be the same as the number of input rows. Now if a request fails, a new column is added to the output `error_code`, with the http response code.
* Removed `get_page_related()` due to deprecation of API endpoint
* `wikkitidy` now depends on R 4.1, due to use of the native pipe
* Removed use of "redirect" parameter in `wikimedia_rest_request` tests, due to deprecation

# wikkitidy 0.1.13

* New `gracefully()` function added to allow graceful failure of http requests, as per CRAN policy. All API calls to Wikipedia query modules are now wrapped in `gracefully()` in the examples.

# wikkitidy 0.1.12

* `tidyr` moved from Imports to Suggests
* Automatic unnesting, introduced in 0.1.8, removed. It was causing bugs in certain edge cases, when nested data frames duplicated columns from the enclosing data frame. Unnesting is now left to the user, as shown in the examples.

# wikkitidy 0.1.11

* Live API calls removed from test suite on CRAN; these are still tested in the package's CI pipeline.
* Fixed failing test due to Wikimedia server error

# wikkitidy 0.1.10

* First release
* Documentation errors removed

# wikkitidy 0.1.9

* Release candidate
* Updated documentation

# wikkitidy 0.1.8

* Fixed `perform_query()` bug again, to deal with results that return nothing for the given query for given pages
* Fewer queries return nested data frames now

# wikkitidy 0.1.7

* Fixed bug where `perform_query()` would fail if only a single property was requested from an API:Properties module (i.e. using `new_prop_query()` or `query_page_properties()`)

# wikkitidy 0.1.6

* "endpoint" parameter of `get_rest_resource()` renamed to "api"
* New `xtools_page()` functions, allowing programmatic access to statistics about pages' edit histories, links and quality markers
* `get_rest_resource()` now supports a 'quiet' failure mode, where 4xx and 5xx errors quietly return no data. Currently implemented for the `xtools_page()` functions

# wikkitidy 0.1.5

* The functions that perform queries to the Action API have been rewritten. There is now a family of three functions: `next_result()`, `next_batch()` and `retrieve_all()`.
* Improvements to the formatting of results. Raw list columns are replaced with lists of tibbles, to enable easy navigation of the results using `tidyr::unnest()`.

# wikkitidy 0.1.4

* `build_category_tree()`: recur through category system to build up graph of subcategories and pages

# wikkitidy 0.1.3

* `query_category_members()`: the first of a set of new helper functions to provide user-friendly access to the [Action API](https://www.mediawiki.org/wiki/API).
* `query_list_of` renamed to `query_list_pages()`
* New functions `query_by_title`, `query_by_pageid()` and `query_by_revid()` to build new kinds of [Action API](https://www.mediawiki.org/wiki/API) queries
* New type system for Action API: see constructors `new_prop_query()`, `new_generator_query()` and `new_list_query()`

# wikkitidy 0.1.2

* `get_history_count()`: find out how many times a page has been edited
* Improved handling of `get_diff()` response data
* Improved handling of `page_vector_functions()` response data. Most functions now return a tbl or list of tbls
* Basic support for `wikimedia_rest_apis()`
* 'perform_query_once()' renamed to `next_batch()`

# wikkitidy 0.1.1

* Better parsing of response values. All functions will now return a simple vector, tbl_df, or a list of tbl_dfs, with the exception of the Action API Query modules, whose response object has been renamed `query_tbl()` for clarity

# wikkitidy 0.1.0

* Underlying interface to the three main APIs: `core_rest_request()`, `wiki_action_request()` and `wikimedia_rest_request()`
* First few vector functions for getting page-level data
* First few helper functions to build more complex Action API requests
* First few data types for response values
* Added a `NEWS.md` file to track changes to the package.
* New low-level helper, `get_rest_resource()`, for generating REST API calls; refactored existing calls to use this function
* REST requests now accept query parameters
* New family of `page_vector_functions` to retrieve data about a vector of page titles, e.g. `get_latest_revision()`
