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