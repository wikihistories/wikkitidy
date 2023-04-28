# wikkitidy (development version)

* `get_history_counts()`: find out how many times a page has been edited
* Improved handling of `get_diff()` response data

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
* New family of `get_page` functions to retrieve data about a vector of page titles, e.g. `get_page_metadata()`
