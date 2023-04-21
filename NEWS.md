# wikkitidy (development version)

* Underlying interface to the three main APIs: [core_rest_request], [wiki_action_request] and [wikimedia_rest_request]
* First few vector functions for getting page-level data
* First few helper functions to build more complex Action API requests
* First few data types for response values

# wikkitidy 0.1.0.9000

* Added a `NEWS.md` file to track changes to the package.
* New low-level helper, [get_rest_resource], for generating REST API calls; refactored existing calls to use this function
* REST requests now accept query parameters
