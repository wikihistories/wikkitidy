# Gracefully request a resource from Wikipedia

The main purpose of this function is to enable examples using live
resources in the documentation. Examples must not throw errors,
according to CRAN policy. If you wrap a requesting method in
`gracefully`, then any errors of type `httr2_http` (HTTP errors) or
`httr2_failure` (e.g. connection failures) will be caught and no error
will be thrown.

## Usage

``` r
gracefully(request_object, request_method)
```

## Arguments

- request_object:

  A `httr2_request` object describing a query to a Wikimedia Action API

- request_method:

  The desired function for performing the request, typically one of
  those in
  [get_query_results](https://wikihistories.github.io/wikkitidy/reference/get_query_results.md)

## Value

The output of `request_method` called on `request_object`, if the
request was successful. Otherwise a `wikkitidy_failure` object wrapping
the `httr2_response` (HTTP error) or the condition (connection failure).
Test for it with
[`is_wikkitidy_failure()`](https://wikihistories.github.io/wikkitidy/reference/is_wikkitidy_failure.md).

## Details

If `request_object` is already a `wikkitidy_failure` (see
[`is_wikkitidy_failure()`](https://wikihistories.github.io/wikkitidy/reference/is_wikkitidy_failure.md)),
it is returned unchanged without calling `request_method`. This lets
failures pass through a pipeline of `gracefully()` calls.

## Examples

``` r
# This fails without throwing an error
req <- httr2::request(httr2::example_url()) |>
  httr2::req_url_path("/status/404")

resp <- gracefully(req, httr2::req_perform)

print(resp)
#> → The query you tried was unsuccessful. See details below.
#> <httr2_response>
#> GET http://127.0.0.1:43627/status/404
#> Status: 404 Not Found
#> Content-Type: text/plain
#> Body: None

# A connection failure is also caught
bad <- httr2::request("http://127.0.0.1:1") |>
  httr2::req_retry(max_tries = 1)

failed <- gracefully(bad, httr2::req_perform)

is_wikkitidy_failure(failed)
#> [1] TRUE

# A failure short-circuits: the method is not called again
gracefully(failed, httr2::req_perform) |>
  is_wikkitidy_failure()
#> [1] TRUE

# This request succeeds
req <- httr2::request(httr2::example_url())

resp <- gracefully(req, httr2::req_perform)

print(resp)
#> <httr2_response>
#> GET http://127.0.0.1:43627/
#> Status: 200 OK
#> Content-Type: text/html
#> Body: In memory (357062 bytes)
```
