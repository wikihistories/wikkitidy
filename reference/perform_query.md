# Perform a single request to the Action API.

This function is the workhorse behind the user-facing
[`next_result()`](https://wikihistories.github.io/wikkitidy/reference/get_query_results.md),
[`next_batch()`](https://wikihistories.github.io/wikkitidy/reference/get_query_results.md)
and
[`retrieve_all()`](https://wikihistories.github.io/wikkitidy/reference/get_query_results.md).

## Usage

``` r
perform_query(request, continue)
```

## Arguments

- request:

  The request object

- continue:

  The continue parameter returned by the previous request

## Value

A
[`query_tbl()`](https://wikihistories.github.io/wikkitidy/reference/query_tbl.md)
of the results

## See also

[`append_query_result()`](https://wikihistories.github.io/wikkitidy/reference/append_query_result.md)
