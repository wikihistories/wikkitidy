# Failed wikkitidy requests

A `wikkitidy_failure` is returned by
[`gracefully()`](https://wikihistories.github.io/wikkitidy/reference/gracefully.md)
when a request fails, instead of throwing an error. It wraps either an
`httr2_response` (HTTP error) or a condition object (e.g. a connection
failure).

## Usage

``` r
is_wikkitidy_failure(x)
```

## Arguments

- x:

  An object to test.

## Value

`is_wikkitidy_failure()` returns `TRUE` or `FALSE`.

## Examples

``` r
is_wikkitidy_failure(1)
#> [1] FALSE
```
