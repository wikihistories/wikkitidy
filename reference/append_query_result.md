# Combine new results for a query with previously downloaded results

Combine new results for a query with previously downloaded results

## Usage

``` r
append_query_result(old, new)
```

## Arguments

- old:

  The
  [query_tbl](https://wikihistories.github.io/wikkitidy/reference/query_tbl.md)
  of previous results

- new:

  The
  [query_tbl](https://wikihistories.github.io/wikkitidy/reference/query_tbl.md)
  of new results from the server

## Value

A new
[query_tbl](https://wikihistories.github.io/wikkitidy/reference/query_tbl.md)
of the appropriate subclass, depending on whether the batch is complete.

## See also

[`perform_query()`](https://wikihistories.github.io/wikkitidy/reference/perform_query.md)
