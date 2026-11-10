# Query the Action API continually until a continuation condition no longer holds.

Query the Action API continually until a continuation condition no
longer holds.

## Usage

``` r
continue_query(last_result, predicate, max_requests = 1000)
```

## Arguments

- last_result:

  The query_tbl of results to complete

- predicate:

  The while condition. Results will be continually requested until this
  evaluates 'false'.

## Value

A query_tbl: an S3 dataframe that is a subclass of tibble::tibble
