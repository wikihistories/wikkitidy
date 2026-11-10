# Representation of Wikipedia data returned from an [Action API Query module](https://www.mediawiki.org/wiki/API:Query) as tibble, with request metadata stored as attributes.

Representation of Wikipedia data returned from an [Action API Query
module](https://www.mediawiki.org/wiki/API:Query) as tibble, with
request metadata stored as attributes.

## Usage

``` r
query_tbl(x, request, continue, batchcomplete)
```

## Arguments

- x:

  A tibble

- request:

  The httr2_request object used to generate the tibble

- continue:

  The continue parameter returned by the API

- batchcomplete:

  The batchcomplete parameter returned by the API

## Value

A tibble: an S3 data.frame with class `query_tbl`.
