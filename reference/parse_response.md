# Convert a response from a Wikipedia API into a convenient format

Wikipedia's APIs provide data using a range of different json schemas.
This generic function converts the data into a convenient formats for
use in an R data frame.

## Usage

``` r
# S3 method for class 'wikidiff2'
parse_response(response)

parse_response(response)

# Default S3 method
parse_response(response)

# S3 method for class 'row_list'
parse_response(response)
```

## Arguments

- response:

  The data retrieved from Wikipedia.

## Value

A vector the same length as the response. Generally, this will be a
simple vector, a
[tibble::tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)
or a list of
[tibble::tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)
objects.

## Methods (by class)

- `parse_response(wikidiff2)`: Simplify a wikidiff2 response to a
  dataframe of textual differences, discarding display data

- `parse_response(default)`: By default, create a list of nested tbl_dfs

- `parse_response(row_list)`: Many of the endpoints return a list of
  named values for each page, which can easily be row-bound. They often
  contain nested data, however, which is automatically unnested by
  dplyr::bind_rows. Hence this more basic approach.
