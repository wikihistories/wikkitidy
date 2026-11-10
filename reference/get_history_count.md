# Count how many times Wikipedia articles have been edited

Count how many times Wikipedia articles have been edited

## Usage

``` r
get_history_count(
  title,
  type = c("edits", "anonymous", "bot", "editors", "minor", "reverted"),
  from = NULL,
  to = NULL,
  language = "en",
  failure_mode = c("error", "quiet")
)
```

## Arguments

- title:

  A vector of article titles

- type:

  The [type of edit to
  count](https://www.mediawiki.org/wiki/API:REST_API/Reference#Parameters_12)

- from:

  Optional: a vector of revision ids

- to:

  Optional: a vector of revision ids

- language:

  Vector of two-letter language codes for Wikipedia editions

- failure_mode:

  What to do if no data is found. See
  [`get_rest_resource()`](https://wikihistories.github.io/wikkitidy/reference/get_rest_resource.md)

## Value

A
[tibble::tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)
with two columns:

- 'count': integer, the number of edits of the given type

- 'limit': logical, whether the 'count' exceeds the API's limit. Each
  type of edit has a different limit. If the 'count' exceeds the limit,
  then the limit is returned as the count and 'limit' is set to TRUE

## Examples

``` r
# Get the number of edits made by auto-confirmed editors to a page between
# revisions 384955912 and 406217369
get_history_count(
  title="Jupiter",
  type="editors",
  from=384955912,
  to=406217369,
  failure_mode="quiet"
  )
#> # A tibble: 1 × 2
#>   count limit
#>   <int> <lgl>
#> 1    65 FALSE

# Compare which authors have the most edit activity
authors <- tibble::tribble(
  ~author,
  "Jane Austen",
  "William Shakespeare",
  "Emily Dickinson"
) %>%
  dplyr::mutate(get_history_count(author, failure_mode="quiet"))
authors
#> # A tibble: 3 × 3
#>   author              count limit
#>   <chr>               <int> <lgl>
#> 1 Jane Austen          6098 FALSE
#> 2 William Shakespeare 13770 FALSE
#> 3 Emily Dickinson      5804 FALSE
```
