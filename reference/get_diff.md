# Search for insertions, deletions or relocations of text between two versions of a Wikipedia page

Any two revisions of a Wikipedia page can be compared using the 'diff'
tool. The tool compares the 'from' revision to the 'to' revision,
looking for insertions, deletions or relocations of text. This operation
can be performed in any order, across any span of revisions.

## Usage

``` r
get_diff(from, to, language = "en", simplify = TRUE, failure_mode = "error")
```

## Arguments

- from:

  Vector of revision ids

- to:

  Vector of revision ids

- language:

  Vector of two-letter language codes (will be recycled if length==1)

- simplify:

  logical: should R simplify the result (see
  [return](https://rdrr.io/r/base/function.html))

- failure_mode:

  Either "quiet" or "error." See
  [`get_rest_resource()`](https://wikihistories.github.io/wikkitidy/reference/get_rest_resource.md)

## Value

The return value depends on the `simplify` parameter.

- If `simplify` == TRUE: A list of
  [tibble::tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)
  objects the same length as `from` and `to`. Most of the response data
  is stripped away, leaving just the textual differences between the
  revisions, their location, type and 'highlightRanges' if the textual
  differences are complicated.

- If `simplify` == FALSE: A list the same length as `from` and `to`
  containing the full [wikidiff2
  response](https://www.mediawiki.org/wiki/API:REST_API/Reference#Response_schema_3)
  for each pair of revisions. This response includes additional data for
  displaying diffs onscreen.

## Examples

``` r
# Compare revision 847170467 to 851733941 on English Wikipedia
get_diff(847170467, 851733941, failure_mode = "quiet")
#> # A tibble: 2 × 5
#>    type lineNumber text                                    offset_from offset_to
#>   <int>      <int> <chr>                                         <int>     <int>
#> 1     1         97 ""                                               NA     15633
#> 2     1         98 "In 2016, a new species of [[Cecidomyi…          NA     15634

# The function is vectorised, so you can compare multiple pairs of revisions
# in a single call
# See diffs for the last two revisions of the Main Page
revisions <- wiki_action_request() %>%
  query_by_title("Main Page") %>%
  query_page_properties(
    "revisions",
    rvlimit = 2, rvprop = "ids", rvdir = "older"
  ) %>%
  gracefully(next_result)

if (!is_wikkitidy_failure(revisions)) {
  revisions <- revisions %>%
    tidyr::unnest(cols = c(revisions)) %>%
    dplyr::mutate(diffs = get_diff(
      from = parentid, to = revid, failure_mode = "quiet"
    ))

  print(revisions)
}
#> # A tibble: 2 × 6
#>     pageid    ns title          revid   parentid diffs           
#>      <int> <int> <chr>          <int>      <int> <list>          
#> 1 15580374     0 Main Page 1383522990 1380977360 <tibble [1 × 6]>
#> 2 15580374     0 Main Page 1380977360 1380834536 <tibble [2 × 6]>
```
