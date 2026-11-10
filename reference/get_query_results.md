# Perform a query using the [MediaWiki Action API](https://www.mediawiki.org/wiki/Special:MyLanguage/API:Main_page)

`next_result()` sends exactly one request to the server.

`next_batch()` requests results from the server until data is complete
the latest batch of pages in the result.

`retrieve_all()` keeps requesting data until all the pages from the
query have been returned.

## Usage

``` r
next_result(x)

next_batch(x)

retrieve_all(x)
```

## Arguments

- x:

  The query. Either a
  [wiki_action_request](https://wikihistories.github.io/wikkitidy/reference/wiki_action_request.md)
  or a
  [query_tbl](https://wikihistories.github.io/wikkitidy/reference/query_tbl.md).

## Value

A
[query_tbl](https://wikihistories.github.io/wikkitidy/reference/query_tbl.md)
containing results of the query. If `x` is a
[query_tbl](https://wikihistories.github.io/wikkitidy/reference/query_tbl.md),
then the function will return a new data with the new data appended to
it. If `x` is a
[wiki_action_request](https://wikihistories.github.io/wikkitidy/reference/wiki_action_request.md),
then the returned
[query_tbl](https://wikihistories.github.io/wikkitidy/reference/query_tbl.md)
will contain the necessary data to supply future calls to
`next_result()`, `next_batch()` or `retrieve_all()`.

## Details

It is rare that a query can be fulfilled in a single request to the
server. There are two ways a query can be incomplete. All queries return
a list of pages as their result. The result may be incomplete because
not all the data for each page has been returned. In this case the
*batch* is incomplete. Or the data may be complete for all pages, but
there are more pages available on the server. In this case the query can
be *continued*. Thus the three functions for `next_result()`,
`next_batch()` and `retrieve_all()`.

## Examples

``` r
# Try out a request using next_result(), then retrieve the rest of the
# results. The clllimt limits the first request to 40 results.
preview <- wiki_action_request() %>%
  query_by_title("Steve Wozniak") %>%
  query_page_properties("categories", cllimit = 40) %>%
  gracefully(next_result)
preview
#> <incomplete/query_tbl>
#> # A tibble: 1 × 4
#>   pageid    ns title         categories       
#>    <int> <int> <chr>         <list>           
#> 1  27848     0 Steve Wozniak <tibble [40 × 2]>
#> ℹ There are more results on the server. Retrieve them with `next_batch()` or `retrieve_all()`
#> ! Data not fully downloaded for last batch. Retrieve it with `next_batch()` or `retrieve_all()`.

all_results <- preview %>%
  gracefully(retrieve_all)
all_results
#> <final/query_tbl>
#> # A tibble: 1 × 4
#>   pageid    ns title         categories       
#>    <int> <int> <chr>         <list>           
#> 1  27848     0 Steve Wozniak <tibble [77 × 2]>
#> ✔ All results downloaded from server
#> ✔ Data complete for all records

# tidyr is useful for list-columns.
if (!is_wikkitidy_failure(all_results)) {
  all_results %>%
    tidyr::unnest(cols=c(categories), names_sep = "_")
}
#> # A tibble: 77 × 5
#>    pageid    ns title         categories_ns categories_title                    
#>     <int> <int> <chr>                 <int> <chr>                               
#>  1  27848     0 Steve Wozniak            14 Category:1950 births                
#>  2  27848     0 Steve Wozniak            14 Category:20th-century American busi…
#>  3  27848     0 Steve Wozniak            14 Category:20th-century American engi…
#>  4  27848     0 Steve Wozniak            14 Category:20th-century American inve…
#>  5  27848     0 Steve Wozniak            14 Category:20th-century Polish busine…
#>  6  27848     0 Steve Wozniak            14 Category:20th-century Polish engine…
#>  7  27848     0 Steve Wozniak            14 Category:20th-century atheists      
#>  8  27848     0 Steve Wozniak            14 Category:21st-century American busi…
#>  9  27848     0 Steve Wozniak            14 Category:21st-century American engi…
#> 10  27848     0 Steve Wozniak            14 Category:21st-century American inve…
#> # ℹ 67 more rows
```
