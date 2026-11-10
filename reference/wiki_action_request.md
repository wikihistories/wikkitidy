# Query Wikipedia using the [MediaWiki Action API](https://www.mediawiki.org/wiki/API:Main_page)

Wikipedia exposes a To build up a query, you first call
`wiki_action_request()` to create the basic request object, then use the
helper functions
[`query_page_properties()`](https://wikihistories.github.io/wikkitidy/reference/query_page_properties.md),
[`query_list_pages()`](https://wikihistories.github.io/wikkitidy/reference/query_list_pages.md)
and
[`query_generate_pages()`](https://wikihistories.github.io/wikkitidy/reference/query_generate_pages.md)
to modify the request, before calling
[`next_batch()`](https://wikihistories.github.io/wikkitidy/reference/get_query_results.md)
or
[`retrieve_all()`](https://wikihistories.github.io/wikkitidy/reference/get_query_results.md)
to perform the query and download results from the server.

## Usage

``` r
wiki_action_request(..., action = "query", language = "en")
```

## Arguments

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Parameters for the request

- action:

  The action to perform, typically 'query'

- language:

  The language edition of Wikipedia to request, e.g. 'en' or 'fr'

## Value

An `action_api` object, an S3 list that subclasses
[httr2::request](https://httr2.r-lib.org/reference/request.html). The
dependencies between different aspects of the Action API are complex. At
the time of writing, there are five major subclasses of
`action_api/httr2_request`:

- `generator/action_api/httr2_request`, returned (sometimes) by
  [query_generate_pages](https://wikihistories.github.io/wikkitidy/reference/query_generate_pages.md)

- `list/action_api/httr2_request`, returned by
  [query_list_pages](https://wikihistories.github.io/wikkitidy/reference/query_list_pages.md)

- `titles`, `pageids` and `revids/action_api/httr2_request`, returned by
  the various
  [query_by\_](https://wikihistories.github.io/wikkitidy/reference/query_by_.md)
  functions

  You can use
  [query_page_properties](https://wikihistories.github.io/wikkitidy/reference/query_page_properties.md)
  to modify any kind of query *except* for `list` queries: indeed, the
  central limitation of the `list` queries is that you cannot choose
  what properties to return for the pages the meet the given criterion.
  The concept of a `generator` is complex. If the `generator` is based
  on a [property](https://www.mediawiki.org/wiki/API:Properties) module,
  then it must be combined with a
  [query_by\_](https://wikihistories.github.io/wikkitidy/reference/query_by_.md)
  function to produce a valid query. If the generator is based on a
  [list module](https://www.mediawiki.org/wiki/API:Lists), then it
  *cannot* be combined with a
  [query_by\_](https://wikihistories.github.io/wikkitidy/reference/query_by_.md)
  query.

## Details

[wikkitidy](https://wikihistories.github.io/wikkitidy/reference/wikkitidy-package.md)
provides an ergonomic API for the Action API's [Query
modules](https://www.mediawiki.org/wiki/API:Query). These modules are
most useful for researchers, because they allow you to explore the
structure of Wikipedia and its back pages. You can obtain a list of
available modules in your R console using
[`list_all_property_modules()`](https://wikihistories.github.io/wikkitidy/reference/query_page_properties.md),
[`list_all_list_modules()`](https://wikihistories.github.io/wikkitidy/reference/query_list_pages.md)
and
[`list_all_generators()`](https://wikihistories.github.io/wikkitidy/reference/query_generate_pages.md),

## See also

[`gracefully()`](https://wikihistories.github.io/wikkitidy/reference/gracefully.md)

## Examples

``` r
# List the first 10 pages in the category 'Australian historians'
historians <- wiki_action_request() %>%
  query_list_pages(
    "categorymembers",
    cmtitle = "Category:Australian_historians",
    cmlimit = 10
  ) %>%
  gracefully(next_batch)
historians
#> <complete/query_tbl>
#> # A tibble: 10 × 3
#>      pageid    ns title                    
#>       <int> <int> <chr>                    
#>  1 72000612     0 Michelle Arrow           
#>  2 74445832     0 Alan Atkinson (historian)
#>  3 84441534     0 Tony Barta               
#>  4 46828642     0 Craig Benjamin           
#>  5 53702558     0 Frank Murcott Bladen     
#>  6 59403076     0 Frank Bongiorno          
#>  7 23093698     0 Tim Bowden               
#>  8 22906640     0 Phillip Bradley          
#>  9 33224431     0 Richard Broome           
#> 10 68287945     0 David Brophy (historian) 
#> ℹ There are more results on the server. Retrieve them with `next_batch()` or `retrieve_all()`
#> ✔ Data complete for all records
```
