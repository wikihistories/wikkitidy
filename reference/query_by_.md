# Query the [MediaWiki Action API](https://www.mediawiki.org/wiki/API:Main_page) using a vector of Wikipedia pages

These functions help you to build a query for the [MediaWiki Action
API](https://www.mediawiki.org/wiki/API:Main_page) if you already have a
set of pages that you wish to investigate. These functions can be
combined with
[query_page_properties](https://wikihistories.github.io/wikkitidy/reference/query_page_properties.md)
to choose which properties to return for the passed pages.

## Usage

``` r
query_by_title(.req, title)

query_by_pageid(.req, pageid)

query_by_revid(.req, revid)
```

## Arguments

- .req:

  A
  [wiki_action_request](https://wikihistories.github.io/wikkitidy/reference/wiki_action_request.md)
  query to modify

- title:

  A character vector of page titles

- pageid:

  A character or numeric vector of page ids

- revid:

  A character or numeric vector of revision ids

## Value

A request object of type `pages/query/action_api/httr2_request`. To
perform the query, pass the object to
[next_batch](https://wikihistories.github.io/wikkitidy/reference/get_query_results.md)
or
[retrieve_all](https://wikihistories.github.io/wikkitidy/reference/get_query_results.md)

## Details

If you don't already know which pages you wish to examine, you can build
a query to find pages that meet certain criteria using
[query_list_pages](https://wikihistories.github.io/wikkitidy/reference/query_list_pages.md)
or
[query_generate_pages](https://wikihistories.github.io/wikkitidy/reference/query_generate_pages.md).

## See also

[`gracefully()`](https://wikihistories.github.io/wikkitidy/reference/gracefully.md)

## Examples

``` r
# Retrieve the categories for Charles Harpur's Wikipedia page
 resp <- wiki_action_request() %>%
  query_by_title("Charles Harpur") %>%
  query_page_properties("categories") %>%
  gracefully(next_batch)
```
