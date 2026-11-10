# Constructor for [list](https://www.mediawiki.org/wiki/API:Lists) queries

This low-level constructor only performs basic type checking.

## Usage

``` r
new_list_query(.req, list, ...)

# S3 method for class 'list'
new_list_query(.req, list, ...)

# S3 method for class 'generator'
new_list_query(.req, list, ...)

# S3 method for class 'prop'
new_list_query(.req, list, ...)

# S3 method for class 'query'
new_list_query(.req, list, ...)
```

## Arguments

- .req:

  A
  [`query/action_api/httr2_request`](https://wikihistories.github.io/wikkitidy/reference/wiki_action_request.md)
  object, or a `list/query/action_api/httr2_request` as returned by this
  function.

- list:

  The [list module](https://www.mediawiki.org/wiki/API:Lists) to add to
  the query

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Parameters to the list module

## Value

An object of type `list/query/action_api/httr2_request`.

## Examples

``` r
# Create a query to list all members of Category:Physics
physics_query <- wiki_action_request() %>%
  new_list_query("categorymembers", cmtitle="Category:Physics")
```
