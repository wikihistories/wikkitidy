# Constructor for the property query type

The intended use for this query is to set the 'titles', 'pageids' or
'revids' parameter, and enforce that only one of these is set. All
[property modules API](https://www.mediawiki.org/wiki/API:Properties) in
the Action API require this parameter to be set, or they require a
[`generator`](https://wikihistories.github.io/wikkitidy/reference/new_generator_query.md)
parameter to be set instead. The `prop/query` type is an abstract type
representing the three possible kinds of property query that do not rely
on a generator (see below on the return value). A complication is that a
`prop/query` can *itself* be used as the basis for a generator.

## Usage

``` r
new_prop_query(.req, by, pages, ...)
```

## Arguments

- .req:

  A
  [`query/action_api/httr2_request`](https://wikihistories.github.io/wikkitidy/reference/wiki_action_request.md)
  object, or a `prop` query object as returned by this function. This
  parameter is covariant on the type, so you can also pass all subtypes
  of `prop`.

- by:

  The type of page. Allowed values are: pageids, titles, revids

- pages:

  A string, the pages to query by, corresponding to the 'by' parameter.
  Multiple values should be separated with "\|"

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Further parameters to the query

## Value

A properly qualified `prop/query` object. There are six possibilities:

- `titles/prop/query`

- `pageids/prop/query`

- `revids/prop/query`

- `generator/titles/prop/query`

- `generator/pageids/prop/query`

- `generator/revids/prop/query`

## Examples

``` r
# Build a query on a set of pageids
# 963273 and 1159171 are Kate Bush albums
bush_albums_query <- wiki_action_request() %>%
  new_prop_query("pageids", "963273|1159171")
```
