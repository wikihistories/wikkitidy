# Constructor for generator query type

Construct a new query to a [generator
module](https://www.mediawiki.org/wiki/API:Query#Example_6:_Generators)
of the Action API. This low-level constructor only performs basic
type-checking. It is your responsibility to ensure that the chosen
`generator` is an existing API endpoint, and that you have composed the
query correctly. For a more user-friendly interface, use
[query_generate_pages](https://wikihistories.github.io/wikkitidy/reference/query_generate_pages.md).

## Usage

``` r
new_generator_query(.req, generator, ...)
```

## Arguments

- .req:

  A
  [`query/action_api/httr2_request`](https://wikihistories.github.io/wikkitidy/reference/wiki_action_request.md)
  object, or a generator query as returned by this function.

- generator:

  The generator to add to the query. If the generator is based on a
  [property module](https://www.mediawiki.org/wiki/API:Properties), then
  `.req` must be a subtype of
  [`prop/query/action_api/httr2_request`](https://wikihistories.github.io/wikkitidy/reference/new_prop_query.md).
  If the generator is based on a [list
  module](https://www.mediawiki.org/wiki/API:Lists), then `.req` must
  subclass
  [`query/action_api/httr2_request`](https://wikihistories.github.io/wikkitidy/reference/wiki_action_request.md)
  directly.

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Further parameters to the generator

## Value

The output type depends on the input. If `.req` is a
[`query/action_api/httr2_request`](https://wikihistories.github.io/wikkitidy/reference/wiki_action_request.md),
then the output will be a `generator/query/action_api/httr2_request`. If
`.req` is a
[`prop/query/action_api/httr2_request`](https://wikihistories.github.io/wikkitidy/reference/new_prop_query.md),
then the return object will be a subclass of the passed request, with
"generator" as the first term in the class vector, i.e.
`generator/(titles|pageids|revids)/prop/query/action_api/httr2_request`.

## Examples

``` r
# Build a generator query using a list module
# List all members of Category:Physics on English Wikipedia
physics <- wiki_action_request() %>%
  new_generator_query("categorymembers", gcmtitle = "Category:Physics")

# Build a generator query on a property module
# Generate the pages that are linked to Albert Einstein's page on English
# Wikipedia
einstein_categories <- wiki_action_request() %>%
  new_prop_query("titles", "Albert Einstein") %>%
  new_generator_query("iwlinks")
```
