# Explore Wikipedia's category system

These functions provide access to the
[CategoryMembers](https://www.mediawiki.org/wiki/API:Categorymembers)
endpoint of the Action API.

`query_category_members()` builds a [generator
query](https://wikihistories.github.io/wikkitidy/reference/query_generate_pages.md)
to return the members of a given category.

`build_category_tree()` finds all the pages and subcategories beneath
the passed category, then recursively finds all the pages and
subcategories beneath them, until it can find no more subcategories.

## Usage

``` r
query_category_members(
  .req,
  category,
  namespace = NULL,
  type = c("file", "page", "subcat"),
  limit = 10,
  sort = c("sortkey", "timestamp"),
  dir = c("ascending", "descending", "newer", "older"),
  start = NULL,
  end = NULL,
  language = "en"
)

build_category_tree(category, language = "en")
```

## Arguments

- .req:

  A [query request
  object](https://wikihistories.github.io/wikkitidy/reference/wiki_action_request.md)

- category:

  The category to start from. `query_category_members()` accepts either
  a numeric pageid or the page title. `build_category_tree()` accepts a
  vector of page titles.

- namespace:

  Only return category members from the provided namespace

- type:

  Alternative to `namespace`: the type of category member to return.
  Multiple types can be requested using a character vector. Defaults to
  all.

- limit:

  The number to return each batch. Max 500.

- sort:

  How to sort the returned category members. 'timestamp' sorts them by
  the date they were included in the category; 'sortkey' by the category
  member's unique hexadecimal code

- dir:

  The direction in which to sort them

- start:

  If `sort` == 'timestamp', only return category members from after this
  date. The argument is parsed by
  [`lubridate::as_date()`](https://lubridate.tidyverse.org/reference/as_date.html)

- end:

  If `sort` == 'timestamp', only return category members included in the
  category from before this date. The argument is parsed by
  [`lubridate::as_date()`](https://lubridate.tidyverse.org/reference/as_date.html)

- language:

  The language edition of Wikipedia to query

## Value

`query_category_members()`: A request object of type
`generator/query/action_api/httr2_request`, which can be passed to
[`next_batch()`](https://wikihistories.github.io/wikkitidy/reference/get_query_results.md)
or
[`retrieve_all()`](https://wikihistories.github.io/wikkitidy/reference/get_query_results.md).
You can specify which properties to retrieve for each page using
[`query_page_properties()`](https://wikihistories.github.io/wikkitidy/reference/query_page_properties.md).

`build_category_tree()`: A list containing two dataframes. `nodes` lists
all the subcategories and pages found underneath the passed categories.
`edges` records the connections between them. The `source` column gives
the pageid of the parent category, while the `target` column gives the
pageid of any categories, pages or files contained within the `source`
category. The `timestamp` records the moment when the `target` page or
subcategory was included in the `source` category. The two dataframes in
the list can be passed to
[igraph::graph_from_data_frame](https://r.igraph.org/reference/graph_from_data_frame.html)
for network analysis.

## See also

[`gracefully()`](https://wikihistories.github.io/wikkitidy/reference/gracefully.md)

## Examples

``` r
# Get the first 10 pages in 'Category:Physics' on English Wikipedia
physics_members <- wiki_action_request() %>%
  query_category_members("Physics") %>%
  gracefully(next_batch)
physics_members
#> <complete/query_tbl>
#> # A tibble: 10 × 3
#>      pageid    ns title                                                        
#>       <int> <int> <chr>                                                        
#>  1    22939     0 Physics                                                      
#>  2  1653925   100 Portal:Physics                                               
#>  3  5103597     0 Jacchia Reference Atmosphere                                 
#>  4 74985603     0 Edge states                                                  
#>  5 76868734     0 HUN-REN Wigner Research Centre for Physics                   
#>  6 78147827     0 Electrostatic solitary wave                                  
#>  7 80064998     0 Frenesy (physics)                                            
#>  8 80358385     0 Atominstitute                                                
#>  9 81547742     0 Large Enriched Germanium Experiment for Neutrinoless BB Decay
#> 10 82429008     0 AIC Judd Award                                               
#> ℹ There are more results on the server. Retrieve them with `next_batch()` or `retrieve_all()`
#> ✔ Data complete for all records


# Build the tree of all albums for the Melbourne band Custard
tree <- gracefully("Category:Custard_(band)_albums", build_category_tree)
#> ⠙ Walking subcategories: 1 done (501/s) | 3ms
#> ⠹ Walking subcategories: 2 done (23/s) | 89ms
tree
#> $nodes
#> # A tibble: 12 × 4
#>      pageid    ns title                                      type  
#>       <int> <int> <chr>                                      <chr> 
#>  1 41181643    14 Category:Custard_(band)_albums             root  
#>  2 47888836     0 Come Back, All Is Forgiven                 page  
#>  3 59271122     0 The Common Touch (album)                   page  
#>  4 30333352     0 Loverama                                   page  
#>  5 63691299     0 Respect All Lifeforms                      page  
#>  6 77627299     0 Suburban Curtains                          page  
#>  7 43770191     0 Wahooti Fandango                           page  
#>  8 30333401     0 We Have the Technology                     page  
#>  9 43769837     0 Wisenheimer                                page  
#> 10 41148700    14 Category:Custard (band) compilation albums subcat
#> 11 43770688     0 Brisbane 1990–1993                         page  
#> 12 43770872     0 Goodbye Cruel World (Custard album)        page  
#> 
#> $edges
#> # A tibble: 11 × 3
#>      source   target timestamp           
#>       <int>    <int> <chr>               
#>  1 41181643 47888836 2025-06-17T05:37:58Z
#>  2 41181643 59271122 2025-06-17T09:23:31Z
#>  3 41181643 30333352 2025-06-17T00:00:43Z
#>  4 41181643 63691299 2025-06-17T10:51:40Z
#>  5 41181643 77627299 2025-06-17T15:29:31Z
#>  6 41181643 43770191 2025-06-17T04:23:08Z
#>  7 41181643 30333401 2025-06-17T00:00:43Z
#>  8 41181643 43769837 2025-06-17T04:23:07Z
#>  9 41181643 41148700 2025-06-17T03:32:15Z
#> 10 41148700 43770688 2025-06-17T04:23:08Z
#> 11 41148700 43770872 2025-06-17T04:23:09Z
#> 

# For network analysis and visualisation, you can pass the category tree
# to igraph
if (!is_wikkitidy_failure(tree)) {
  tree_graph <- igraph::graph_from_data_frame(tree$edges, vertices = tree$nodes)
  tree_graph
}
#> IGRAPH c032269 DN-B 12 11 -- 
#> + attr: name (v/c), ns (v/n), title (v/c), type (v/c), timestamp (e/c)
#> + edges from c032269 (vertex names):
#>  [1] 41181643->47888836 41181643->59271122 41181643->30333352 41181643->63691299
#>  [5] 41181643->77627299 41181643->43770191 41181643->30333401 41181643->43769837
#>  [9] 41181643->41148700 41148700->43770688 41148700->43770872
```
