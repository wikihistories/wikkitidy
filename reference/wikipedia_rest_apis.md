# Build a REST request to one of Wikipedia's specific REST APIs

`core_request_request()` builds a request for the [MediaWiki Core REST
API](https://www.mediawiki.org/wiki/API:REST_API), the basic REST API
available on all MediaWiki wikis.

`wikimedia_rest_request()` builds a request for the [Wikimedia REST
API](https://www.mediawiki.org/wiki/Wikimedia_REST_API), an additional
api just for Wikipedia and other wikis managed by the Wikimedia
Foundation

## Usage

``` r
core_rest_request(..., language = "en")

wikimedia_rest_request(..., language = "en")
```

## Arguments

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Components to add to the URL. Unnamed arguments are added to the path
  of the request, while named arguments are added as query parameters.

- language:

  The two-letter language code for the Wikipedia edition

## Value

A `core/rest`, `wikimedia/rest`, object, an S3 vector that subclasses
`httr2_request` (see
[httr2::request](https://httr2.r-lib.org/reference/request.html)). The
request needs to be passed to
[httr2::req_perform](https://httr2.r-lib.org/reference/req_perform.html)
to retrieve data from the API.

## Examples

``` r
# Get the html of the 'Earth' article on English Wikipedia
response <- core_rest_request("page", "Earth", "html") %>%
  gracefully(httr2::req_perform)

response <- wikimedia_rest_request("page", "html", "Earth") %>%
  gracefully(httr2::req_perform)

# Some REST requests take query parameters. Pass these as named arguments.
# To search German Wikipedia for articles about Goethe
response <- core_rest_request("search/page", q = "Goethe", limit = 2, language = "de") %>%
  gracefully(httr2::req_perform)
if (!is_wikkitidy_failure(response)) {
  httr2::resp_body_json(response)
}
#> $pages
#> $pages[[1]]
#> $pages[[1]]$id
#> [1] 2501
#> 
#> $pages[[1]]$key
#> [1] "Johann_Wolfgang_von_Goethe"
#> 
#> $pages[[1]]$title
#> [1] "Johann Wolfgang von Goethe"
#> 
#> $pages[[1]]$excerpt
#> [1] "Johann Wolfgang <span class=\"searchmatch\">Goethe</span>, ab 1782 von <span class=\"searchmatch\">Goethe</span> (* 28. August 1749 in Frankfurt am Main; † 22. März 1832 in Weimar), war ein deutscher Dichter, Politiker und"
#> 
#> $pages[[1]]$matched_title
#> NULL
#> 
#> $pages[[1]]$anchor
#> NULL
#> 
#> $pages[[1]]$description
#> [1] "deutscher Dichter, Dramatiker, Naturforscher und Politiker (1749–1832)"
#> 
#> $pages[[1]]$thumbnail
#> $pages[[1]]$thumbnail$mimetype
#> [1] "image/jpeg"
#> 
#> $pages[[1]]$thumbnail$width
#> [1] 60
#> 
#> $pages[[1]]$thumbnail$height
#> [1] 74
#> 
#> $pages[[1]]$thumbnail$duration
#> NULL
#> 
#> $pages[[1]]$thumbnail$url
#> [1] "//thumb.wikimedia.org/wikipedia/commons/thumb/0/0e/Goethe_%28Stieler_1828%29.jpg/60px-Goethe_%28Stieler_1828%29.jpg?utm_source=de.wikipedia.org&utm_campaign=rest&utm_content=thumbnail"
#> 
#> 
#> 
#> $pages[[2]]
#> $pages[[2]]$id
#> [1] 5731837
#> 
#> $pages[[2]]$key
#> [1] "Goethe!"
#> 
#> $pages[[2]]$title
#> [1] "Goethe!"
#> 
#> $pages[[2]]$excerpt
#> [1] "Bis Ende 2011 erreichte <span class=\"searchmatch\">Goethe</span>! in Deutschland über 740.000 Kinobesucher. In Straßburg fällt der Jura-Student Johann <span class=\"searchmatch\">Goethe</span> durch das Staatsexamen. In"
#> 
#> $pages[[2]]$matched_title
#> NULL
#> 
#> $pages[[2]]$anchor
#> NULL
#> 
#> $pages[[2]]$description
#> [1] "Film von Philipp Stölzl (2010)"
#> 
#> $pages[[2]]$thumbnail
#> NULL
#> 
#> 
#> 
```
