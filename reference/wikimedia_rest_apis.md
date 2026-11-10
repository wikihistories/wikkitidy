# Build a REST request to one of the Wikimedia Foundation's central APIs

`wikimedia_org_rest_request()` builds a request for the [wikimedia.org
REST API](https://wikimedia.org/api/rest_v1/), which provides
statistical data about Wikimedia Foundation projects

`xtools_rest_request()` builds a request to the [XTools
API](https://www.mediawiki.org/wiki/XTools/API), which provides
additional statistical data about Wikimedia foundation projects

## Usage

``` r
wikimedia_org_rest_request(endpoint, ..., language = "en")

xtools_rest_request(endpoint, ..., language = "en")
```

## Arguments

- endpoint:

  The endpoint for the specific kind of request; for wikimedia apis,
  this comprises the path components in between the general API endpoint
  and the component specifying the project to query

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Components to add to the URL. Unnamed arguments are added to the path
  of the request, while named arguments are added as query parameters.

- language:

  Two-letter language code for the desired Wikipedia edition.

## Value

A `wikimedia_org/rest` or `xtools/rest` object, an S3 vector that
subclasses
[httr2::request](https://httr2.r-lib.org/reference/request.html).

## Examples

``` r
# Build request for articleinfo about Kate Bush's page on English Wikipedia
request <- xtools_rest_request("page/articleinfo", "Kate_Bush")

# Build request for most-viewed pages on German Wikipedia in July 2020
request <- wikimedia_org_rest_request(
    "metrics/pageviews/top",
    "all-access", "2020", "07", "all-days",
    language = "de"
    )
```
