# Get resources from one of Wikipedia's [two REST APIs](https://www.mediawiki.org/wiki/API)

This function is intended for developer use. It makes it easy to quickly
generate vectorised calls to the different APIs.

## Usage

``` r
get_rest_resource(
  ...,
  language = "en",
  api = c("core", "wikimedia", "wikimedia_org", "xtools"),
  response_format = c("json", "html"),
  response_type = NULL,
  failure_mode = c("error", "quiet")
)
```

## Arguments

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  The URL components and query parameters of the desired resources.
  Names of the arguments are ignored. The function follows the
  [tidyverse vector recycling
  rules](https://vctrs.r-lib.org/reference/vector_recycling_rules), so
  all vectors must have the same length or be of length one. Unnamed
  arguments will be appended to the URL path; named arguments will be
  added as query parameters

- language:

  Character vector of two-letter language codes

- api:

  The desired REST api:
  "[core](https://www.mediawiki.org/wiki/API:REST_API)",
  "[wikimedia](https://www.mediawiki.org/wiki/Wikimedia_REST_API)",
  "[wikimedia_org](https://wikimedia.org/api/rest_v1/)", or
  "[xtools](https://www.mediawiki.org/wiki/XTools/API)"

- response_format:

  The expected Content-Type of the response. Currently "html" and "json"
  are supported.

- response_type:

  The schema of the response. If supplied, the results will be parsed
  using the schema.

- failure_mode:

  How to respond if a request fails "error", the default: raise an error
  "quiet", don't raise an error; instead return a placeholder for each
  failed request, preserving the number of rows. HTTP errors record the
  status in `error_code`. Connection failures (no response) record
  `error_code = NA` and the condition message in `error_message`.

## Value

A list of responses. If `response_format` == "json", then the responses
will be simple R lists. If `response_format` == "html", then the
responses will `xml_document` objects. If `response_type` is supplied,
the response will be coerced into a
[tibble::tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)
or vector using the relevant schema. If the response is a 'scalar list'
(i.e. a list of length == 1), then it is silently unlisted, returning a
simple list or vector.

## Details

The key invariant to maintain is the number of rows. Users ought to be
able to use this function with
[dplyr::mutate](https://dplyr.tidyverse.org/reference/mutate.html),
which requires the number of rows to be invariant.
