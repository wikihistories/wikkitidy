# Access page-level statistics from the [XTools Page API endpoint](https://www.mediawiki.org/wiki/XTools/API/Page)

`get_xtools_page_info()` returns [basic
statistics](https://www.mediawiki.org/wiki/XTools/API/Page#Article_info)
about articles' history and quality, including their total edits,
creation date, and assessment value (good, featured etc.)

`get_xtools_page_prose()` returns [statistics about the word counts and
referencing](https://www.mediawiki.org/wiki/XTools/API/Page#Prose) of
articles

`get_xtools_page_links()` returns [the number of ingoing and outgoing
links to articles, including
redirects](https://www.mediawiki.org/wiki/XTools/API/Page#Links)

`get_xtools_page_top_editors()` returns the [list of top editors for
articles](https://www.mediawiki.org/wiki/XTools/API/Page#Top_editors),
with optional filters by date range and non-bot status

`get_xtools_page_assessment()` returns more detailed [statistics about
articles' assessment status and Wikiproject importance
levels](https://www.mediawiki.org/wiki/XTools/API/Page#Assessments)

## Usage

``` r
get_xtools_page_info(title, language = "en", failure_mode = "error")

get_xtools_page_prose(
  title,
  language = "en",
  failure_mode = c("error", "quiet")
)

get_xtools_page_links(
  title,
  language = "en",
  failure_mode = c("error", "quiet")
)

get_xtools_page_top_editors(
  title,
  start = NULL,
  end = NULL,
  limit = 1000,
  nobots = FALSE,
  language = "en",
  failure_mode = c("error", "quiet")
)

get_xtools_page_assessment(
  title,
  classonly = FALSE,
  language = "en",
  failure_mode = c("error", "quiet")
)
```

## Arguments

- title:

  Character vector of page titles

- language:

  Language code for the version of Wikipedia to query

- failure_mode:

  What to do if no data is found. See
  [`get_rest_resource()`](https://wikihistories.github.io/wikkitidy/reference/get_rest_resource.md)

- start:

  A character vector or date object (optional): the start date for
  calculating top editors

- end:

  A character vector or date object (optional): the end date for
  calculating top editors

- limit:

  An integer: the maximum number of top editors to return

- nobots:

  TRUE or FALSE: if TRUE, bots are excluded from the top editor
  calculation

- classonly:

  TRUE or FALSE: if TRUE, only return the article's assessment status,
  without Wikiproject information

## Value

A list or tbl of results, the same length as `title`. **NB:** The
results for `get_xtools_page_assessment` are still not parsed properly.

## Examples

``` r
# Get basic statistics about Erich Auerbach on German Wikipedia
auerbach <- get_xtools_page_info("Erich Auerbach", language = "de", failure_mode = "quiet")
auerbach
#> # A tibble: 1 × 17
#>   project page  watchers pageviews pageviews_offset revisions editors anon_edits
#>   <chr>   <chr> <lgl>        <int>            <int>     <int>   <int>      <int>
#> 1 de.wik… Eric… NA             539               30       455     153         42
#> # ℹ 9 more variables: minor_edits <int>, creator <chr>,
#> #   creator_editcount <lgl>, created_at <chr>, created_rev_id <int>,
#> #   modified_at <chr>, secs_since_last_edit <int>, modified_rev_id <int>,
#> #   elapsed_time <dbl>
```
