# Get data about pages from their titles

`get_latest_revision()` returns metadata about the latest revision of
each page.

`get_page_html()` returns the rendered html for each page.

`get_page_summary()` returns metadata about the latest revision, along
with the page description and a summary extracted from the opening
paragraph

`get_page_related()` returns summaries for 20 related pages for each
passed page

`get_page_talk()` returns structured talk page content for each title.
You must ensure to use the title for the Talk page itself, e.g.
"Talk:Earth" rather than "Earth"

`get_page_langlinks()` returns interwiki links for each title

## Usage

``` r
get_latest_revision(title, language = "en", failure_mode = "error")

get_page_html(title, language = "en", failure_mode = "error")

get_page_summary(title, language = "en", failure_mode = "error")

get_page_talk(title, language = "en", failure_mode = "error")

get_page_langlinks(title, language = "en", failure_mode = "error")
```

## Arguments

- title:

  A character vector of page titles.

- language:

  A character vector of two-letter language codes, either of length 1 or
  the same length as `title`

- failure_mode:

  Either "quiet" or "error." See
  [`get_rest_resource()`](https://wikihistories.github.io/wikkitidy/reference/get_rest_resource.md)

## Value

A list, vector or tibble, the same length as `title`, with the desired
data.

## Examples

``` r
# Get language links for a known page on English Wikipedia
get_page_langlinks("Charles Harpur", failure_mode = "quiet")
#> # A tibble: 2 × 4
#>   code  name     key            title         
#>   <chr> <chr>    <chr>          <chr>         
#> 1 de    Deutsch  Charles_Harpur Charles Harpur
#> 2 fr    français Charles_Harpur Charles Harpur

# The functions are vectorised over title and language
# Find all articles about Joanna Baillie, and retrieve summary data for
# the first two.
baillie <- get_page_langlinks("Joanna Baillie", failure_mode = "quiet") %>%
  dplyr::slice(1:2) %>%
  dplyr::mutate(get_page_summary(title = title, language = code, failure_mode = "quiet"))
baillie
#> # A tibble: 2 × 30
#>   code  name    key        title  type  displaytitle namespace_id namespace_text
#>   <chr> <chr>   <chr>      <chr>  <chr> <chr>               <int> <chr>         
#> 1 ar    العربية جوانا_بيلي جوانا… stan… "<span lang…            0 ""            
#> 2 arz   مصرى    جوانا_بيلى جوانا… stan… "<span lang…            0 ""            
#> # ℹ 22 more variables: wikibase_item <chr>, titles_canonical <chr>,
#> #   titles_normalized <chr>, titles_display <chr>, pageid <int>,
#> #   thumbnail_source <chr>, thumbnail_width <int>, thumbnail_height <int>,
#> #   originalimage_source <chr>, originalimage_width <int>,
#> #   originalimage_height <int>, lang <chr>, dir <chr>, revision <chr>,
#> #   tid <chr>, timestamp <chr>, description <chr>, description_source <chr>,
#> #   content_urls_desktop <list>, content_urls_mobile <list>, extract <chr>, …
```
