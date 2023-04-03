
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wikkitidy

<!-- badges: start -->

[![R-CMD-check](https://github.com/wikihistories/wikkitidy/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/wikihistories/wikkitidy/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/wikihistories/wikkitidy/branch/main/graph/badge.svg)](https://app.codecov.io/gh/wikihistories/wikkitidy?branch=main)
<!-- badges: end -->

Tidy analysis of Wikipedia in R

## What’s in a name?

**wiki**: There are many wikis, but one dominates the Wikiverse.
[Wikipedia](https://wikipedia.org) is the largest repository of facts
ever assembled by human hands. Scholars the world over are turning to
Wikipedia to understand how twenty-first century society understands
itself.

**quiddity**: [The ‘whatness’ of a
thing](https://wikipedia.org/wiki/Quiddity). The kind of thing it is.
[What is Wikipedia?](https://wikipedia.org/wiki/WP:NOT) Is it merely
another encyclopaedia? It is news presented as history? Is it the
consensus of a global village, or the battleground of an ideological
war?

**tidy**: The best kind of data. R programmers are lucky to have access
to the [tidyverse](https://joss.theoj.org/papers/10.21105/joss.01686), a
collection of packages that make it easy to analyse, visualise and
publish data. This package embodies [tidy data
principles](https://www.jstatsoft.org/article/view/v059i10) by returning
results from Wikipedia’s APIs as tibbles or simple vectors, and by
providing a number of vectorised analysis functions that can be applied
reliably and without fuss to the data you retrieve.

Thus `wikkitidy`’s aim: to help you work out what Wikipedia is with
minimal data wrangling and cleaning.

## Getting to 1.0

| Version | Feature                                                                                                                        | Done?                |
|---------|--------------------------------------------------------------------------------------------------------------------------------|----------------------|
| 0.1     | Basic request objects                                                                                                          | :white_check_mark:   |
| 0.2     | Calls and response objects for [Core REST API](https://www.mediawiki.org/wiki/API:REST_API)                                    | :white_large_square: |
| 0.3     | Calls and response objects for [MediaWiki Action API Query Modules](https://www.mediawiki.org/wiki/API:Query)                  | :white_large_square: |
| 0.4     | Interface to \[Wikipedia XML dumps\]                                                                                           | :white_large_square: |
| 0.5     | Implementation of [Wikiblame](https://github.com/FlominatorTM/wikiblame)                                                       | :white_large_square: |
| 0.6     | Calls and response objects for [Wikimedia REST API](https://www.mediawiki.org/wiki/Wikimedia_REST_API) (if they are relevant…) | :white_large_square: |

## Installation

You can install the development version of wikkitidy from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("wikihistories/wikkitidy")
```
