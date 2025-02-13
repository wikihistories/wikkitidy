
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wikkitidy <a href="https://wikihistories.github.io/wikkitidy/"><img src="man/figures/logo.png" align="right" height="100" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/wikihistories/wikkitidy/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/wikihistories/wikkitidy/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/wikihistories/wikkitidy/branch/main/graph/badge.svg)](https://app.codecov.io/gh/wikihistories/wikkitidy?branch=main)
[![DOI](https://zenodo.org/badge/597954751.svg)](https://zenodo.org/doi/10.5281/zenodo.10638610)
<!-- badges: end -->

Tidy analysis of Wikipedia in R

## What’s in a name?

**wiki**: There are many wikis, but one dominates the Wikiverse.
[Wikipedia](https://www.wikipedia.org/) is the largest repository of
facts ever assembled by human hands. Scholars the world over are turning
to Wikipedia to understand how twenty-first century society understands
itself.

**quiddity**: [The ‘whatness’ of a
thing](https://en.wikipedia.org/wiki/Quiddity). The kind of thing it is.
[What is Wikipedia?](https://en.wikipedia.org/wiki/Wikipedia:NOT) Is it
merely another encyclopaedia? It is news presented as history? Is it the
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

| Version | Feature | Done? |
|----|----|----|
| 0.1 | Basic request objects | :white_check_mark: |
| 0.2 | Calls and response objects for [Core](https://www.mediawiki.org/wiki/API:REST_API) and [Wikimedia](https://www.mediawiki.org/wiki/Wikimedia_REST_API) REST APIs | :white_large_square: |
| 0.3 | Calls and response objects for [MediaWiki Action API Query Modules](https://www.mediawiki.org/wiki/API:Query) | :white_large_square: |
| 0.4 | Interface to Wikipedia XML dumps | :white_large_square: |
| 0.5 | Implementation of [Wikiblame](https://github.com/FlominatorTM/wikiblame) | :white_large_square: |
| 0.6 | Calls and response objects for the [XTools](https://www.mediawiki.org/wiki/XTools/API) and [WikiMedia](https://wikimedia.org/api/rest_v1/) APIs | :white_large_square: |

## Installation

You can install wikkitidy from CRAN with:

    install.packages("wikkitidy")

You can install the development version from Github with:

    devtools::install_github("wikihistories/wikkitidy")

ur \## Code of Conduct

Please note that the wikkitidy project is released with a [Contributor
Code of
Conduct](https://wikihistories.github.io/wikkitidy/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
