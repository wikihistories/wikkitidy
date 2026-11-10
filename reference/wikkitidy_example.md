# Get path to wikkitidy example

wikkitidy comes bundled with a number of sample files in its
`inst/extdata` directory. This function make them easy to access

## Usage

``` r
wikkitidy_example(file = NULL)
```

## Arguments

- file:

  Name of file. If `NULL`, the example files will be listed.

## Value

A character vector, containing either the path of the chosen file, or
the nicknames of all available example files.

## Examples

``` r
wikkitidy_example()
#> [1] "fatwiki_dump"  "fatwiki_index"
wikkitidy_example("fatwiki_dump")
#> [1] "/home/runner/work/_temp/Library/wikkitidy/extdata/fatwiki-20240920-pages-articles-multistream.xml.bz2"
```
