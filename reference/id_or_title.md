# Determine if a page parameter comprises titles or pageids, and prefix accordingly.

Determine if a page parameter comprises titles or pageids, and prefix
accordingly.

## Usage

``` r
id_or_title(page, prefix = NULL)

# S3 method for class 'character'
id_or_title(page, prefix = NULL)

# S3 method for class 'numeric'
id_or_title(page, prefix = NULL)
```

## Arguments

- page:

  Either a character or numeric vector. If a character vector, it is
  interpreted as a vector of page titles. If a numeric vector, of
  pageids.

- prefix:

  Optional: A prefix to affix to the page titles if it is missing

## Value

A list
