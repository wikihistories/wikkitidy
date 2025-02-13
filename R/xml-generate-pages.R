# Iterate over Wikipedia pages in an XML dump of the database
#
# The Wikipedia xml dumps are very large, and use a complex 'multistream' compression
# format. This function allows you to treat the XML file as though it were a vector
# of XML pages. It iterates 'lazily' over the pages, loading them 100-at-time
# from the file. This prevents the data from overflowing the memory.
#
# If you are working with a large wiki, such as English or German Wikipedia, then
# the articles will easily exceed the memory of most machines. You should use
# this function in conjunction with [xml_filter_pages()] to only keep a subset
# of pages in memory. When you have added your filter to the pipeline, use
# [xml_collect_pages()] to pull your chosen pages into a tibble.
#
# You can download xml files from the [Wikimedia downloads](https://dumps.wikimedia.org/)
# page.
#
#
# @param pages_file A string or file handle to the XML file of page dumps
#
# @return An [coro::generator] of Wikipedia pages, represented as R lists with
# the following keys: `title`, `ns`, `pageid`, `revisionid`, `text`.
#
# @keywords internal
xml_iterate_pages <- function(pages_file) {

  lines <- generate_lines(pages_file)
  pages <- generate_pages(lines)

  return(pages)

}

generate_lines <- coro::generator(function(pages_file) {
  con <- file(pages_file)
  # Explicitly open con so that it stays open while
  # function is suspended.
  open(con)
  on.exit(close(con))
  is_open <- TRUE
  while (is_open) {
    # Setting ok to TRUE in readLines throws an error
    # at the end of the file. We can catch this error
    # to terminate the generator and close the connection
    tryCatch(
      {
        next_line <- readLines(con, n = 1, ok = FALSE, warn = FALSE)
        coro::yield(next_line)
      },
      simpleError = function(e) {
        # Super-assign to is_open so it breaks the while loop
        is_open <<- FALSE
      }
    )
  }
})

generate_pages <- coro::generator(function(line_generator) {

  in_page_meta <- FALSE # Are we in the metadata node for a page?
  in_revision <- FALSE # Are we in the revision node?
  in_text <- FALSE # Are we in the text node?
  text_idx <- 1 # Which line of text are we up to?
  current_page <- new_page()

  for (line in line_generator) {

    # Detect start of page
    if (stringr::str_detect(line, "^\\s*<page>")) {
      in_page_meta <- TRUE
    }

    # Extract page metadata
    if (in_page_meta) {
      if (stringr::str_detect(line, "^\\s*<title>")) {
        current_page$title <- stringr::str_extract(line, "^\\s*<title>(.+)</title>", group = 1)
      } else if (stringr::str_detect(line, "^\\s*<ns>")) {
        current_page$ns <- stringr::str_extract(line, "^\\s*<ns>(.+)</ns>", group = 1)
      } else if (stringr::str_detect(line, "^\\s*<id>")) {
        current_page$pageid <- stringr::str_extract(line, "^\\s*<id>(.+)</id>", group = 1)
      } else if (stringr::str_detect(line, "^\\s*<revision>")) {
        in_page_meta <- FALSE
        in_revision <- TRUE
      }
    }

    # Extract current revision metadata
    if (in_revision) {
      if (stringr::str_detect(line, "^\\s*<id>")) {
        current_page$revisionid <- stringr::str_extract(line, "^\\s*<id>(.+)</id>", group = 1)
      } else if (stringr::str_detect(line, "^\\s*<text")) {
        in_revision <- FALSE
        in_text <- TRUE
      }
    }

    # Extract text
    if (in_text) {

      # Extend container if needed
      if (text_idx > length(current_page$text)) {
        current_page$text <- append(current_page$text, vector("list", 1000))
      }

      # Add next text and increment counter
      current_page$text[text_idx] <- line
      text_idx <- text_idx + 1

      # Stop counting if at the end
      if (stringr::str_detect(line, "</text>")) {
        in_text <- FALSE
        text_idx <- 1
      }
    }

    # Detect end of page
    if (stringr::str_detect(line, "^\\s*</page>")) {
      # Discard empty slots from container
      current_page$text <- purrr::discard(current_page$text, is.null)
      # Concatenate text into single string
      current_page$text <- paste(current_page$text, collapse = "\n")
      # Strip <text> tags
      current_page$text <- stringr::str_remove_all(current_page$text, "\\s*</?text[^>]*>")
      # Yield
      coro::yield(current_page)
      # Generate new container
      current_page <- new_page()
    }
  }
})

new_page <- function() {
  list(
    title = NULL,
    ns = NULL,
    pageid = NULL,
    revisionid = NULL,
    # To speed up the loop, pre-allocate a container for the text
    text = vector("list", 1000)
  )
}
