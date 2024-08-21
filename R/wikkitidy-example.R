#' Get path to wikkitidy example
#'
#' wikkitidy comes bundled with a number of sample files in its `inst/extdata`
#' directory. This function make them easy to access
#'
#' @param file Name of file. If `NULL`, the example files will be listed.
#' @export
#' @return A character vector, containing either the path of the chosen file, or
#'   the nicknames of all available example files.
#'
#' @keywords utility
#' @examples
#' wikkitidy_example()
#' wikkitidy_example("akan_wiki")
wikkitidy_example <- function(file = NULL) {
  if (is.null(file)) {
    names(.fn_map)
  } else {
    file <- rlang::arg_match(file, names(.fn_map))
    file <- .fn_map[[file]]
    system.file("extdata", file, package = "wikkitidy", mustWork = TRUE)
  }
}

.fn_map <- list(
  akan_wiki = "akwiki-20230301-pages-articles-multistream-index.txt.bz2"
)
