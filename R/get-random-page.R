get_random_page <- function(n, format = c("title", "html", "summary", "related"), language = "en") {
  format <- rlang::arg_match(format)
  response_type <- switch(
    format,
    "title" = ,
    "summary" = ,
    "related" = "json",
    "html" = "html"
  )
  format_n <- rep(format, n)
  response <- get_rest_resource(
    "page", "random", format_n,
    language = language, endpoint = "wikimedia", response_type = response_type
  )
  response
}
