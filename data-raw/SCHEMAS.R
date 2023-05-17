## code to prepare `SCHEMAS` dataset goes here
get_schema_query_modules <- function() {
  raw <- wiki_action_request(action = "paraminfo", modules = "query+*") %>%
    wikkitidy_user_agent() %>%
    httr2::req_perform() %>%
    httr2::resp_body_json() %>%
    purrr::pluck("paraminfo", "modules")

  # get list of unique names to form column headings
  col_names <- raw %>%
    purrr::reduce(
      \(acc, nxt) union(acc, names(nxt)),
      .init = as.character()) %>%
    as.character()

  raw %>%
    purrr::list_transpose(
      template = col_names,
      default = NA
    ) %>%
    tibble::tibble(!!!.)
}

schema_query_modules <- get_schema_query_modules()

usethis::use_data(schema_query_modules, internal = TRUE, overwrite = TRUE)
