wikkitidy_user_agent <- function(.req) {
  httr2::req_user_agent(.req, "wikkitidy R package (https://github.com/wikihistories/wikkitidy)")
}
