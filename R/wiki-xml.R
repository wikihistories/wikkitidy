#' Check that a Wikimedia XML file has not been corrupted
#'
#' The Wikimedia Foundation publishes MD5 checksums for all its database dumps.
#' This function looks up the published sha1 checksums based on the file name,
#' then compares them to the locally calcualte has using the `openssl` package.
#'
#' @param path The path to the file
#'
#' @return True (invisibly) if successful, otherwise error
#' @export
#'
#' @examples
#' akan_wiki <- wikkitidy_example("akan_wiki")
#' verify_xml_integrity(akan_wiki)
verify_xml_integrity <- function(path) {
  checksum <- .get_checksum(path)
  conn <- file(path, open="rb")
  local_hash <- openssl::sha1(conn) %>% as.character()
  if (local_hash == checksum) {
    invisible(TRUE)
  } else {
    rlang::abort(
      glue::glue("Invalid checksum for {basename(path)}. File may be corrupted.")
    )
  }
}

.get_checksum <- function(path) {
  chks_tbl <- .get_checksums(path) %>% .checksum_tbl()
  dplyr::filter(chks_tbl, file == basename(path))$checksum[[1]]
}

.checksum_tbl <- function(raw_checksums) {
  rows <- strsplit(raw_checksums, split="\n") %>% purrr::map(strsplit, split="  ")
  tbl <- do.call(rbind, rows[[1]]) %>% data.frame()
  names(tbl) <- c("checksum", "file")
  tbl
}

.get_checksums <- function(path) {
  filename <- basename(path)
  nm_parts <- stringr::str_split_1(filename, "-")
  md5_file <- glue::glue("{nm_parts[1]}-{nm_parts[2]}-sha1sums.txt")
  checksums <- httr2::request("https://dumps.wikimedia.org/") %>%
    wikkitidy_user_agent() %>%
    httr2::req_url_path_append(nm_parts[1], nm_parts[2], md5_file) %>%
    httr2::req_perform() %>%
    httr2::resp_body_string()
  checksums
}
