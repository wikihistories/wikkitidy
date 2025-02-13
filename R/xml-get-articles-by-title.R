xml_get_articles_by_title <- function(titles, dump_file, index_file) {

}

# Return a list, whose keys are segment offsets, and whose values
# are character vectors of articles in those segments
make_index <- function(titles, index_file) {
  iterate_index <- coro::gen(for (l in readLines(index_file)) coro::yield(parse_index_entry(l)))
  # This function makes two strong assumptions: every title is in the index,
  # and both the titles and the index are in alphabetical order. Hence
  # the call to 'sort'.
  iterate_titles <- coro::gen(for (t in sort(titles)) coro::yield(t))

  # There will never be more segments than the number
  # of titles; except that due to the management of the
  # byte offsets, the first slot in each list will by
  # wasted
  out <- vector(mode="list", length=length(titles) + 1)
  out_names <- vector(mode="list", length=length(titles) + 1)

  # Start the the beginning
  curr_ent <- iterate_index()
  curr_tit <- iterate_titles()
  curr_out_idx <- 1
  curr_byte_offset <- ""

  # Iterate over coroutines
  while(!coro::is_exhausted(curr_ent) && !coro::is_exhausted(curr_tit)) {
    if (curr_ent[[3]] == curr_tit) {
      # We have found a match. Check to see if we are at the correct
      # byte offset. If not, increment our position in the out array
      if (curr_byte_offset != curr_ent[[1]]) {
        # On the first iteration, this will write the byte offset
        # into position 2, leaving a NULL entry at the start. Who cares!
        curr_out_idx <- curr_out_idx + 1
        out_names[[curr_out_idx]] <- curr_ent[[1]]
        curr_byte_offset <- curr_ent[[1]]
      }
      # Once byte offset has been managed, write the title into the out
      # array
      out[[curr_out_idx]] <- append(out[[curr_out_idx]], curr_tit)
      # Then move to next title
      curr_tit <- iterate_titles()
    }
    # In any case, move to next index entry
    curr_ent <- iterate_index()
  }

  # Set names
  names(out) <- out_names
  # Strip nulls
  out <- purrr::discard(out, rlang::is_null)

  return(out)
}

parse_index_entry <- function(index_entry) {
  components <- stringr::str_split(index_entry, ":", n = 3)
  return(components[[1]])
}

