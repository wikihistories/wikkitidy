blame <- function(pages,
                  term,
                  id_type = c("pageid", "title"),
                  search_for = c("insertion", "deletion"),
                  search_strategy = c("linear", "binary"),
                  direction = c("back", "forward"),
                  from = NULL,
                  to = NULL,
                  skip = NULL,
                  language = "en") {
  id_type <- rlang::arg_match(id_type)
  search_for <- rlang::arg_match(search_for)
  search_strategy <- rlang::arg_match(search_strategy)
  direction <- rlang::arg_match(direction)
  iterator <- tibble::tibble(pages, from, to)
  if (search_strategy == "binary" & !is.null(skip)) {
    warning("Ignored parameter `skip`: only relevant when search_strategy == 'linear'")
  }
  blames <-
    purrr::pmap(
      iterator,
      \(page, from, to) .blame_one(
        search_strategy,
        page,
        term,
        id_type,
        search_for,
        direction,
        from,
        to,
        language
      )
    )
}

.blame_one <- function(search_strategy, ...) {
  switch(
    search_strategy,
    linear = .linear_blame_one(...),
    binary = .binary_blame_one(...)
  )
}

.linear_blame_one <- function(page, term, id_type, search_for, from, to, language) {

}

.binary_blame_one <- function(page, term, id_type, search_for, from, to, language) {
  all_revisions <- .get_all_revids(page, id_type, from, to, language)
  n_init <- length(all_revisions$revid)
  # Set the 'found' parameter to kick off the iteration
  found <- if (search_for == "deletion") TRUE else FALSE
  curr_idx <- n_init %/% 2


}

.get_all_revids <- function(page, id_type, from, to, language) {
  wiki_action_request(language = language) %>%
    query_list_of(
      "revisions",
      id_type := page,
      rvprop = "ids",
      rvlimit = "max",
      # By default, the API lists revisions in reverse chronological order, so
      # the "from" revision should be the *final* revision retrieved, and the
      # "to" revision should be the first.
      rvend = from,
      rvstart = to
    ) %>%
    retrieve_all()
}

.make_detector <- function(main_diff_type, highlight_type) {
  detect_term_in_diff <- function(diff, term) {
    if (diff$type == main_diff_type) {
      stringr::str_detect(diff$text, term)
    } else if (diff$type == 3 | diff$type == 5) {
      .scan_highlight(diff, term, highlight_type)
    } else {
      FALSE
    }
  }
}

.scan_highlight <- function(diff, term, highlight_type) {
  diff # a diff object
  term # the term being searched for
  highlight_type # the type of highlight to search for the term in
}

.detect_deletion <- .make_detector(main_diff_type = 2, highlight_type = 1)

.detect_insertion <- .make_detector(main_diff_type = 1, highlight_type = 0)

.compute_boundaries <- function(boundaries, action=c("branch_left","try_right")) {
  action <- rlang::arg_match(action)
  upper <- boundaries$upper
  lower <- boundaries$lower
  if (action == "branch_left") {
    delta <- (upper - lower + 1) %/% 2
    list(upper=upper-delta, lower=lower)
  } else {
    delta <- (upper - lower + 1)
    list(upper=upper+delta, lower=lower+delta)
  }
}
