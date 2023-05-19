#' Explore Wikipedia's category system
#'
#' @description These functions provide access to the
#'   [CategoryMembers](https://www.mediawiki.org/wiki/API:Categorymembers)
#'   endpoint of the Action API.
#'
#'   [query_category_members()] builds a [generator
#'   query][query_generate_pages()] to return the members of a given category.
#'
#'   [build_category_tree()] finds all the pages and subcategories beneath the
#'   passed category, then recursively finds all the pages and subcategories
#'   beneath them, until it can find no more subcategories.
#'
#' @param .req A [query request object][wiki_action_request()]
#' @param category The category to start from. [query_category_members()]
#'   accepts either a numeric pageid or the page title. [build_category_tree()]
#'   accepts a vector of page titles.
#' @param namespace Only return category members from the provided namespace
#' @param type Alternative to `namespace`: the type of category member to
#'   return. Multiple types can be requested using a character vector. Defaults
#'   to all.
#' @param limit The number to return each batch. Max 500.
#' @param sort How to sort the returned category members. 'timestamp' sorts them
#'   by the date they were included in the category; 'sortkey' by the category
#'   member's unique hexadecimal code
#' @param dir The direction in which to sort them
#' @param start If `sort` == 'timestamp', only return category members from
#'   after this date. The argument is parsed by [lubridate::as_date()]
#' @param end If `sort` == 'timestamp', only return category members included in
#'   the category from before this date. The argument is parsed by
#'   [lubridate::as_date()]
#' @param language The language edition of Wikipedia to query
#'
#' @return [query_category_members()]: A request object of type
#'   `generator/query/action_api/httr2_request`, which can be passed to
#'   [next_batch()] or [retrieve_all()]. You can specify which properties to
#'   retrieve for each page using [query_page_properties()].
#'
#'   [build_category_tree()]: A list containing two dataframes. `nodes` lists
#'   all the subcategories and pages found underneath the passed categories.
#'   `edges` records the connections between them. The `source` column gives the
#'   pageid of the parent category, while the `target` column gives the pageid
#'   of any categories, pages or files contained within the `source` category.
#'   The `timestamp` records the moment when the `target` page or subcategory
#'   was included in the `source` category. The two dataframes in the list can
#'   be passed to [igraph::graph_from_data_frame] for network analysis.
#' @export
#'
#' @examples
#' # Get the first 10 pages in 'Category:Physics' on English Wikipedia
#' physics_members <- wiki_action_request() %>%
#'   query_category_members("Physics") %>% next_batch()
#' physics_members
#'
#' # Retrieve interwiki links for the first 10 pages in the 'Beatles Albums'
#' # category on Danish Wikipedia
#' # TODO: This currently doesn't work, because incomplete batches aren't properly handled
#' beatles <- wiki_action_request(language = "da") %>%
#'   query_category_members("Beatles-album") %>%
#'   query_page_properties("langlinks") %>%
#'   next_batch()
#'
#' # Build the tree of all trees in Wikipedia
#' tree <- build_category_tree("Category:Trees (graph theory)")
#' tree
#'
#' # For network analysis and visualisation, you can pass the category tree
#' # to igraph
#' tree_graph <- igraph::graph_from_data_frame(tree$edges, vertices = tree$nodes)
#' tree_graph
query_category_members <- function(
  .req,
  category,
  namespace = NULL,
  type = c("file", "page", "subcat"),
  limit = 10,
  sort = c("sortkey", "timestamp"),
  dir = c("ascending", "descending", "newer", "older"),
  start = NULL,
  end = NULL,
  language = "en"
) {
  category <- id_or_title(category, prefix = "Category")
  namespace <- check_namespace(namespace)
  type <- rlang::arg_match(type, multiple = T) %>%
    paste0(collapse = "|")
  limit <- check_limit(limit, max = 500)
  sort <- rlang::arg_match(sort)
  dir <- rlang::arg_match(dir)
  if (!is.null(start) || !is.null(end)) {
    if (!sort == "timestamp") {
      rlang::abort("If using `start` or `end`, you must use sort = 'timestamp'",
                   class = "incompatible_arguments")
    }
  }
  timestamp_args <- process_timestamps(start, end)
  query_params <- rlang::dots_list(
    !!!category,
    namespace,
    type,
    limit,
    sort,
    dir,
    !!!timestamp_args,
    .named = T
  )
  names(query_params) <- stringr::str_c("gcm", names(query_params))
  query_generate_pages(.req, "categorymembers", !!!query_params)
}

#' @rdname query_category_members
#' @export
build_category_tree <- function(category, language = "en") {
  root <- get_latest_revision(category, language)
  tree <- list(
    nodes = tibble::tibble(
      pageid = root[["page_id"]],
      ns = root[["namespace"]],
      title = root[["title"]],
      type = "root"
    ),
    edges = tibble::tibble(
      source = integer(),
      target = integer(),
      timestamp = character()
    )
  )
  progress <- cli::cli_progress_bar(
    "Walking subcategories:",
    .auto_close = FALSE,
    clear = FALSE
  )
  tree <- walk_category_tree(tree, root$page_id, language, progress)
  cli::cli_progress_done(id = progress)
  # strip irrelevant <query_tbl> attributes and metadata
  tree <- purrr::map(tree, tibble::as_tibble)
  tree
}

walk_category_tree <- function(tree, category, language, progress) {
  children <- get_children(category, language, progress)
  new_categories <- extract_new_categories(tree, children)
  if (length(new_categories) > 0) {
    walk_category_tree(merge_trees(tree, children), new_categories, language, progress)
  } else {
    merge_trees(tree, children)
  }
}

get_one_children <- function(category, language = "en", progress) {
  cli::cli_progress_update(id = progress, force = T)
  children <- wiki_action_request(language = language) %>%
    new_list_query(
      "categorymembers",
      cmpageid = category,
      cmprop = "ids|title|type|timestamp",
      cmlimit = "max"
    ) %>%
    retrieve_all() %>%
    dplyr::mutate(source = category)
}

get_children <- function(category, language = "en", progress) {
  params <- vctrs::vec_recycle_common(category, language, progress)
  children <- purrr::pmap(params, get_one_children)
  children <- purrr::list_rbind(children)
  list(
    nodes = extract_nodes(children),
    edges = extract_edges(children)
  )
}

extract_nodes <- function(children) {
  children %>%
    dplyr::select(!timestamp:source) %>%
    dplyr::distinct()
}

extract_edges <- function(children) {
  children %>%
    dplyr::select(source, target = pageid, timestamp)
}

extract_new_categories <- function(tree, children) {
  children$nodes %>%
    dplyr::filter(type == "subcat") %>%
    dplyr::anti_join(tree$nodes, by = "pageid") %>%
    .[["pageid"]]
}

merge_trees <- function(old_tree, new_tree) {
  list(
    nodes = dplyr::union(old_tree$nodes, new_tree$nodes),
    edges = dplyr::union(old_tree$edges, new_tree$edges)
  )
}
