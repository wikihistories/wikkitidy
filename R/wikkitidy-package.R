#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom pillar tbl_format_footer
#' @importFrom pillar tbl_sum
#' @importFrom vctrs vec_ptype_abbr
#' @importFrom vctrs vec_ptype_full
## usethis namespace: end
NULL

# Suppress 'global variable' warnings due to using dplyr
# See https://dplyr.tidyverse.org/articles/in-packages.html
utils::globalVariables("type")
utils::globalVariables("generator")
utils::globalVariables("group")
utils::globalVariables("name")
utils::globalVariables(".")
utils::globalVariables("is_generator")
