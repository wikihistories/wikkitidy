datetime_for_url <- function(x, .default = NULL, .precision = "ymd") {
  x_sym <- rlang::enexpr(x)
  if (rlang::is_null(x)) {
    return(.default)
  }
  withCallingHandlers(
    warning = function(cnd) {
      rlang::abort(
        glue::glue("Unparseable dates in `{stringr::str_trunc(rlang::expr_deparse(x_sym)[[1]], 25)}`")
      )
    },
    {datetimes <- lubridate::as_datetime(x)}
  )
  lubridate::format_ISO8601(datetimes, precision = .precision)
}
