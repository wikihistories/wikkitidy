# Examples fail gracefully; tests fail loudly

CRAN policy forbids examples that error when an internet resource is unavailable. Every documentation example that hits a live API therefore fails gracefully everywhere: Action API examples use `gracefully()`, and REST/XTools examples use `failure_mode = "quiet"`. Both handle HTTP errors (`httr2_http`) and connection failures (`httr2_failure`). API breakage is detected only by the testthat suite, where live tests throw errors and are skipped on CRAN with `skip_on_cran()`.

## Considered Options

- **Make examples environment-aware (graceful on CRAN, erroring elsewhere via `NOT_CRAN`).** Rejected: it duplicates what the tests already do, and it would make the exported `gracefully()` behave differently depending on an environment variable.

## Consequences

- Examples are not evidence that the API still works. Every behaviour shown in an example must be covered by a test.
- `gracefully()` returns failures as `wikkitidy_failure` objects and short-circuits on them (a failure passed in is returned unchanged), so chained examples stay safe. Calls to code outside wikkitidy (e.g. `tidyr::unnest()`) after a graceful step need an explicit guard: `if (!is_wikkitidy_failure(x))`.
- Quiet failure mode works per row, not per request: each failed element becomes a placeholder row (`error_code`, plus `error_message` for connection failures), so output length always matches input length.
