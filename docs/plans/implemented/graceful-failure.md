# Plan: graceful failure for live API examples

Implements [ADR 0001](../adr/0001-examples-fail-gracefully-tests-fail-loudly.md). Terms: see [GLOSSARY.md](../../GLOSSARY.md).

Implemented 2026-11-10: resulted in release 0.2.0

## Decisions

- All examples that hit a live API fail gracefully everywhere (no `NOT_CRAN` switch). Breakage is caught by tests.
- Tests throw errors and use `skip_on_cran()`. Every behaviour shown in an example must have test coverage.
- `gracefully()` stays environment-agnostic.
- Action API examples use `gracefully()`. REST/XTools examples use `failure_mode = "quiet"`.
- Both handle `httr2_http` and `httr2_failure`.
- Out of scope: inconsistent `failure_mode` defaults (`"error"` vs `c("error", "quiet")`).

## Steps

### 1. `wikkitidy_failure` S3 class (new `R/wikkitidy-failure.R`)

- `new_wikkitidy_failure(x)` (internal): prepend class `wikkitidy_failure` to an `httr2_response` or condition object.
- `is_wikkitidy_failure(x)` (exported): `inherits(x, "wikkitidy_failure")`.
- `print.wikkitidy_failure()`: cli alert ("The query you tried was unsuccessful..."), then print the underlying response or condition.

### 2. `gracefully()` (`R/request-gracefully.R`)

- If `is_wikkitidy_failure(request_object)`, return it unchanged (short-circuit).
- Catch `httr2_http`: return `new_wikkitidy_failure(cnd$resp)`.
- Catch `httr2_failure`: return `new_wikkitidy_failure(cnd)`.
- Printing moves to the print method; remove `show_bad_response()`.
- Update docs: return value, short-circuit behaviour, link to `is_wikkitidy_failure()`.

### 3. Quiet failure mode (`R/get-rest-resource.R`)

- In quiet mode, catch `httr2_failure` per element (around perform + handle in the `pmap` pipeline).
- Return the placeholder row `list(error_code = NA_integer_, error_message = conditionMessage(cnd))`.
- HTTP errors stay `list(error_code = <status>)`. Check that `parse_response()` handles the mixed/missing `error_message` column for every `response_type`.
- Preserve the invariant that output length equals input length.
- Update `@param failure_mode` docs.

### 4. Examples

- Audit all `@examples` that make live calls (Action API, REST, XTools).
- Action API: wrap in `gracefully()`.
- REST/XTools: pass `failure_mode = "quiet"`.
- Replace `if (tibble::is_tibble(x))` guards with `if (!is_wikkitidy_failure(x))` before calls to other packages (e.g. `tidyr::unnest()`).
- Update the `gracefully()` example to show a connection failure and the short-circuit.

### 5. Tests

- `test-wikkitidy-failure.R`: constructor, predicate, print method.
- `test-request-gracefully.R` (`webfakes`): HTTP error, connection failure (e.g. unreachable port), short-circuit, success passthrough.
- `test-get-rest-resource.R`: quiet mode on a connection failure gives a placeholder row with the correct length; mixed success/failure input.
- Coverage audit: map each example behaviour to a live test with `skip_on_cran()`; add any that are missing.

### 6. Housekeeping

- `NEWS.md`: new `wikkitidy_failure` class and `is_wikkitidy_failure()`; `gracefully()` catches connection failures and short-circuits; quiet mode records connection failures; examples always fail gracefully.
- `_pkgdown.yml`: add `is_wikkitidy_failure` to the reference index.
- `devtools::document()`, `devtools::test()`, `devtools::check()`.
- Remind the human to run `usethis::use_version("minor")` (new export).
