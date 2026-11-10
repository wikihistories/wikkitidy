# R style

- Always qualify functions with their namespace (unless base R),
  e.g. [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html)
- Prefer tidyverse functions, e.g. `purrr` for iteration, `tibble` for
  data frames
- Use `%>%`

# Project structure

- This repo is an installable R package (`wikkitidy`) holding reusable
  logic (`R/`), data (`inst/extdata/`), and vignettes.
- Result CSVs are promoted into `inst/extdata/results/` only manually,
  after researcher review; full run output stays external (synced via
  `sync-results.sh`), never committed.
- See `GLOSSARY.md` for domain terminology and `docs/adr/` for
  architectural decisions.
- This package is available on CRAN
- `testthat` is used for testing (`tests/testthat`): to avoid problems
  on CRAN, live tests of Wikipedia’s API are blocked from execution on
  CRAN.

# Project management

- Be as concise as possible while still making sense. Sacrifice grammar
  for concision.
- Outstanding work is tracked as GitHub issues if required, and in major
  project milestones in `README.md`
- Summarise all key changes in `NEWS.md`, and remind the human to bump
  the version using `usethis::use_version()` as appropriate.
- Remind the user as appropriate to check that tests will run as
  intended on CRAN:
  `withr::with_envvar(c(NOT_CRAN = "false"), devtools::test(reporter = "summary"))`

## Agent skills

### Issue tracker

GitHub Issues on wikihistories/wikkitidy via `gh`. See
`docs/agents/issue-tracker.md`.

### Domain docs

Single-context: root `GLOSSARY.md` + `docs/adr/`. See
`docs/agents/domain.md`.
