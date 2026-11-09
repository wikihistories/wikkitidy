# wikkitidy

An R package for requesting data from Wikipedia and related Wikimedia services and returning it in tidy form.

## Language

### Data sources

**Action API**:
The MediaWiki query API (`api.php`), queried by composing query modules into a single request and paging through continued results.
_Avoid_: MediaWiki API, query API

**REST API**:
Wikimedia's resource-oriented API, where each endpoint returns one resource for one page.
_Avoid_: Core API, Wikimedia API

**XTools API**:
The XTools service's REST endpoints, which return derived statistics about pages and editors.
_Avoid_: XTools REST

### Failure handling

**Graceful failure**:
A failed request that returns an object describing the failure instead of throwing an error.
_Avoid_: soft failure, silent failure

**Failure mode**:
The caller's choice between throwing an error and failing quietly, by recording the error against the affected row, when a REST API or XTools API request fails.
_Avoid_: error mode
