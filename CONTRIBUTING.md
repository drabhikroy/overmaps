# Contributing to Overmaps

Thank you for considering a contribution. This is a noncommercial open source project under the PolyForm Noncommercial License 1.0.0, and contributions are accepted under those same terms.

## Before you start

Open an issue describing the change before writing code. A new map type, a new ancient site, a user interface change, and a bug fix each want a different conversation, and an issue saves you from building something that does not fit.

A new ancient site needs open, accurate data: street geometry that exists in OpenStreetMap or in a dataset with a license that allows reuse. Please link the source in the issue. Sites without such data are left out on purpose.

## Running the app

```r
shiny::runApp(".", launch.browser = TRUE)
```

Requires R 4.1 or later. Run `Rscript install.R` once to add the packages.

## Running the tests

```sh
Rscript tests/run_tests.R
```

The gate must pass before a pull request is reviewed. The suite stubs every network call, so it runs without a connection. The browser scripts in `tests/browser` drive the offline demo app and are worth running for any change to the interface.

## Style

- American English.
- No em dashes or en dashes in code, comments, or documents.
- No contractions in prose.
- Plain, specific wording. Say what a thing does.
- Keep the version in step across `app.R`, `DESCRIPTION`, `CITATION.cff`, and `CHANGELOG.md`.
