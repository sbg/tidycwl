# tidycwl 1.0.1

## Test environments

- Local macOS install, R 3.6.1
- Ubuntu 16.04.6 LTS (on Travis-CI), R 3.6.1
- win-builder (release, devel, and oldrelease)

## R CMD check results

There were no ERRORs or WARNINGs.

## Detailed Description text

More details about the package functionality have been added to the Description text.

## Files writing location

- The examples in `export_html()` and `export_image()` have been updated to only write files to `tempdir()` instead of user's home filespace.

- Double-checked all examples/vignettes/tests. No other code found writing to home filespace.

## Missing RD tags

The `\value` tag has been added to exported functions to explain the return results.
