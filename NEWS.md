# tidycwl 1.0.7

## Improvements

- Changed package maintainer.

# tidycwl 1.0.6

## Bug Fixes

- Fixed the top level description metadata field, which should be defined as "doc" rather than "description." Compatibility is maintained with CWL files which use a "description" field.

## Improvements

- Added Jeffrey Grover as the new package maintainer.

# tidycwl 1.0.5

## Bug Fixes

- Removed the non-ASCII characters in the example data (rnaseq-salmon.cwl) to fix the check error under the Debian Linux environment with the ISO-8859-15 locale.

## Improvements

- Update package maintainer.

# tidycwl 1.0.4

## Improvements

- Run example code conditionally which requires pandoc to support CRAN's Solaris environment.
- Added pandoc to the SystemRequirements field.

# tidycwl 1.0.3

## Improvements

- Removed the redundant copy of the full AGPL-3 license file.

# tidycwl 1.0.2

## Improvements

- Added single quotes for package names, software names, and API names to the Title and Description section.
- Removed the unnecessary `if(interactive()){}` statement from the example in `export_html()`.
- Added PhantomJS to the SystemRequirements field as it is an essential system dependency for `export_image()`.

# tidycwl 1.0.1

## Improvements

- Added more details about the package functionality to the Description text.
- Updated the examples in `export_html()` and `export_image()` to write files to temporary folder.
- Added the `\value` RD tag to exported functions to explain the return results.
- Imported and re-exported the pipe operator to simplify the example code.

# tidycwl 1.0.0

## New Features

- First public release.
