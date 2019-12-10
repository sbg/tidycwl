# tidycwl

[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![CRAN Version](https://www.r-pkg.org/badges/version/tidycwl)](https://cran.r-project.org/package=tidycwl)
[![Travis build status](https://travis-ci.org/sbg/tidycwl.svg?branch=master)](https://travis-ci.org/sbg/tidycwl)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/dp3rsacx9m10x6ei/branch/master?svg=true)](https://ci.appveyor.com/project/nanxstats/tidycwl)
[![Downloads from the RStudio CRAN mirror](https://cranlogs.r-pkg.org/badges/grand-total/tidycwl)](https://cran.r-project.org/package=tidycwl)

Read, parse, and visualize Common Workflow Language (CWL) workflows.

The [Common Workflow Language](https://www.commonwl.org/) is an open standard for describing data analysis workflows. This package takes the raw Common Workflow Language workflows encoded in JSON or YAML and turns the workflow elements into tidy data frames or lists. A graph representation for the workflow can be constructed and visualized with the parsed workflow inputs, outputs, and steps. Users can embed the visualizations in their Shiny applications, and export them as HTML files or static images.

Check out the [vignette](https://sbg.github.io/tidycwl/articles/tidycwl.html) for a quick introduction.

## Installation

To download and install `tidycwl` from CRAN:

```r
install.packages("tidycwl")
```

Or try the development version on GitHub:

```r
# install.packages("remotes")
remotes::install_github("sbg/tidycwl")
```

## Copyright

© 2019 Seven Bridges Genomics, Inc. All rights reserved.

This project is licensed under the GNU Affero General Public License v3.
