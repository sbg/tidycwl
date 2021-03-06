% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse.R
\name{parse_cwl}
\alias{parse_cwl}
\title{Parse a CWL workflow}
\usage{
parse_cwl(x)
}
\arguments{
\item{x}{CWL object}
}
\value{
List of CWL metadata, inputs, outputs, and steps
}
\description{
Parse a CWL workflow and return the metadata,
inputs, outputs, and steps in a list.
}
\examples{
system.file("cwl/sbg/workflow/rnaseq-salmon.cwl", package = "tidycwl") \%>\%
  read_cwl_yaml() \%>\%
  parse_cwl() \%>\%
  names()
}
