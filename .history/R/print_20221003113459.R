#' Print CWL objects
#'
#' Print a brief summary of the CWL object.
#'
#' @param x An object of class \code{cwl}.
#' @param ... Additional parameters for \code{\link{print}} (not used).
#'
#' @return The input \code{cwl} object.
#'
#' @method print cwl
#'
#' @export
#'
#' @examples
#' path <- system.file("cwl/sbg/workflow/rnaseq-salmon.json", package = "tidycwl")
#' flow <- read_cwl(path, format = "json")
#' flow
print.cwl <- function(x, ...) {
  if (!is_cwl(x)) stop(not_cwl_obj_str_gbl)
  cat("Name:", x$label, "\n")
  cat("Class:", x$class, "\n")
  cat("CWL Version:", x$cwlVersion, "\n")
  invisible(x)
}
