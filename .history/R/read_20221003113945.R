#' Changelog
#' ---  Saul E. Acevedo 9/29/2022
#'     --- Added functionality that replace NA values in label column with those in id


#' Read a CWL file (JSON format) into a list
#'
#' @param file A file path, JSON string, or connection.
#'
#' @return List representation of the input CWL
#'
#' @importFrom jsonlite fromJSON
#'
#' @export read_cwl_json
#'
#' @examples
#' system.file("cwl/sbg/workflow/rnaseq-salmon.json", package = "tidycwl") %>%
#'   read_cwl_json()
read_cwl_json <- function(file) {
  lst <- jsonlite::fromJSON(file)
  lst <- replace_labels_if_na(lst)
  class(lst) <- cwl_str_gbl
  lst
}

#' Read a CWL file (YAML format) into a list
#'
#' @param file A file path, YAML string, or connection.
#'
#' @return List representation of the input CWL
#'
#' @export read_cwl_yaml
#'
#' @examples
#' system.file("cwl/sbg/workflow/rnaseq-salmon.cwl", package = "tidycwl") %>%
#'   read_cwl_yaml()
read_cwl_yaml <- function(file) {
  lst <- yaml::read_yaml(file)
  lst <- replace_labels_if_na(lst)
  class(lst) <- cwl_str_gbl
  lst
}

#' Read a CWL file into a list
#'
#' @param file A file path, character string, or connection.
#' @param format CWL storage format. \code{"json"} or \code{"yaml"}.
#'
#' @return List representation of the input CWL
#'
#' @export read_cwl
#'
#' @examples
#' system.file("cwl/sbg/workflow/rnaseq-salmon.json", package = "tidycwl") %>%
#'   read_cwl(format = "json")
#'
#' system.file("cwl/sbg/workflow/rnaseq-salmon.cwl", package = "tidycwl") %>%
#'   read_cwl(format = "yaml")
read_cwl <- function(file, format = c("json", "yaml")) {
  format <- match.arg(format)
  if (format == "json") {
    return(read_cwl_json(file))
  }
  if (format == "yaml") {
    return(read_cwl_yaml(file))
  }
}
