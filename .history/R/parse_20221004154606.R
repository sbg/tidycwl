#' Parse CWL content type
#'
#' @param x CWL object
#'
#' @return CWL content type (Workflow or CommandLineTool)
#'
#' @export parse_type
#'
#' @examples
#' system.file("cwl/sbg/workflow/gatk4-wgs.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   parse_type()
#'
#' system.file("cwl/sbg/tool/bwa-mem.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   parse_type()
parse_type <- function(x) {
  if (!is_cwl(x)) stop(not_cwl_obj_str_gbl)
  trimws(x$class)
}

#' Parse the metadata in the CWL workflow
#'
#' @param x CWL object
#'
#' @return List of CWL metadata
#'
#' @export parse_meta
#'
#' @examples
#' system.file("cwl/sbg/workflow/gatk4-wgs.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   parse_meta()
parse_meta <- function(x) {
  if (!is_cwl(x)) stop(not_cwl_obj_str_gbl)
  sanitize_metadata_list(
    list(
      "id" = x$"id",
      "label" = x$"label",
      "class" = x$"class",
      "cwlversion" = x$"cwlVersion",
      "sbg:revision" = x$"sbg:revision",
      "sbg:id" = x$"sbg:id",

      # Workflow description can be contained in fields labeled either "doc" or "description"
      # doc is compliant CWL 1.0+ but many legacy apps use the description field instead
      "doc" = x$"doc",
      "description" = x$"description"
    )
  )
}

#' Parse the inputs of the CWL workflow into a data frame
#'
#' @param x CWL object
#' @param simplify Simplify the list as a data frame?
#'
#' @return List or data frame of inputs
#'
#' @importFrom dplyr bind_rows
#'
#' @export parse_inputs
#'
#' @examples
#' system.file("cwl/sbg/workflow/rnaseq-salmon.json", package = "tidycwl") %>%
#'   read_cwl_json() %>%
#'   parse_inputs() %>%
#'   names()
#'
#' system.file("cwl/sbg/workflow/rnaseq-salmon.cwl", package = "tidycwl") %>%
#'   read_cwl_yaml() %>%
#'   parse_inputs() %>%
#'   names()
parse_inputs <- function(x, simplify = TRUE) {
  if (!is_cwl(x)) stop(not_cwl_obj_str_gbl)

  if (is.null(x$inputs)) {
    return(NULL)
  }
  inputs <- x$inputs

  if (is_literal_list(inputs)) {
    df <- list2df(sanitize_inputs_list(inputs))
  } else if (is_literal_df(inputs)) {
    df <- sanitize_inputs_df(inputs)
  } else {
    stop("inputs cannot be properly parsed from the CWL object")
  }

  if (!simplify) df <- jsonlite::toJSON(df, dataframe = "rows")

  df
}

#' Parse the outputs of the CWL workflow into a data frame
#'
#' @param x CWL object
#' @param simplify Simplify the list as a data frame?
#'
#' @return List or data frame of outputs
#'
#' @importFrom dplyr bind_rows
#'
#' @export parse_outputs
#'
#' @examples
#' system.file("cwl/sbg/workflow/rnaseq-salmon.json", package = "tidycwl") %>%
#'   read_cwl_json() %>%
#'   parse_outputs() %>%
#'   names()
#'
#' system.file("cwl/sbg/workflow/rnaseq-salmon.cwl", package = "tidycwl") %>%
#'   read_cwl_yaml() %>%
#'   parse_outputs() %>%
#'   names()
parse_outputs <- function(x, simplify = TRUE) {
  if (!is_cwl(x)) stop(not_cwl_obj_str_gbl)
  if (is.null(x$outputs)) {
    return(NULL)
  }

  outputs <- x$outputs

  if (is_literal_list(outputs)) {
    df <- list2df(sanitize_outputs_list(outputs))
  } else if (is_literal_df(outputs)) {
    df <- sanitize_outputs_df(outputs)
  } else {
    stop("inputs cannot be properly parsed from the CWL object")
  }

  if (!simplify) df <- jsonlite::toJSON(df, dataframe = "rows")

  df
}

#' Parse the steps of the CWL workflow into a data frame
#'
#' @param x CWL object
#'
#' @return List or data frame of steps
#'
#' @export parse_steps
#'
#' @examples
#' # steps represented by a dictionary
#' system.file("cwl/sbg/workflow/rnaseq-salmon.json", package = "tidycwl") %>%
#'   read_cwl_json() %>%
#'   parse_steps() %>%
#'   nrow()
#'
#' # steps represented by a list
#' system.file("cwl/sbg/workflow/rnaseq-salmon.cwl", package = "tidycwl") %>%
#'   read_cwl_yaml() %>%
#'   parse_steps() %>%
#'   length()
parse_steps <- function(x) {
  if (!is_cwl(x)) stop(not_cwl_obj_str_gbl)
  if (is.null(x$steps)) {
    return(NULL)
  }
  # since this can be a JSON list or dict, we need to deal
  # with each case separately in downstream functions
  x$steps
}

#' Parse a CWL workflow
#'
#' Parse a CWL workflow and return the metadata,
#' inputs, outputs, and steps in a list.
#'
#' @param x CWL object
#'
#' @return List of CWL metadata, inputs, outputs, and steps
#'
#' @export parse_cwl
#'
#' @examples
#' system.file("cwl/sbg/workflow/rnaseq-salmon.cwl", package = "tidycwl") %>%
#'   read_cwl_yaml() %>%
#'   parse_cwl() %>%
#'   names()
parse_cwl <- function(x) {
  list(
    "meta" = parse_meta(x),
    "steps" = parse_steps(x),
    "inputs" = parse_inputs(x),
    "outputs" = parse_outputs(x)
  )
}