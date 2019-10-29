#' Is this a CWL object?
#'
#' @param x any object
#'
#' @export is_cwl
#'
#' @examples
#' library("tidycwl")
#' library("magrittr")
#'
#' system.file("cwl/sbg/workflow/gatk4-wgs.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   is_cwl()
is_cwl <- function(x) if ("cwl" %in% class(x)) TRUE else FALSE

#' Is this CWL draft2?
#'
#' @param x CWL object
#'
#' @export is_draft2
#'
#' @examples
#' library("tidycwl")
#' library("magrittr")
#'
#' system.file("cwl/sbg/workflow/gatk4-wgs.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   is_draft2()
#'
#' system.file("cwl/sbg/workflow/rnaseq-salmon.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   is_draft2()
is_draft2 <- function(x) get_cwl_version(x) == "sbg:draft-2"

#' Is this CWL v1.0?
#'
#' @param x CWL object
#'
#' @export is_v1.0
#'
#' @examples
#' library("tidycwl")
#' library("magrittr")
#'
#' system.file("cwl/sbg/workflow/gatk4-wgs.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   is_v1.0()
#'
#' system.file("cwl/sbg/workflow/rnaseq-salmon.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   is_v1.0()
is_v1.0 <- function(x) get_cwl_version(x) == "v1.0"

#' Is this CWL v1.1?
#'
#' @param x CWL object
#'
#' @export is_v1.1
#'
#' @examples
#' library("tidycwl")
#' library("magrittr")
#'
#' system.file("cwl/sbg/workflow/rnaseq-salmon.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   is_v1.1()
is_v1.1 <- function(x) get_cwl_version(x) == "v1.1"

#' Is this a CWL workflow?
#'
#' @param x CWL object
#'
#' @export is_workflow
#'
#' @examples
#' library("tidycwl")
#' library("magrittr")
#'
#' system.file("cwl/sbg/workflow/gatk4-wgs.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   is_workflow()
#'
#' system.file("cwl/sbg/tool/bwa-mem.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   is_workflow()
is_workflow <- function(x) if (tolower(parse_type(x)) == "workflow") TRUE else FALSE

#' Is this a CWL command line tool?
#'
#' @param x CWL object
#'
#' @export is_tool
#'
#' @examples
#' library("tidycwl")
#' library("magrittr")
#'
#' system.file("cwl/sbg/tool/bwa-mem.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   is_tool()
#'
#' system.file("cwl/sbg/workflow/gatk4-wgs.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   is_tool()
is_tool <- function(x) if (tolower(parse_type(x)) == "commandlinetool") TRUE else FALSE

# is the inputs/outputs/steps component in the cwl object represented by a dict?
is_cwl_dict <- function(steps) !is.null(names(steps))

# is the inputs/outputs/steps component in the cwl object represented by a list?
is_cwl_list <- function(steps) is.null(names(steps)) & all(!sapply((sapply(steps, "[", "id")), is.null))

# remove named element(s) from all components in a list
remove_from_list <- function(lst, el) {
  for (i in 1:length(lst)) {
    for (j in el) {
      lst[[i]][[j]] <- NULL
    }
  }
  lst
}

# remove named variables from a data frame
remove_from_df <- function(df, var) {
  for (i in var) df[, i] <- NULL
  df
}

.inputs_to_remove <- c("sbg:x", "sbg:y", "sbg:suggestedValue", "sbg:stageInput")
.outputs_to_remove <- c("sbg:x", "sbg:y")

# remove unused data and things that caused list to data frame
# conversion issues in inputs list
sanitize_inputs_list <- function(inputs)
  remove_from_list(inputs, .inputs_to_remove)

# remove unused data in inputs data frame
sanitize_inputs_df <- function(inputs)
  remove_from_df(inputs, .inputs_to_remove)

# remove unused data in outputs list
sanitize_outputs_list <- function(outputs)
  remove_from_list(outputs, .outputs_to_remove)

# remove unused data in outputs data frame
sanitize_outputs_df <- function(outputs)
  remove_from_df(outputs, .outputs_to_remove)

# convert list to data frame
list2df <- function(lst) as.data.frame(dplyr::bind_rows(lst))

# is this literally a list?
is_literal_list <- function(x) "list" %in% class(x)

# is this literally a data frame?
is_literal_df <- function(x) "data.frame" %in% class(x)

# get named element from all components in a list
get_el_from_list <- function(x, name) {
  # get the elements
  obj <- sapply(x, "[[", name)
  # replace NULL as NA (in case we need to unlist
  # this so the element doesn't get dropped)
  obj[sapply(obj, is.null)] <- NA
  obj
}
