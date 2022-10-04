#' Changelog
#' ---  Saul E. Acevedo 9/29/2022
#'     --- Added is_v1.2 function
#'     --- Added replace_labels_if_na function


#' Is this a CWL object?
#'
#' @param x any object
#'
#' @return Logical. \code{TRUE} if it is a CWL object, \code{FALSE} if not.
#'
#' @export is_cwl
#'
#' @examples
#' system.file("cwl/sbg/workflow/gatk4-wgs.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   is_cwl()
is_cwl <- function(x) if ("cwl" %in% class(x)) TRUE else FALSE

#' Is this CWL draft2?
#'
#' @param x CWL object
#'
#' @return Logical. \code{TRUE} if it is a CWL draft2 object, \code{FALSE} if not.
#'
#' @export is_draft2
#'
#' @examples
#' system.file("cwl/sbg/workflow/gatk4-wgs.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   is_draft2()
#'
#' system.file("cwl/sbg/workflow/rnaseq-salmon.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   is_draft2()
is_draft2 <- function(x) get_cwl_version(x) == sbg_draft_str_gbl

#' Is this CWL v1.0?
#'
#' @param x CWL object
#'
#' @return Logical. \code{TRUE} if it is a CWL v1.0 object, \code{FALSE} if not.
#'
#' @export is_v1.0
#'
#' @examples
#' system.file("cwl/sbg/workflow/gatk4-wgs.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   is_v1.0()
#'
#' system.file("cwl/sbg/workflow/rnaseq-salmon.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   is_v1.0()
is_v1.0 <- function(x) get_cwl_version(x) == ver_1_0_str_gbl

#' Is this CWL v1.1?
#'
#' @param x CWL object
#'
#' @return Logical. \code{TRUE} if it is a CWL v1.1 object, \code{FALSE} if not.
#'
#' @export is_v1.1
#'
#' @examples
#' system.file("cwl/sbg/workflow/rnaseq-salmon.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   is_v1.1()
is_v1.1 <- function(x) get_cwl_version(x) == ver_1_1_str_gbl

#' Is this CWL v1.2?
#'
#' @param x CWL object
#'
#' @return Logical. \code{TRUE} if it is a CWL v1.2 object, \code{FALSE} if not.
#'
#' @export is_v1.2
#'
#' @examples
#' system.file("cwl/sbg/workflow/rnaseq-salmon.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   is_v1.2()
is_v1.2 <- function(x) get_cwl_version(x) == ver_1_2_str_gbl

#' Is this a CWL workflow?
#'
#' @param x CWL object
#'
#' @return Logical. \code{TRUE} if it is a CWL workflow
#' (instead of a command line tool), \code{FALSE} if not.
#'
#' @export is_workflow
#'
#' @examples
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
#' @return Logical. \code{TRUE} if it is a CWL command line tool
#' (instead of a workflow), \code{FALSE} if not.
#'
#' @export is_tool
#'
#' @examples
#' system.file("cwl/sbg/tool/bwa-mem.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   is_tool()
#'
#' system.file("cwl/sbg/workflow/gatk4-wgs.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   is_tool()
is_tool <- function(x) if (tolower(parse_type(x)) == "commandlinetool") TRUE else FALSE

#' is the inputs/outputs/steps component in the cwl object represented by a dict?
#'
#' @param steps Parsed steps
#'
#' @return Logical. \code{TRUE} if the component in the cwl object is represented by a dict
#' ,\code{FALSE} if not.
#'
#' @export is_cwl_dict
#' 
#' @examples
#' system.file("cwl/sbg/tool/bwa-mem.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   parse_steps() %>%
#'   is_cwl_dict()
#'
#' system.file("cwl/sbg/workflow/gatk4-wgs.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   parse_steps() %>%
#'   is_cwl_dict()
is_cwl_dict <- function(steps) !is.null(names(steps))

#' is the inputs/outputs/steps component in the cwl object represented by a list?
#' 
#' @param steps Parsed steps
#'
#' @return Logical. \code{TRUE} if the component in the cwl object is represented by a list
#' ,\code{FALSE} if not.
#'
#' @export is_cwl_list
#' 
#' @examples
#' system.file("cwl/sbg/tool/bwa-mem.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   parse_steps() %>%
#'   is_cwl_list()
#'
#' system.file("cwl/sbg/workflow/gatk4-wgs.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   parse_steps() %>%
#'   is_cwl_list()
is_cwl_list <- function(steps) is.null(names(steps)) & all(!sapply((sapply(steps, "[", id_str_gbl)), is.null))

#' remove named element(s) from all components in a list
#'
#' @param lst listobject
#' @param el named variables
#'
#' @return List object with named variables removed
#'
#' @export remove_from_list
remove_from_list <- function(lst, el) {
  for (i in 1:length(lst)) {
    for (j in el) {
      lst[[i]][[j]] <- NULL
    }
  }
  lst
}

#' remove named variables from a data frame
#' 
#' @param df dataframe object
#' @param var named variables
#'
#' @return Dataframe object with named variables removed
#'
#' @export remove_from_df 
remove_from_df <- function(df, var) {
  for (i in var) df[, i] <- NULL
  df
}

.inputs_to_remove <- c("sbg:x", "sbg:y", "sbg:suggestedValue", "sbg:stageInput")
.outputs_to_remove <- c("sbg:x", "sbg:y")

#'remove unused data and things that caused list to data frame
# conversion issues in inputs list
#' 
#' @param inputs Parsed inputs
#'
#' @return Sanitized list of inputs
#'
#' @export sanitize_inputs_list
#' 
#' @examples
#' system.file("cwl/sbg/tool/bwa-mem.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   parse_inputs() %>%
#'   sanitize_inputs_list()
#'
#' system.file("cwl/sbg/workflow/gatk4-wgs.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   parse_inputs() %>%
#'   sanitize_inputs_list()
sanitize_inputs_list <- function(inputs) {
  remove_from_list(inputs, .inputs_to_remove)
}

#' remove unused data in inputs data frame
#' 
#' @param inputs Parsed inputs
#'
#' @return Sanitized df of inputs
#'
#' @export sanitize_inputs_df
#' 
#' @examples
#' system.file("cwl/sbg/tool/bwa-mem.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   parse_inputs() %>%
#'   sanitize_inputs_df()
#'
#' system.file("cwl/sbg/workflow/gatk4-wgs.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   parse_inputs() %>%
#'   sanitize_inputs_df()
sanitize_inputs_df <- function(inputs) {
  remove_from_df(inputs, .inputs_to_remove)
}

#' remove unused data in outputs list
#' 
#' @param outputs Parsed outputs
#'
#' @return Sanitized list of outputs
#'
#' @export sanitize_outputs_list
#' 
#' @examples
#' system.file("cwl/sbg/tool/bwa-mem.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   parse_outputs() %>%
#'   sanitize_outputs_list()
#'
#' system.file("cwl/sbg/workflow/gatk4-wgs.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   parse_outputs() %>%
#'   sanitize_outputs_list()
sanitize_outputs_list <- function(outputs) {
  remove_from_list(outputs, .outputs_to_remove)
}

#' remove unused data in outputs data frame
#' 
#' @param outputs Parsed outputs
#'
#' @return Sanitized df of outputs
#'
#' @export sanitize_outputs_list
#' 
#' @examples
#' system.file("cwl/sbg/tool/bwa-mem.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   parse_inputs() %>%
#'   sanitize_outputs_df()
#'
#' system.file("cwl/sbg/workflow/gatk4-wgs.json", package = "tidycwl") %>%
#'   read_cwl(format = "json") %>%
#'   parse_inputs() %>%
#'   sanitize_outputs_df()
sanitize_outputs_df <- function(outputs) {
  remove_from_df(outputs, .outputs_to_remove)
}

# clean the metadata fields
# metadata should include a field called either "docs" or "description" with "docs" being CWL 1.0 compliant
sanitize_metadata_list <- function(metadata_list) {
  if (is.null(metadata_list$doc)) {
    metadata_list[doc_str_gbl] <- NULL
  }
  if (is.null(metadata_list$description)) {
    metadata_list[desc_str_gbl] <- NULL
  }

  # If neither doc or description are present, then create a NULL doc field
  if(!(doc_str_gbl %in% names(metadata_list)) && !(desc_str_gbl %in% names(metadata_list))) {
    metadata_list[doc_str_gbl] <- list(NULL)
  }
  return(metadata_list)
}

#' convert list to data frame
#' 
#' @param lst list object
#'
#' @return return converted dataframe
#'
#' @export list2df
list2df <- function(lst) as.data.frame(dplyr::bind_rows(lst))

#' is this literally a list?
#' 
#' @param x any object
#'
#' @return  Logical. \code{TRUE} if it is a list object, \code{FALSE} if not.
#'
#' @export is_literal_list
is_literal_list <- function(x) "list" %in% class(x)

#' is this literally a data frame?
#'
#' @param x any object
#'
#' @return  Logical. \code{TRUE} if it is a dataframe object, \code{FALSE} if not.
#'
#' @export is_literal_df
is_literal_df <- function(x) "data.frame" %in% class(x)

#' get named element from all components in a list
#' 
#' @param x any object
#' @param name element names
#'
#' @return named elements from all components in a list
#'
#' @export get_el_from_list
get_el_from_list <- function(x, name) {
  # get the elements
  obj <- sapply(x, "[[", name)
  # replace NULL as NA (in case we need to unlist
  # this so the element doesn't get dropped)
  obj[sapply(obj, is.null)] <- NA
  obj
}

#' If label values are NA, replace with id values. S.E.A, 9/29/2022
#'
#' @param lst json or yaml file object
#'
#' @return json or yaml file object with underscores in substituted label values removed
#'
#' @export replace_labels_if_na
#'
#' @examples
#' If json file
#' system.file("cwl/sbg/workflow/rnaseq-salmon.cwl", package = "tidycwl") %>%
#'   jsonlite::fromJSON() %>%
#'   replace_labels_if_na()
#' #' If yaml file
#' system.file("cwl/sbg/workflow/rnaseq-salmon.cwl", package = "tidycwl") %>%
#'   yaml::read_yaml() %>%
#'   replace_labels_if_na()
replace_labels_if_na <- function(lst) {
  if (!is.null(lst$inputs$label) && !is.null(lst$inputs$id)){
    lst$inputs <- lst$inputs %>% mutate(label = coalesce(label, id))
    lst$inputs$label <- remove_hashtag(lst$inputs$label)
    lst$inputs$label <- remove_underscores(lst$inputs$label)
  }
  if (!is.null(lst$outputs$label) && !is.null(lst$outputs$id)){
    lst$outputs <- lst$outputs %>% mutate(label = coalesce(label, id))
    lst$outputs$label <- remove_hashtag(lst$outputs$label)
    lst$outputs$label <- remove_underscores(lst$outputs$label)
  }
  lst
}
