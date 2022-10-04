#' Changelog
#' ---  Saul E. Acevedo 9/28/2022
#'     --- Added support for CWL versions 1.1 and 1.2
#'     --- Added remove_underscores function
#'     --- Added additional documentation to functions


#' get cwl version from steps (the hard way)
#' 
#' @param steps Parsed steps
#' 
#' @return Returns a string that describes the version of CWL in file
#' 
#' @export get_cwl_version_steps
#' 
#' @examples
#' system.file("cwl/sbg/workflow/rnaseq-salmon.json", package = "tidycwl") %>%
#'   read_cwl_json() %>%
#'   parse_steps() %>%
#'   get_cwl_version_steps()
get_cwl_version_steps <- function(steps) {
  ver <- NULL
  if ("v1.0" %in% c(steps$run$cwlVersion, steps$cwlVersion)) ver <- "v1.0"
  if ("v1.1" %in% c(steps$run$cwlVersion, steps$cwlVersion)) ver <- "v1.1"
  if ("v1.2" %in% c(steps$run$cwlVersion, steps$cwlVersion)) ver <- "v1.2"
  if ("sbg:draft-2" %in% c(steps$run$cwlVersion, steps$cwlVersion)) ver <- "sbg:draft-2"
  if (is.null(ver)) {
    if ("v1.0" %in% get_el_from_list(get_el_from_list(steps, "run"), "cwlVersion")) ver <- "v1.0"
    if ("v1.1" %in% get_el_from_list(get_el_from_list(steps, "run"), "cwlVersion")) ver <- "v1.1"
    if ("v1.2" %in% get_el_from_list(get_el_from_list(steps, "run"), "cwlVersion")) ver <- "v1.2"
    if ("sbg:draft-2" %in% get_el_from_list(get_el_from_list(steps, "run"), "cwlVersion")) ver <- "sbg:draft-2"
  }
  ver
}

# post-processing for node/edge ids: in case they have a leading hashtag (#)
remove_hashtag <- function(x) {
  idx <- which(substr(x, 1, 1) == "#")
  x[idx] <- substring(x[idx], 2)
  x
}

remove_hashtag_df <- function(df) {
  for (i in 1:ncol(df)) df[, i] <- remove_hashtag(df[, i])
  df
}

remove_underscores <- function(x) {
  x <- gsub("_", " ", x)
  x
}


#' read edges from $outputs
#' 
#' @param output_source Parsed output source
#' @param outputs Parsed outputs
#' @param cwl_version Parsed cwl version
#' 
#' @return Returns dataframe with output node data
#' 
#' @export read_edges_outputs
#' 
#' @examples
#' #' # outputs represented by a dictionary
#' file <- system.file("cwl/sbg/workflow/rnaseq-salmon.json", package = "tidycwl")
#' outputs <- file %>% parse_outputs()
#' steps <- file %>% parse_steps()
#' ver <- get_cwl_version_steps(steps)
#' output_source <- unlist(outputs[[source_name]])
#' read_edges_outputs(
#'    output_source,
#'    outputs,
#'    ver
#' )
#'
#' # outouts represented by a list
#' system.file("cwl/sbg/workflow/rnaseq-salmon.cwl", package = "tidycwl") %>%
#' outputs <- file %>% parse_outputs()
#' steps <- file %>% parse_steps()
#' ver <- get_cwl_version_steps(steps)
#' output_source <- unlist(get_el_from_list(outputs, source_name))
#' read_edges_outputs(
#'    output_source,
#'    outputs,
#'    ver
#' )
read_edges_outputs <- function(output_source, outputs, cwl_version) {
  if (cwl_version == "v1.0") sep <- "/"
  if (cwl_version == "v1.1") sep <- "/"
  if (cwl_version == "v1.2") sep <- "/"
  if (cwl_version == "sbg:draft-2") sep <- "\\."

  df <- data.frame(
    "from" = character(),
    "to" = character(),
    "port_from" = character(),
    "port_to" = character(),
    "type" = character(),
    stringsAsFactors = FALSE
  )

  outputs_id <- outputs %>% get_outputs_id()

  for (i in 1:length(outputs_id)) {
    if (grepl(sep, output_source[i])) {
      val_vec <- strsplit(output_source[i], sep)[[1]]
      df[i, "from"] <- val_vec[1]
      df[i, "to"] <- outputs_id[i]
      df[i, "port_from"] <- val_vec[2]
      df[i, "port_to"] <- NA
      df[i, "type"] <- "step_to_output"
    } else {
      df[i, "from"] <- output_source[i]
      df[i, "to"] <- outputs_id[i]
      df[i, "port_from"] <- NA
      df[i, "port_to"] <- NA
      df[i, "type"] <- "step_to_output"
    }
  }

  df %>% remove_hashtag_df()
}

#' read edges from $steps$in
#' 
#' @param steps_in inputs for steps
#' @param steps Parsed steps
#' @param cwl_version Parsed cwl version
#' 
#' @return Returns dataframe with step edges data
#' 
#' @export read_edges_steps
#' 
#' @examples
#' #' # steps represented by a dictionary
#' file <- system.file("cwl/sbg/workflow/rnaseq-salmon.json", package = "tidycwl")
#' steps <- file %>% parse_steps()
#' steps_in <- steps[[in_name]]
#' ver <- get_cwl_version_steps(steps)
#' output_source <- unlist(outputs[[source_name]])
#' read_edges_steps(
#'    steps_in,
#'    steps,
#'    ver
#' )
#'
#' # steps represented by a list
#' system.file("cwl/sbg/workflow/rnaseq-salmon.cwl", package = "tidycwl") %>%
#' steps <- file %>% parse_steps()
#' steps_in <- get_el_from_list(steps, in_name)
#' ver <- get_cwl_version_steps(steps)
#' read_edges_steps(
#'    steps_in,
#'    steps,
#'    ver
#' )
read_edges_steps <- function(steps_in, steps, cwl_version) {
  if (cwl_version == "v1.0") sep <- "/"
  if (cwl_version == "v1.1") sep <- "/"
  if (cwl_version == "v1.2") sep <- "/"
  if (cwl_version == "sbg:draft-2") sep <- "\\."

  df <- data.frame(
    "from" = character(),
    "to" = character(),
    "port_from" = character(),
    "port_to" = character(),
    "type" = character(),
    stringsAsFactors = FALSE
  )

  steps_id <- steps %>% get_steps_id()

  # replacing every null with NA so that is.na() won't give logical(0)
  for (i in 1:length(steps_in)) {
    steps_in[[i]][sapply(steps_in[[i]]$"source", is.null), "source"] <- NA
  }

  for (i in 1:length(steps_id)) {
    for (j in 1:nrow(steps_in[[i]])) {
      key <- steps_in[[i]][j, id_str_gbl]
      key_vec <- strsplit(key, sep)[[1]]

      val <- unlist(steps_in[[i]][j, "source"])
      # iterate over the value list (if any) because one
      # port j of node i could receive inputs from multiple upstream ports
      for (k in 1:length(val)) {
        # only attach when the edge source is not NULL or NA
        if (!is.na(val[k])) {
          if (grepl(sep, val[k])) {
            val_vec <- strsplit(val[k], sep)[[1]]
            tmp <- data.frame(
              "from" = val_vec[1],
              "to" = steps_id[i],
              "port_from" = val_vec[2],
              "port_to" = key_vec[2],
              "type" = "step_to_step",
              stringsAsFactors = FALSE
            )
            df <- rbind(df, tmp)
          } else {
            tmp <- data.frame(
              "from" = val[k],
              "to" = steps_id[i],
              "port_from" = NA,
              "port_to" = key_vec[2],
              "type" = "input_to_step",
              stringsAsFactors = FALSE
            )
            df <- rbind(df, tmp)
          }
        }
      }
    }
  }

  df %>% remove_hashtag_df()
}
