#' Changelog
#' ---  Saul E. Acevedo 9/28/2022
#'     --- Added support for CWL versions 1.1 and 1.2


#' Get nodes in a CWL workflow into a data frame
#'
#' @param inputs Parsed inputs
#' @param outputs Parsed outputs
#' @param steps Parsed steps
#'
#' @return Data frame containing node information
#'
#' @export get_nodes
#'
#' @examples
#' flow <- system.file("cwl/sbg/workflow/gatk4-wgs.json", package = "tidycwl") %>% read_cwl_json()
#' get_nodes(
#'   flow %>% parse_inputs(),
#'   flow %>% parse_outputs(),
#'   flow %>% parse_steps()
#' ) %>% str()
get_nodes <- function(inputs, outputs, steps) {
  # nodes - input/output nodes on the side + step nodes in the middle
  nodes <- data.frame(
    "id" = c(
      get_inputs_id(inputs),
      get_outputs_id(outputs),
      get_steps_id(steps)
    ),
    "label" = c(
      get_inputs_label(inputs),
      get_outputs_label(outputs),
      get_steps_label(steps)
    ),
    "group" = c(
      rep(input_str_gbl, length(get_inputs_id(inputs))),
      rep(output_str_gbl, length(get_outputs_id(outputs))),
      rep(step_str_gbl, length(get_steps_id(steps)))
    ),
    stringsAsFactors = FALSE
  )
  nodes$"id" <- remove_hashtag(nodes$"id")
  nodes
}

# logic for parsing edges:
# - only need to look into $outputs and $steps
# - for each node in $outputs, the only useful field is [outputSource:]
#     - Deal with the without slash case as [node from]
#       and with slash case as [node from]/[port from]
#     - [node to] is the corresponding node id for this $outputs item
#     - [port to] should be NA
# - for each node in $steps, the field [out:] is redudant and can be discarded
#   only need to look into the field [in:]
#   - for each [in:] item in the nodes of $steps
#     - if it is a list, iterate over the list, else consider the single item
#       (because one port of a node can get inputs from multiple other ports)
#     - for each item
#        - if it is a string, parse it as [node from]/[port from]
#        - else find `source` and parse it as [node from]/[port from]
#        - the `id` for each item is the name of [port to]
#        - [node to] is the corresponding node id for this [in:] item
#        - For the no slash case, it's just [node from], and [port from] is NA

#' Get edges in a CWL workflow into a data frame
#'
#' @param outputs Parsed outputs
#' @param steps Parsed steps
#'
#' @return Data frame containing edge information
#'
#' @export get_edges
#'
#' @examples
#' # edges represented by a dictionary
#' flow <- system.file("cwl/sbg/workflow/rnaseq-salmon.json", package = "tidycwl") %>% read_cwl_json()
#' get_edges(
#'   flow %>% parse_outputs(),
#'   flow %>% parse_steps()
#' ) %>% str()
#'
#' # edges represented by a list
#' try(
#'   flow <- system.file("cwl/sbg/workflow/rnaseq-salmon.cwl", package = "tidycwl") %>% read_cwl_yaml()
#' )
#' try(
#'   get_edges(
#'     flow %>% parse_outputs(),
#'     flow %>% parse_steps()
#'   ) %>% str()
#' )
get_edges <- function(outputs, steps) {
  # edges - only need to look into outputs and steps
  ver <- get_cwl_version_steps(steps)
  out_src_str <- "outputSource"

  # edges from outputs
  if (ver == ver_1_0_str_gbl) source_name <- out_src_str
    # Added if statements that set the source_name variable for CWL versions 1.1 and 1.2. S.E.A, 9/28/2022
  if (ver == ver_1_1_str_gbl) source_name <- out_src_str
  if (ver == ver_1_2_str_gbl) source_name <- out_src_str
  if (ver == sbg_draft_str_gbl) source_name <- source_str_gbl

  if (is_cwl_dict(outputs)) {
    output_source <- unlist(outputs[[source_name]])
  } else if (is_cwl_list(outputs)) {
    output_source <- unlist(get_el_from_list(outputs, source_name))
  } else {
    stop("`outputs` is not a proper dict or list")
  }

  df_edges_outputs <- read_edges_outputs(output_source, outputs, ver)
  in_str <- "in"

  # edges from steps
  if (ver == ver_1_0_str_gbl) in_name <- in_str
    # Added if statements that set the in_name variable for CWL versions 1.1 and 1.2. S.E.A, 9/28/2022
  if (ver == ver_1_1_str_gbl) in_name <- in_str
  if (ver == ver_1_2_str_gbl) in_name <- in_str
  if (ver == sbg_draft_str_gbl) in_name <- "inputs"

  if (is_cwl_dict(steps)) {
    steps_in <- steps[[in_name]]
  } else if (is_cwl_list(steps)) {
    steps_in <- get_el_from_list(steps, in_name)
    # convert to the same format as the dictionary case
    for (i in 1:length(steps_in)) steps_in[[i]] <- as.data.frame(dplyr::bind_rows(steps_in[[i]]))
  } else {
    stop("`steps` is not a proper dict or list")
  }

  df_edges_steps <- read_edges_steps(steps_in, steps, ver)

  # combine edges from outputs and steps
  edges <- rbind(df_edges_steps, df_edges_outputs)

  edges
}

#' Get the CWL workflow graph
#'
#' Get the CWL workflow graph as a list of two data frames:
#' a data frame of nodes and a data frame of edges.
#'
#' @param inputs Parsed inputs
#' @param outputs Parsed outputs
#' @param steps Parsed steps
#'
#' @return List of two data frames containing node and edge information
#'
#' @export get_graph
#'
#' @examples
#' # sbg:draft2
#' flow <- system.file("cwl/sbg/workflow/gatk4-wgs.json", package = "tidycwl") %>% read_cwl_json()
#' get_graph(
#'   flow %>% parse_inputs(),
#'   flow %>% parse_outputs(),
#'   flow %>% parse_steps()
#' ) %>% str()
#'
#' # v1.0
#' flow <- system.file("cwl/sbg/workflow/rnaseq-salmon.json", package = "tidycwl") %>% read_cwl_json()
#' get_graph(
#'   flow %>% parse_inputs(),
#'   flow %>% parse_outputs(),
#'   flow %>% parse_steps()
#' ) %>% str()
get_graph <- function(inputs, outputs, steps) {
  nodes <- get_nodes(inputs, outputs, steps)
  edges <- get_edges(outputs, steps)

  list(
    "nodes" = nodes,
    "edges" = edges
  )
}
