# get cwl version from steps (the hard way)
get_cwl_version_steps <- function(steps) {
  ver <- NULL
  if ("v1.0" %in% c(steps$run$cwlVersion, steps$cwlVersion)) ver <- "v1.0"
  if ("sbg:draft-2" %in% c(steps$run$cwlVersion, steps$cwlVersion)) ver <- "sbg:draft-2"
  if (is.null(ver)) {
    if ("v1.0" %in% get_el_from_list(get_el_from_list(steps, "run"), "cwlVersion")) ver <- "v1.0"
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

# read edges from $outputs
read_edges_outputs <- function(output_source, outputs, cwl_version) {
  if (cwl_version == "v1.0") sep <- "/"
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

# read edges from $steps$in
read_edges_steps <- function(steps_in, steps, cwl_version) {
  if (cwl_version == "v1.0") sep <- "/"
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
      key <- steps_in[[i]][j, "id"]
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
