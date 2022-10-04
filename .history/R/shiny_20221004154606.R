#' Shiny bindings for tidycwl
#'
#' Output and renderer functions for using tidycwl within Shiny apps and
#' interactive R Markdown documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{"100\%"},
#' \code{"600px"}, \code{"auto"}) or a number, which will be coerced to
#' a string and have \code{"px"} appended.
#' @param expr An expression that generates a CWL graph
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})?
#' This is useful if you want to save an expression in a variable.
#'
#' @return An output or render function that enables the use of the widget within Shiny apps.
#'
#' @importFrom visNetwork visNetworkOutput renderVisNetwork
#' @importFrom htmlwidgets shinyRenderWidget shinyWidgetOutput
#'
#' @name tidycwl_shiny
#'
#' @examples
#' if (interactive()) {
#'   library("shiny")
#'   library("tidycwl")
#'
#'   cwl_folder <- system.file("cwl/sbg/workflow/", package = "tidycwl")
#'   file_all <- list.files(cwl_folder)
#'   cwl_name <- file_all[which(tools::file_ext(file_all) == "json")]
#'
#'   ui <- fluidPage(
#'     selectInput("cwl_file", "Select a CWL file:", cwl_name),
#'     cwl_output("cwl_plot", height = "800px")
#'   )
#'
#'   server <- function(input, output, session) {
#'     output$cwl_plot <- render_cwl({
#'       flow <- paste0(cwl_folder, input$cwl_file) %>% read_cwl_json()
#'       get_graph(
#'         flow %>% parse_inputs(),
#'         flow %>% parse_outputs(),
#'         flow %>% parse_steps()
#'       ) %>% visualize_graph()
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#' @export
cwl_output <- function(outputId, width = "100%", height = "600px") {
  shinyWidgetOutput(outputId, "visNetwork", width, height, package = "visNetwork")
}

#' @rdname tidycwl_shiny
#' @export
render_cwl <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)
  shinyRenderWidget(expr, visNetworkOutput, env, quoted = TRUE)
}
