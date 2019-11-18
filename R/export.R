#' Export the workflow plot as HTML
#'
#' @param g Plot rendered by \code{\link{visualize_graph}}.
#' @param file File to save HTML into.
#' @param ... Additional parameters for \code{\link[visNetwork]{visSave}}.
#'
#' @return HTML file path
#'
#' @export export_html
#'
#' @importFrom visNetwork visSave
#'
#' @examples
#' if (interactive()) {
#'   file_html <- tempfile(fileext = ".html")
#'   flow <- system.file("cwl/sbg/workflow/gatk4-wgs.json", package = "tidycwl") %>% read_cwl_json()
#'   get_graph(
#'     flow %>% parse_inputs(),
#'     flow %>% parse_outputs(),
#'     flow %>% parse_steps()
#'   ) %>%
#'     visualize_graph() %>%
#'     export_html(file_html)
#' }
export_html <- function(g, file, ...) {
  visSave(g, file, ...)
  invisible(file)
}

#' Export the workflow plot as PNG, JPEG, or PDF files
#'
#' @param file_html File path to the HTML exported by \code{\link{export_html}}.
#' @param file_image File path to the output image.
#' Should end with \code{.png}, \code{.pdf}, or \code{.jpeg}.
#' @param ... Additional parameters for \code{\link[webshot]{webshot}}.
#'
#' @return Image file path
#'
#' @export export_image
#'
#' @importFrom webshot webshot
#'
#' @note This function uses \code{\link[webshot]{webshot}} to take
#' a screenshot for the rendered HTML of the graph.
#' It requires PhantomJS installed in your system.
#'
#' @examples
#' if (interactive()) {
#'   file_png <- tempfile(fileext = ".png")
#'   flow <- system.file("cwl/sbg/workflow/gatk4-wgs.json", package = "tidycwl") %>% read_cwl_json()
#'   get_graph(
#'     flow %>% parse_inputs(),
#'     flow %>% parse_outputs(),
#'     flow %>% parse_steps()
#'   ) %>%
#'     visualize_graph() %>%
#'     export_html(tempfile(fileext = ".html")) %>%
#'     export_image(file_png, vwidth = 2000, vheight = 3000, selector = "div.vis-network")
#' }
export_image <- function(file_html, file_image, ...) {
  file_url <- paste0("file://", normalizePath(file_html))
  webshot(url = file_url, file = file_image, ...)
  invisible(file_image)
}
