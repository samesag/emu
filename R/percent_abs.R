#' Labels in format percent and absolute for ggplot graphs
#'
#' This function gives a labels format based on percentage and absolute values.
#' @param x The labels.
#' @param ... Another aesthetical value.
#' @keywords percent absolute labs ggplot
#' @return Labs in percent and absolute format for ggplot graphics.
#' @examples
#' labels = percent_abs()
#' @export

percent_abs <- function(x, ...) {
  paste0(abs(100 * x), "%")
}
