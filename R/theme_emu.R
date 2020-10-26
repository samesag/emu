#' Theme for EMU graphs
#'
#' This function gives a nice looking theme for ggplot graphs.
#' @param base_size Size for letters.
#' @param base_family Font used for the elements on the plot, different from those inserted with geom_text.
#' @param base_line_size Line size
#' @param base_rect_size Base rect size
#' @keywords theme ggplot emu
#' @return A theme for ggplot graphics.
#' @examples
#' theme(base_size = 30, base_family = "Gotham Rounded Book")
#' @export

theme_emu <- function (base_size = 11, base_family = NULL, base_line_size = base_size/22,
                         base_rect_size = base_size/22)
{
  if (missing(base_family)) {base_family <- "Franklin Gothic Book"}
  theme_bw(base_size = base_size, base_family = base_family,
           base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(legend.background = element_blank(),
          legend.key = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(colour = "grey", size = rel(1)),
          panel.grid.major = element_line(size = rel(0.5)),
          panel.grid.minor = element_line(size = rel(0.25)),
          complete = TRUE)
}
