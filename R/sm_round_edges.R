#' Round edges for ggplot graphs
#'
#' This function allows you to round the edges of a ggplot graph.
#' @param plot The resulting ggplot graph.
#' @param radio The radio in percentage for rounding the edges.
#' @keywords round edges ggplot
#' @return The plot with rounded edges.
#' @examples
#' geocode_Colombia(plot, r = 0.05)
#' @export

sm_round_edges <- function(plot = NULL, radio = NULL){

  library(grid)
  library(ggplotify)

  if (missing(plot)){plot = last_plot()}
  if(missing(radio)){radio <- 0.05}

  g <- plot
  g <- ggplotGrob(g)
  bg <- g$grobs[[1]]
  round_bg <- roundrectGrob(x=bg$x, y=bg$y, width=bg$width, height=bg$height,
                            r=unit(radio, "snpc"),
                            just=bg$just, name=bg$name, gp=bg$gp, vp=bg$vp)
  g$grobs[[1]] <- round_bg
  d <- as.ggplot(g)
  return(d)

}
