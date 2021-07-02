#' A function that allows piping ggsave
#'
#' @param NULL It doesn't require parameters
#' @keywords ggsave
#' @return A saved ggplot object
#' @examples
#' ggsave2()
#' @export

ggsave2 <- function(...) {
  require(tidyverse)
  ggplot2::ggsave(...)
  invisible()
}
