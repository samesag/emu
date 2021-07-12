#' A function to calculate the quantile from a numeric variable
#'
#' This function allows you to calculate the quantile position from a variable in a dataframe
#' @param df The dataframe containing the EMU information.
#' @param var The column name of the variable to calculate the quantile position.
#' @param min.seq The minimum value to compute the probablities of the quantile (default to 0)
#' @param max.seq The maximum value to compute the probablities of the quantile (default to 1)
#' @param by The interval to compute the probabilities calculation (default to 0.25 for 4 quartiles)
#' @param labels Logical. If labels should be calculated, instead of the brackets position
#' @param labels.mark Interval. Labels for the quartiles calculated depending on the intervals used as inputs (default 1:4)
#' @param include.lowest Logical. If lowest value from the interval should be included
#' @keywords quartile
#' @return The column with the quartile computed
#' @examples
#' quantile_emu(bd, var = value)
#' @export

quantile_emu <- function(df, var, min.seq, max.seq, by, labels, labels.mark,
                         include.lowest){

  if(!missing(var)) var <- rlang::enquo(var)
  if(missing(min.seq)) min.seq <- 0
  if(missing(max.seq)) max.seq <- 1
  if(missing(by)) by <- 0.25
  if(missing(labels)) labels <- T
  if(missing(labels.mark)) labels.mark <- 1:4
  if(missing(include.lowest)) include.lowest <- T

  if (missing(var)) {
    stop("Please specify a variable to calculate the quantile")
  }

  if (labels == T){
    column <- df %>%
      pull(!!var) %>%
      cut(breaks = quantile(., probs = seq(min.seq, max.seq, by = by), na.rm = TRUE),
          include.lowest = include.lowest,
          labels = labels.mark)
  }

  if (labels == F){
    column <- df %>%
      pull(!!var) %>%
      cut(breaks = quantile(., probs = seq(min.seq, max.seq, by = by), na.rm = TRUE),
          include.lowest = include.lowest)
  }
  return(column)

}
