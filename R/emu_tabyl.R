#' A function to create weighted crosstables
#'
#' This function allows you to create pivot tables for EMU, expanded and non expanded.
#' @param df The dataframe containing the EMU information.
#' @param var1 the column name of the first variable.
#' @param var2 (optional) the column name of the second variable (the rows in a 2-way tabulation).
#' @param var3 (optional) the column name of the third variable (the list in a 3-way tabulation).
#' @param expansion the column name with the expansion factor to be used in the summarise
#' @param show_na should counts of NA values be displayed? In a one-way tabyl, the presence of NA values triggers an additional column showing valid percentages(calculated excluding NA values).
#' @keywords pivot table expansion factor
#' @return The pivot table with the expansion variable.
#' @examples
#' emu_tabyl(bd, Antes_viajes_2_6, show_na = F)
#' @export

emu_tabyl <- function(df, var1, var2, var3, expansion = f_exp, show_na = T, ...){

  if(!missing(var1)) var1 <- rlang::enquo(var1)
  if(!missing(var2)) var2 <- rlang::enquo(var2)
  if(!missing(var3)) var3 <- rlang::enquo(var3)
  expansion <- rlang::enquo(expansion)

  if (missing(var1) && missing(var2) && missing(var3)) {
    stop("if calling on a data.frame, specify unquoted column names(s) to tabulate.  Did you mean to call tabyl() on a vector?")
  }
  if (dplyr::is_grouped_df(df)) {
    df <- dplyr::ungroup(dat)
  }
  if (missing(var2) && missing(var3) && !missing(var1)) {
    if (show_na == F) {
      df <- df %>%
        filter(!is.na(!!var1))
    }
    df2 <- df %>%
      group_by(!!var1) %>%
      summarise(n = sum(!!expansion, na.rm = T)) %>%
      mutate(percent = n/sum(n))
  }
  else if (missing(var3) && !missing(var1) && !missing(var1)) {
    if (show_na == F) {
      df <- df %>%
        filter(!is.na(!!var1), !is.na(!!var2))
    }
    df2 <- df %>%
      group_by(!!var1, !!var2) %>%
      summarise(n = sum(!!expansion, na.rm = T)) %>%
      mutate(percent = n/sum(n))
  }
  else if (!missing(var1) && !missing(var2) && !missing(var3)) {
    if (show_na == F) {
      df <- df %>%
        filter(!is.na(!!var1), !is.na(!!var2), !is.na(var3))
    }
    df2 <- df %>%
      group_by(!!var1, !!var2, !!var3) %>%
      summarise(n = sum(!!expansion, na.rm = T)) %>%
      mutate(percent = n/sum(n))
  }
  else {
    stop("please specify var1 OR var1 & var2 OR var1 & var2 & var3")
  }
  return(df2)
}
