#' A function to convert a range of values into their mean
#'
#' This function allows you to convert each range into their mean, even if it has string elements, make sure your
#' delete any don't wanted numbers in the range.
#' @param df The dataframe containing the variables you want to changes.
#' @param vars a variable (or vector of variables) quoted that you want to change from a range to their mean.
#' @keywords range mean
#' @return The mean of \code{vars}.
#' @examples
#' range_as_mean(df, "salary")
#' @export

range_as_mean <- function(df, vars){

  library(stringr)

  for (i in vars) {
    var <- i
    base1 <- str_extract_all(df[[as.character(var)]],"\\(?[0-9,.]+\\)?")
    base1 <- data.frame(do.call("rbind", base1))
    base1[["X1"]] <-  gsub('\\.', '', base1[["X1"]]); base1[["X1"]] <- as.numeric(base1[["X1"]])
    base1[["X2"]] <-  gsub('\\.', '', base1[["X2"]]); base1[["X2"]] <- as.numeric(base1[["X2"]])
    df[[as.character(var)]] <- rowMeans(base1)
    rm(base1, var, i)
  }
  return(df)
  rm(vars)
}
