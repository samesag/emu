#' Yes/No answer to binary
#'
#' This function allows you to convert multiple yes/no answers in binary format
#' @param df The dataframe containing the information.
#' @param vars a variable (or vector of variables) quoted that you want to change to binary.
#' @keywords binary answer survey
#' @return The binary answers, changing "no" or "none" to 0 and "yes" to 1.
#' @examples
#' answer_as_binary(df, "tenencia_bici")
#' @export
answer_as_binary <- function(df, vars){

  for (i in vars) {
    var <- i
    df[[as.character(var)]][df[[as.character(var)]] == NA] <- NA_real_
    df[[as.character(var)]][df[[as.character(var)]] == "Ninguno (0)"] <- 0
    df[[as.character(var)]][df[[as.character(var)]] == "No"] <- 0
    df[[as.character(var)]][df[[as.character(var)]] == "Sí"] <- 1
    df[[as.character(var)]][df[[as.character(var)]] != 0] <- 1
    df[[as.character(var)]] <- as.numeric(df[[as.character(var)]])
  }
  rm(i, var, vars)
  return(df)

}
