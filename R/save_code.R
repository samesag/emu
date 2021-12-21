#' Save Apollo code and rename older ones
#'
#' This function allows you to save Apollo code and rename older ones
#'
#' @param apollo_control The objetct with the apollo_control information
#' @keywords Apollo, save code
#' @return The code in a R file
#' @examples
#' save_code(apollo_control)
#' @export

save_code <- function (apollo_control)
{
  modName <- paste0(apollo_control$outputDirectory, "/", apollo_control$modelName)
  if (file.exists(paste0(modName, "_code.R"))) {
    n <- 1
    while (file.exists(paste0(modName, "_OLD", n, "_code.R"))) n <- n +
        1
    modNameOld <- paste0(modName, "_OLD", n)
    outFiles <- c("_code.R")
    for (i in outFiles) if (file.exists(paste0(modName,
                                               i))) {
      file.rename(from = paste0(modName, i), to = paste0(modNameOld,
                                                         i))
      cat("\nOld result file \"", paste0(modName, i),
          "\" \n renamed to: \"", paste0(modNameOld, i),
          "\"", sep = "")
    }
    cat("\n")
  }
  rstudioapi::documentSave()
}
