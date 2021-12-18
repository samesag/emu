#' Save Apollo image space and rename older ones
#'
#' This function allows you to save Apollo image spaces and rename older ones
#'
#' @param apollo_control The objetct with the apollo_control information
#' @keywords gApollo, save image
#' @return The workspace in a RData file
#' @examples
#' save_image(apollo_control)
#' @export

save_image <- function(apollo_control){

  modName <- paste0(apollo_control$outputDirectory, "/", apollo_control$modelName)

  if(file.exists(paste0(modName,"_data.RData"))) {
    n <- 1
    while (file.exists(paste0(modName, "_OLD", n, "_data.RData"))) n <- n +
        1
    modNameOld <- paste0(modName, "_OLD", n)
    outFiles <- c("_data.RData")
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
  save.image(file = paste0(modName,"_data.RData"))
}
