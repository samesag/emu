#' A function to paste the input for street in geocode_Colombia
#'
#' This function allows you to paste the information from "Calle" and "Carrera" as an input for geocode_Colombia
#' @param df The dataframe containing the informaiton.
#' @keywords geocode_Colombia geocode Colombia
#' @return The address.
#' @examples
#' paste_addresses(df)
#' @export

paste_addresses <- function(BD) {

  calle <- c("calle_hogar", "calle_hogar_compl", "calle_hogar_suf")
  carrera <- c("carrera_hogar", "carrera_hogar_compl", "carrera_hogar_suf")
  Calles <- BD[, calle]
  Carreras <- BD[, carrera]

  Calles[4] <- Calles[3]
  Calles[3] <- apply(Calles[,1:2], 1,function(i){ paste(na.omit(i), collapse = "") })
  Calles[5] <- apply(Calles[,3:4], 1,function(i){ paste(na.omit(i), collapse = " ") })

  Carreras[4] <- Carreras[3]
  Carreras[3] <- apply(Carreras[,1:2], 1,function(i){ paste(na.omit(i), collapse = "") })
  Carreras[5] <- apply(Carreras[,3:4], 1,function(i){ paste(na.omit(i), collapse = " ") })

  Direccion <- cbind(Calles[5],Carreras[5])
  Direccion$final <- apply(Direccion[,1:2], 1,function(i){ paste(na.omit(i), collapse = " ") })
  Direccion[4] <- Direccion$final
  Direccion[3] <- ifelse(Direccion$final == " ", NA, "CL ")
  Direccion$total <- apply(Direccion[,3:4], 1,function(i){ paste(na.omit(i), collapse = "") })

  BD$Direccion <- Direccion$total

  return(BD)

}




