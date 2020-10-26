#' A function that retreives specific colors for EMU
#'
#' This function creates 4 vectors with categorical colors for the EMU
#' @param NULL It doesn't require parameters
#' @keywords palette colors EMU
#' @return The four color vectors
#' @examples
#' colores_EMU()
#' @export

colores_EMU <- function(){

paleta = c("Transmilenio" = "#C00001", "SITP" = "#5B9BD5", "SITP Provisional" = "#D3E0EA", "Bus intermunicipal" = "grey",
           "Carro como conductor" = "#525252", "Carro como pasajero" = "#7C7C7C", "Carro compartido" = "#A5A5A5",
           "Caminata" = "#548235", "Bicicleta" = "#92D050", "Patineta eléctrica" = "green",
           "Aplicaciones" = "#44546A", "Taxi" = "#FFC001", "Moto" = "#ED7D31", "Otro" = "black",
           "Automóvil" = "#525252", "Carro" = "#525252")

paleta_Texto = c("Transmilenio" = "white", "SITP" = "black", "SITP Provisional" = "black", "Bus intermunicipal" = "black",
           "Carro como conductor" = "white", "Carro como pasajero" = "black", "Carro compartido" = "black",
           "Caminata" = "white", "Bicicleta" = "black", "Patineta eléctrica" = "black",
           "Aplicaciones" = "white", "Taxi" = "black", "Moto" = "black", "Otro" = "black", "Automóvil" = "white",
           "Carro" = "white")

paleta_Tipos = c("Transporte público" = "#BDD7EE", "Carro" = "#525252", "Modos activos" = "#C5E0B4",
            "Aplicaciones y taxi" = "#FFE698", "Moto" = "#F8CBAD", "Otro" = "black", "Automóvil" = "#525252",
            "Carro" = "#525252")

paleta_Tipos_Texto = c("Transporte público" = "black", "Carro" = "white", "Modos activos" = "black",
           "Aplicaciones y taxi" = "black", "Moto" = "black", "Otro" = "white", "Automóvil" = "white",
           "Carro" = "white")

colores <- list(paleta, paleta_Texto, paleta_Tipos, paleta_Tipos_Texto)
names(colores) <- c("paleta", "paleta_Texto", "paleta_Tipos", "paleta_Tipos_Texto")
list2env(colores, globalenv())

}





