#' A function to calculate expansions factors in EMU
#'
#' This function allows you calculate expansion factors per poblation in EMU, with a dataframe containing population
#' @param df The dataframe containing the depurated EMU information
#' @param as.df If true, returns a dataframe with the information for the expansion factors
#' @param as.data If true, returns only the values of the expansion factors
#' @keywords expansion factors survey EMU
#' @return The expansion factors
#' @examples
#' factores_EMU(df, as.df, as.data)
#' @export

factores_EMU <- function(df, as.df){

  BD <- df
  if(missing(as.df)) as.df = T

  if(as.df == T){
    Factores <- BD %>%
      group_by(vinculo) %>%
      summarise(Respuestas = n(),
                Poblacion = mean(Poblacion)) %>%
      mutate(FE = Poblacion/Respuestas)
    return(Factores)}
  else if(as.df == F){
    Factores <- BD %>%
      group_by(vinculo) %>%
      summarise(Respuestas = n(),
                Poblacion = mean(Poblacion)) %>%
      mutate(FE = Poblacion/Respuestas)
    lista <- as.list(Factores$FE)
    names(lista) <- Factores$vinculo
    list2env(lista, globalenv())}
  else {
    stop("El argumento de salida debe ser verdadero o falso")}
}






