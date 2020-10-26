#' A function to create pivot tables
#'
#' This function allows you to create pivot tables for EMU, expanded and non expanded.
#' @param df The dataframe containing the EMU information.
#' @param vars a variable (or vector of variables) quoted that you want use as input for pivot table.
#' @param expanded T if expanded, F if unexpanded. If missins, asummes TRUE.
#' @keywords pivot table EMU
#' @return The pivot table.
#' @examples
#' pivot_table_EMU(df, "hacia_modo_ppal", TRUE)
#' @export

pivot_table_EMU <- function(df, vars, expanded, digits.expanded, digits.percentage) {

  library(tidyverse)
  library(janitor)

  BD <- df
  if(missing(digits.expanded)) digits.expanded = 1
  if(missing(digits.percentage)) digits.percentage = 3
  if(missing(expanded)) expanded = T

  ## Creación de factores de expansión
  Factores <- BD %>%
    group_by(vinculo) %>%
    summarise(Respuestas = n(),
              Poblacion = mean(Poblacion)) %>%
    mutate(FE = Poblacion/Respuestas)

  ## Creación de las tablas dinámicas sin expandir
  if (expanded == F) {

    tablas <- lapply(vars, function(x)
      assign(paste0(x),
             BD %>%
               tabyl(.data[[x]], vinculo) %>%
               adorn_totals(c("col"), name = "Uniandes") %>%
               adorn_percentages(denominator = "col") %>%
               setNames(c("modo","Estudiante_%", "Administrativo_%", "SSG_%", "P_Cátedra_%", "P_Planta_%", "Uniandes_%")) %>%
               select(-modo) %>%
               cbind(BD %>% tabyl(.data[[x]], vinculo) %>% adorn_totals(c("col"), name = "Uniandes"),.) %>%
               adorn_totals(c("row"), name = "Uniandes") %>%
               mutate(across(where(is.numeric), ~round(.,digits = digits.percentage))) %>%
               dplyr::rename(categoria = eval(x))))
    names(tablas) <- paste0("Tabla_SE_", vars)
  }

  ## Creación de las tablas dinámicas expandidas
  else if (expanded == T) {
    tablas <- lapply(vars, function(x)
      assign(paste0(x),
             BD %>%
               tabyl(.data[[x]], vinculo) %>%
               adorn_totals(c("col"), name = "Uniandes") %>%
               adorn_percentages(denominator = "col") %>%
               setNames(c("modo","Estudiante_%", "Administrativo_%", "SSG_%", "P_Cátedra_%", "P_Planta_%", "Uniandes_%")) %>%
               select(-modo) %>%
               cbind(BD %>% tabyl(.data[[x]], vinculo) %>% adorn_totals(c("col"), name = "Uniandes"),.) %>%
               adorn_totals(c("row"), name = "Uniandes") %>%
               mutate(across(where(is.numeric), ~round(.,digits = digits.percentage))) %>%
               mutate(Estudiante = round(Estudiante * Factores$FE[which(Factores$vinculo == "Estudiante")], digits = digits.expanded),
                      `Personal administrativo` = round(`Personal administrativo` * Factores$FE[which(Factores$vinculo == "Personal administrativo")], digits = digits.expanded),
                      `Personal de SSG` = round(`Personal de SSG` * Factores$FE[which(Factores$vinculo == "Personal de SSG")], digits = digits.expanded),
                      `Profesor de cátedra` = round(`Profesor de cátedra` * Factores$FE[which(Factores$vinculo == "Profesor de cátedra")], digits = digits.expanded),
                      `Profesor de planta` = round(`Profesor de planta` * Factores$FE[which(Factores$vinculo == "Profesor de planta")], digits = digits.expanded)) %>%
               dplyr::rename(categoria = eval(x)) %>%
               rowwise() %>%
               mutate(Uniandes = sum(Estudiante, `Personal administrativo`, `Personal de SSG`, `Profesor de cátedra`, `Profesor de planta`))))
    names(tablas) <- paste0("Tabla_", vars)
  }

  ## Error si el argumento de expansión es distinto a verdadero o falso
  else {
    stop("El argumento de expansión debe ser verdadero o falso")
  }

  return(tablas)

}
