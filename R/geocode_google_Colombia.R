#' A function to geocode colombian addresses using google geocoding API and googleway package
#'
#' This function allows you to geocode addresses in Colombia and get the results as a readable dataframe.
#'
#' @param df The df containing the addresses
#' @param direccion The name of the column (quoted) where the address is located (it must be formed as "address", "city", "country")
#' @param key The google API key used for geocoding
#' @param suffix The suffix for the resultant columns generated by the geocoding
#' @param barrio Logical. If true, returns the information of the neighborhood
#' @param codigo_postal Logical. If true, returns the information of the postal code
#' @param join Logical. If true, the resultant dataframe is united with the original dataframe
#' @param join_id A string indicating the name of the column to join the results into the new dataframe
#' @keywords geocode google Colombia Bogota
#' @return A dataframe containing the coordinates in crs 4326 with the addresses geocoded
#' @examples
#' direcciones <- geocode_google_Colombia(df, "direccion_google", key = "ABC", suffix = "_com", barrio = T, codigo_postal = T)
#' @export

geocode_google_Colombia <- function(df, direccion, key, suffix, barrio, codigo_postal, localidad, join, join_id) {
  require(tidyverse)
  require(googleway)
  if (missing(suffix)) {
    suffix <- "_gm"
  }
  if (missing(barrio)) {
    barrio <- F
  }
  if (missing(codigo_postal)) {
    codigo_postal <- F
  }
  if (missing(localidad)) {
    localidad <- F
  }
  if (missing(join)) {
    join <- T
  }
  if (missing(join_id)) {
    join_id <- NULL
  }
  original <- df
  if (!missing(direccion)) {
    df <- df %>% mutate(direccion_google = .data[[direccion]])
  } else if ("direccion" %in% names(df) && "ciudad" %in% names(df)) {
    df <- df %>% mutate(
      ciudad = case_when(ciudad == "Bogotá" ~
                           "Bogota", T ~ ciudad),
      direccion_google = paste0(
        direccion,
        ", ", ciudad, ", Colombia"
      )
    )
    warning("La dirección de google se creó a partir de las columnas 'dirección' y 'ciudad'")
  } else if (!"direccion_google" %in% names(df)) {
    stop(
      "Por favor indique en cuál columna está la información de la dirección en formato google (direccion, ciudad, país) con el parámetro dirección"
    )
  }
  if (F %in% complete.cases(df$direccion_google)) {
    df <- df %>% drop_na(direccion_google)
    warning(
      "Los valores de NA en la columna de dirección fueron eliminados. Revise el número de datos de salida"
    )
  }
  resultados <- apply(df, 1, function(x) {
    google_geocode(
      address = x[["direccion_google"]],
      key = key,
      simplify = T
    )
  })
  formato <- vector(mode = "list", length = 1)
  for (i in 1:length(resultados)) {
    if (resultados[[i]]$status != "OK") {
      if (localidad == T) {
        formato[[i]] <- data.frame(
          ciudad = NA,
          localidad = NA,
          direccion = NA,
          lat = NA,
          lon = NA
        )
      } else {
        formato[[i]] <- data.frame(
          ciudad = NA,
          direccion = NA,
          lat = NA,
          lon = NA
        )
      }
    } else {
      if (localidad == T) {
        if (is.null(resultados[[i]]$results$address_components[[1]])) {
          localidad <- NA
        } else {
          localidad <-
            as.data.frame(resultados[[i]]$results$address_components[[1]]) %>%
            filter(types == "c(\"political\", \"sublocality\", \"sublocality_level_1\")") %>%
            pull(long_name)
        }
      }
      if (is.null(resultados[[i]]$results$address_components[[1]])) {
        ciudad <- NA
      } else {
        ciudad <-
          as.data.frame(resultados[[i]]$results$address_components[[1]]) %>%
          filter(types == "c(\"administrative_area_level_2\", \"political\")") %>%
          pull(long_name)
      }
      if (is_empty(ciudad)) {
        ciudad <-
          as.data.frame(resultados[[i]]$results$address_components[[1]]) %>%
          filter(types == "c(\"locality\", \"political\")") %>%
          pull(long_name)
      }
      if (is_empty(ciudad)) {
        ciudad <-
          as.data.frame(resultados[[i]]$results$address_components[[1]]) %>%
          filter(types == "c(\"administrative_area_level_1\", \"political\")") %>%
          pull(long_name)
      }
      if (is_empty(ciudad)) {
        ciudad <- NA
      }
      if (barrio == T) {
        if ("c(\"neighborhood\", \"political\")" %in%
            resultados[[i]]$results$address_components[[1]]$types) {
          barrio <-
            as.data.frame(resultados[[i]]$results$address_components[[1]]) %>%
            filter(types == "c(\"neighborhood\", \"political\")") %>%
            pull(long_name)
        } else {
          barrio <- NA
        }
      }
      if (codigo_postal == T) {
        if ("postal_code" %in% resultados[[i]]$results$address_components[[1]]$types) {
          codigo_postal <-
            as.data.frame(resultados[[i]]$results$address_components[[1]]) %>%
            filter(types == "postal_code") %>%
            pull(long_name)
        } else {
          codigo_postal <- NA
        }
      }
      direccion <- resultados[[i]]$results$formatted_address
      lat <- resultados[[i]]$results$geometry$location$lat
      lon <- resultados[[i]]$results$geometry$location$lng
      if (localidad == T) {
        if (is_empty(localidad)) {
          localidad <- NA
        }
      }
      if (is_empty(direccion)) {
        direccion <- NA
      }
      if (is_empty(lon)) {
        lon <- NA
      }
      if (is_empty(lat)) {
        lat <- NA
      }
      if (barrio == T && codigo_postal == T & localidad == T) {
        formato[[i]] <- data.frame(
          ciudad,
          localidad,
          barrio,
          codigo_postal,
          direccion,
          lat,
          lon
        )
      } else if (barrio == T && codigo_postal == F & localidad == T) {
        formato[[i]] <- data.frame(
          ciudad, localidad,
          barrio, direccion, lat, lon
        )
      } else if (barrio == F && codigo_postal == T & localidad == T) {
        formato[[i]] <- data.frame(
          ciudad, localidad,
          codigo_postal, direccion, lat, lon
        )
      } else if (barrio == T && codigo_postal == T & localidad == F) {
        formato[[i]] <- data.frame(
          ciudad,
          barrio, codigo_postal, direccion, lat, lon
        )
      } else if (barrio == T && codigo_postal == F & localidad == F) {
        formato[[i]] <- data.frame(
          ciudad,
          barrio, direccion, lat, lon
        )
      } else if (barrio == F && codigo_postal == T & localidad == F) {
        formato[[i]] <- data.frame(
          ciudad,
          codigo_postal, direccion, lat, lon
        )
      } else {
        formato[[i]] <- data.frame(
          ciudad, localidad,
          direccion, lat, lon
        )
      }
    }
  }
  lista <- lapply(formato, function(x) {
    x %>% slice(1)
  })
  if (localidad == T) {
    geocoded <-
      bind_rows(lista) %>%
      mutate(
        localidad = case_when(
          localidad ==
            "Comuna Ciudad Bolívar" ~ "Ciudad Bolívar",
          localidad ==
            "Comuna Chapinero" ~ "Chapinero",
          localidad == "Santa Fé" ~
            "Santa Fe",
          ciudad != "Bogotá" && !is.na(ciudad) ~
            "Fuera de Bogotá",
          is.na(localidad) ~ NA_character_,
          is.na(ciudad) ~ NA_character_,
          T ~ localidad
        )
      ) %>%
      {
        if (!is.null(suffix)) {
          rename_with(., ~
                        paste0(., suffix))
        } else {
          .
        }
      }
  } else {
    geocoded <- bind_rows(lista) %>%
      {
        if (!is.null(suffix)) {
          rename_with(., ~
                        paste0(., suffix))
        } else {
          .
        }
      }
  }
  if (join == T) {
    union <- df %>%
      bind_cols(., geocoded) %>%
      {
        if (is.null(join_id)) {
          full_join(original, .)
        } else {
          full_join(original, ., by = join_id)
        }
      }
  } else {
    union <- geocoded
  }
  return(union)
}
