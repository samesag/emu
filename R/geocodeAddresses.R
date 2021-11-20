#' Geocode addresses in Colombia
#'
#' This function allows you to geocode addresses in Colombia, specifying address, city and a CRS.
#'
#' @param df The dataframe containing the information.
#' @param address The name of the column with the addresses for geocoding.
#' @param city The name of the column with the city of the address.
#' @param countryCode Limits the candidates returned to the specified country or countries. Acceptable values include the 3-character country code. You can specify multiple country codes to limit results to more than one country.
#' @param crs The spatial reference of the x/y coordinates returned by a geocode request. The spatial reference can be specified as either a well-known ID (WKID) or as a JSON spatial reference object. If crs is not specified, the spatial reference of the output locations is 3116
#' @param names_crs Logical. If the crs must be added at the end of the results columns "lon" and "lat"
#' @param names_sep A string to separate the name of the columns "lon" and "lat" and the crs (WKID)
#' @keywords geocode Colombia address.
#' @return The coordinates (lon, lat) of the address given and the match score.
#' @examples
#' df %>% geocodeAddresses(address = direccion_hogar, city = ciudad)
#' @export

geocodeAddresses <- function(df, address, city, countryCode = "CO", crs = 3116, names_crs, names_sep) {

  require(httr)
  require(rlang)
  require(tidyverse)

  address <- enquo(address)
  city <- enquo(city)

  if (missing(names_crs)) {
    names_crs <- T
  }
  if (missing(names_sep)) {
    names_sep <- "_"
  }

  if (!as_label(address) %in% names(df)) stop("La variable address no es una variable presente en el dataframe")
  if (!as_label(city) %in% names(df)) {
    remove_city <- T
    df <- df %>% mutate(city = as_name(!!city))
  }

  gserver <- "https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/findAddressCandidates?"

  df_list <- split(df, seq(nrow(df)))

  geocoded_list <- map(df_list, function(df) {
    content(POST(url = gserver, body = list(
      Address = df[[as_name(address)]], City = "Bogotá", countryCode = "CO", f = "pjson", outSR = 4326
    ), encode = "form"), "parsed", "application/json")$candidates[[1]]
  })

  if (names_crs == T) {
    geocoded_df <- map_df(geocoded_list, function(df) {
      setNames(
        list(
          df[["location"]][["x"]],
          df[["location"]][["y"]],
          df[["score"]],
          df[["address"]]
        ),
        c(paste0("lon", names_sep, crs), paste0("lat", names_sep, crs), "score", "matchAddr")
      )
    })
  } else {
    geocoded_df <- map_df(geocoded_list, function(df) {
      list(
        lon = df[["location"]][["x"]],
        lat = df[["location"]][["y"]],
        score = df[["score"]],
        matchAddr = df[["address"]]
      )
    })
  }

  df_final <- df %>% bind_cols(., geocoded_df)

  if (remove_city == T) df_final <- df_final %>% select(-city)

  return(df_final)
}
