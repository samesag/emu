#' A function to geocode colombian addresses using google geocoding API and googleway package
#'
#' This function allows you to geocode addresses in Colombia and get the results as a readable dataframe.
#'
#' @param df The df containing the addresses
#' @param direccion The name of the column (quoted) where the address is located (it must be formed as "address", "city", "country")
#' @param key The google API key used for geocoding
#' @keywords geocode google Colombia Bogota
#' @return A dataframe containing the coordinates in crs 4326 with the addresses geocoded
#' @examples
#' direcciones <- geocode_google_Colombia(df, "direccion_google", key)
#' @export

geocode_google_Colombia <- function(df, direccion, key){

  library(googleway)

  if(!missing(direccion)){
    df <- df %>%
      mutate(direccion_google = .data[[direccion]])
  } else if (c("direccion","ciudad") %in% names(df)){
    df <- df %>%
      mutate(direccion_google = paste0(direccion, ", ", ciudad, ", Colombia"))
  } else {
    df <- df %>%
      mutate(direccion_google = direccion_google)
  }

  df <- df %>%
    drop_na(direccion_google)

  resultados <- apply(df, 1, function(x){
    df_V2 <- google_geocode(address = x[["direccion_google"]],
                            key = key,
                            simplify = T)}
  )

  empty_list <- vector(mode = "list", length = 1)

  for (i in 1:length(resultados)) {

    if(df_V2$status != "OK"){
      data.frame(ciudad = NA, localidad = NA, direccion = NA, lat = NA, lon = NA)
    } else {

      if(is.null(resultados[[i]]$results$address_components[[1]])){
        localidad <- NA } else
          localidad <- as.data.frame(resultados[[i]]$results$address_components[[1]]) %>%
            filter(types == 'c("political", "sublocality", "sublocality_level_1")') %>%
            pull(long_name)

        if(is.null(resultados[[i]]$results$address_components[[1]])){
          ciudad <- NA } else {
            ciudad <- as.data.frame(resultados[[i]]$results$address_components[[1]]) %>%
              filter(types == 'c("administrative_area_level_2", "political")') %>%
              pull(long_name) }
        if(is_empty(ciudad)){
          ciudad <- as.data.frame(resultados[[i]]$results$address_components[[1]]) %>%
            filter(types == 'c("locality", "political")') %>%
            pull(long_name)}
        if(is_empty(ciudad)){
          ciudad <- NA
        }

        direccion <- resultados[[i]]$results$formatted_address
        lat <- resultados[[i]]$results$geometry$location$lat
        lon <- resultados[[i]]$results$geometry$location$lng

        if(is_empty(localidad)) {
          localidad <- NA
        }
        if(is_empty(direccion)) {
          direccion <- NA
        }
        if(is_empty(lon)) {
          lon <- NA
        }
        if(is_empty(lat)) {
          lat <- NA}
        empty_list[[i]] <- data.frame(ciudad, localidad, direccion, lat, lon)
    }
  }

  lista <- lapply(empty_list, function(x) x %>% slice(1))

  geocoded <- bind_rows(lista) %>%
    mutate(localidad = case_when(localidad == "Comuna Ciudad Bolívar" ~ "Ciudad Bolívar",
                                 localidad == "Comuna Chapinero"  ~ "Chapinero",
                                 localidad == "Santa Fé" ~ "Santa Fe",
                                 ciudad != "Bogotá" ~ "No resido en Bogotá"),
           ciudad = case_when(ciudad == "Bogotá" ~ "Bogota", T ~ ciudad)) %>%
    rename_with(~paste0(.,"_gm"))

  union <- df %>%
    bind_cols(., geocoded)

  return(union)

}
