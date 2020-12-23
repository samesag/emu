#' geocode in Colombia using the ESRI API geocoding server first
#'
#' This function allows you to geocode addresses in Colombia. The first attempt to geocode addresses is by using the ESRI
#' API geocoding server. If any address is not geocoded, then this directions are geocoded using the Google Maps API
#' (requires key). The result is a datframe with all the information recolected, specifying in the column geocoded with which
#' server the address was geocoded (1: ESRI, 2: Google Maps). A column for localidad is specified when the address is geocoded
#' with the Google Maps API, since the score of this geocoding process is not computated, so this may be the fastes way to
#' verify the results obtained.
#'
#' @param df The dataframe containing the information.
#' @param key The google API key used for geocoding with the Google Maps API.
#' @param id_col The name of the column with the id for keeping track of your data (for each address).
#' @param join_id A string indicating the name of the column to join the results into the new dataframe.
#' @keywords geocode, Colombia, address, geocoding, Google Maps.
#' @return The coordinates of the address given.
#' @examples
#' ver <- geocode_multiple(bd, key)
#' @export

geocode_multiple_Colombia <- function(df, key, ..., id_col = NULL, join_id = NULL){

  if(is.null(id_col)){
    if("id" %in% names(df)){
      id_col = "id"
      warning('Se asume que la columna "id" es el identificador único de las direcciones. En caso contrario, especifíque el parámetro id_col')
    }
  }

  df2 <- df %>%
    geocode_Colombia(CRS = 4326, ...) %>%
    {. ->> df1} %>% filter(., is.na(lon)) %>%
    geocode_google_Colombia(key = key, join_id = join_id) %>%
    full_join(., df1) %>%
    mutate(geocoded = case_when(!is.na(lon) ~ 1,
                                !is.na(lon_gm) ~ 2,
                                T ~ NA_real_),
           lon = case_when(geocoded == 1 ~ lon,
                           geocoded == 2 ~ lon_gm,
                           T ~ NA_real_),
           lat = case_when(geocoded == 1 ~ lat,
                           geocoded == 2 ~ lat_gm,
                           T ~ NA_real_),
           matchAddr = case_when(geocoded == 1 ~ matchAddr,
                                 geocoded == 2 ~ direccion_gm,
                                 T ~ NA_character_)) %>%
    select(-ciudad_gm, -direccion_gm, -lat_gm, -lon_gm, -direccion_google) %>%
    {if(!is.null(id_col)) arrange(., .data[[id_col]]) else .}

  return(df2)

}
