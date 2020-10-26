#' geocode in Colombia
#'
#' This function allows you to geocode addresses in Colombia, specifying address, city and a CRS.
#' @param df The dataframe containing the information.
#' @param CRS The CRS for geocoding the addresses.
#' @param id The name of the column with the id for keeping track of your data.
#' @param address The name of the column with the addresses for geocoding.
#' @param city The name of the column with the city of the address.
#' @keywords geocode Colombia address.
#' @return The coordinates of the address given, and the match score.
#' @examples
#' geocode_Colombia(df, 3116, "id","street", "city")
#' @export

geocode_Colombia <- function(df, CRS, id, address, city) {

  if (missing(id)) {if("id" %in% colnames(df)){id <- "id"}
    else if ("ID" %in% colnames(df)) {id <- "ID"}
    else if ("Encuesta No." %in% colnames(df)){id <- "Encuesta No."}
    else {stop("Por favor especifique la columna 'id'")}
  }

  if (missing(address)) {if("Direccion" %in% colnames(df)){address <- "Direccion"}
    else if ("Dirección" %in% colnames(df)) {address <- "Dirección"}
    else if ("dirección" %in% colnames(df)){address <- "dirección"}
    else if ("direccion" %in% colnames(df)){address <- "direccion"}
    else {stop("Por favor especifique la columna 'address' o 'dirección")}
  }

  if (missing(city)) {if("Ciudad" %in% colnames(df)){city <- "Ciudad"}
    else if ("ciudad" %in% colnames(df)) {city <- "ciudad"}
    else if ("dirección" %in% colnames(df)){city <- "dirección"}
    else {stop("Por favor especifique la columna 'city' o 'ciudad")}
  }

  df <- df %>%
    mutate(!!city := case_when(.data[[city]] == "Bogotá D.C." ~ "Bogota",
                               TRUE ~ .data[[city]]))

  geocodeSantiago <- function(df, id, address, city, CRS){
    require(httr)
    require(rjson)

    # Mirar si hay mas de 1000 direcciones, si sí, parar
    # if (length(id) > 1000){
    #   print(paste("Número de direcciones: ", length(id)))
    #   stop("Solo se pueden usar hasta 1000 direcciones por vez")}

    # Revisar si todos los id son numéricos, tanto real como un número en string
    # if(!all(grepl("^[0-9]{1,}$", id))){ # HT: https://stackoverflow.com/a/48954452/2630957
    #   if (!is.numeric(id)) {
    #     stop("id debe ser un número")
    #   }
    # }

    # Crear data frame
    adr_df <- data.frame(OBJECTID = df[[id]],  #  es necesario que el id se llame OBJECTID
                         STREET = df[[address]],
                         ZONE = df[[city]])

    # Crear codificación json
    tmp_list <- apply(adr_df, 1, function(i) list(attributes = as.list(i)))

    # Convertir OBJECTID en numérico
    tmp_list <- lapply(tmp_list, function(i) {
      i$attributes$OBJECTID <- as.numeric(i$attributes$OBJECTID);
      i})

    adr_json <- toJSON(list(records = tmp_list))
    adr_json_enc <- URLencode(adr_json, reserved = TRUE) #?

    # Servidor de geocodificación
    gserver <- "http://157.253.236.252:6080/arcgis/rest/services/geocode/Colombia_Geocode/GeocodeServer/geocodeAddresses"

    # Subir los datos al servidor de geocodificación
    req <- POST(
      url = gserver,
      body = list(addresses = adr_json, f="json", outSR = CRS),
      encode = "form")
    stop_for_status(req) # error check

    # procesar y unir
    res <- content(req, "parsed", "application/json")

    # Creación del data frame de salida
    resdfr <- data.frame()
    options(digits = 15)
    for (i in seq_len(length(res$locations))){
      d <- with(res$locations[[i]], {data.frame(id = attributes$ResultID,
                                                Direccion = address,
                                                lon = as.numeric(location$x),
                                                lat = as.numeric(location$y),
                                                score = score,
                                                #locName = attributes$Loc_name,
                                                status = attributes$Status,
                                                matchAddr = attributes$Match_addr)})
      #side = attributes$Side
      #addressType = attributes$Addr_type)})
      resdfr <- rbind(resdfr, d)
    }
    resdfr$Direccion <- adr_df$STREET
    return(resdfr)
  }

   # geocodeSantiago(df, id, address, city, CRS)

  if ("data.frame" %in% class(df)) {
    df <- df %>%
      arrange(.data[[id]])
    base1 <- df
    n <- ceiling(nrow(df)/1000)
    df_lista <- split(df, f = rep_len(1:n, nrow(df)))
    df <- lapply(df_lista, function(x) geocodeSantiago(x,id,address,city,CRS))
    df <- do.call(rbind, df)
    df <- df[complete.cases(df),]
    df <- df %>%
      arrange(as.numeric(id))
    row.names(df) <- NULL
    x = names(base1)[1]
    y = names(df)[1]
    base1 <- merge.data.frame(base1, subset(df, select = c("id","lon","lat","matchAddr")), by.x = x, by.y = y, all.x = T)
    return(base1)
  } else if ("list" %in% class(df)) {
    stop("Por favor programe la función para listas")
  }
}



