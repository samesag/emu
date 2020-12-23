#' A function to calculate distances in a network between coordinates of origin and destination
#'
#' This function return distances in meters for between points in a network. It can accept an sf object as origin or destination,
#' or a dataframe with columns for longitud and latitud for origin and destination.
#'
#' @param net The sf or dodgr_streetnet object representing the network
#' @param shp_origen Object with the coordinates of origin (may be a sf or a dataframe object)
#' @param shp_destino Object with the coordinates of destination (may be a sf or a dataframe object)
#' @param pairwise A logical parameter indicating if the computation must be in a pairwise format. If false, the result is a matrix
#' with distances between each point
#' @param crs_projected The EPSG number or proj4 object defined for calculating the nearest neighborhood (if join is true)
#' @param crs_longlat The EPSG number or proj4 object defined for computing distances with the dodgr_distances function (not used)
#' @param join A logical parameter indicating if the shp_origen and shp_destino must be joined by the nearest neighborhood
#' @param id_col String character with the name of the column with the id of the network
#' @param lon_origen String character with the name of the column containing the longitud of origin when shp_origen and shp_destino are not
#' sf objects
#' @param lat_origen String character with the name of the column containing the latitude of origin when shp_origen and shp_destino are not
#' sf objects
#' @param lon_destino String character with the name of the column containing the longitud of destination when shp_origen and shp_destino
#' are not sf objects
#' @param lat_destino String character with the name of the column containing the latitude of destination when shp_origen and shp_destino
#' are not sf objects
#' @param simplified A logical parameter. If true, the return is not a dataframe
#' @param id_destination String character indicating a shp_destino column wanted in the resulting dataframe (tipically an id column)
#' @param matrix_as_table A logical parameter. Only used when pairwise is false. The matrix of distances between points is returned as a
#' datframe of three columns (origin, destination, and distance)
#' @keywords matrix distance, shortest path, distance, coordinates
#' @return A dataframe, matrix or vector of distances between points along a network
#' @examples
#'
#' ## origin and destination as a dataframe
#' distancias <- net_dist(vias2, shp_origen = BD_sf3, shp_destino = BD_sf3, pairwise = T, id_col = "edge_id", lon_origen = "lon_or",
#' lat_origen = "lat_or", lon_destino = "lon_dest", lat_destino = "lat_dest")
#'
#' ## origin and destination is the same, calculating a distance matrix as a sf objects
#' distancias_tm <- net_dist(tm, estaciones, estaciones, pairwise = F, join = F, matrix_as_table = T)
#'
#' ## origin and destination are sf objects, and the distance is computated to the nearest neighborhood
#' distancias_tm2 <- net_dist(tm, BD_sf1, estaciones, pairwise = T, join = T, id_destination = "nombre_est")
#' @export

net_dist <- function(net, shp_origen, shp_destino, pairwise, crs_projected, crs_longlat, join, id_col,
                     lon_origen, lat_origen, lon_destino, lat_destino, simplified, id_destination = NULL,
                     matrix_as_table){

  require(sf)
  require(dodgr)
  require(tidyverse)

  # Revisión de los tipos de objetos de entrada a la función --------------------------------------------------------
  if(missing(pairwise)) pairwise = T
  if(missing(join)) join = T
  if(missing(simplified)) simplified = F
  if(!is.null(id_destination) && (!id_destination %in% names(shp_destino))) stop("la columna id_destination debe estar en el objeto shp_destino")
  if("sf" %in% class(net)){
    if("MULTILINESTRING" %in% st_geometry_type(net)) stop("La capa de la red debe ser 'limpiada' en GRASS o similares antes de usarla con la función" )
  }
  if(missing(matrix_as_table)) matrix_as_table = T

  # Adición de los objetos por defecto ------------------------------------------------------------------------------
  if(missing(crs_projected)) crs_projected = 3116
  if(missing(crs_longlat)) crs_longlat = 4326
  if(missing(id_col)) id_col = "fid"

  # Creación de la red  ---------------------------------------------------------------------------------------------

  ## Adopción de la red como net_V2 según el objeto de entrada
  if("sf" %in% class(net)){
    net_V2 <- net %>%
      st_transform(crs = 4326) %>%
      weight_streetnet(id_col = id_col, wt_profile = 1)
  } else if("dodgr_streetnet" %in% class(net)){
    net_V2 <- net
    warning("Se asume que el objeto dodgr_streetnet fue creado en CRS 4326 (WGS 84)")
  } else {
    stop("La red debe ser de tipo sf o dodgr_streetnet (se puede hacer el proceso antes de usar la función)")
  }

  ## Selección de un solo componente de las vías, revisa que todas las líneas estén conectadas
  net_V2 <- net_V2[net_V2$component == 1, ]

  # Creación de los objetos de orígen y destino ---------------------------------------------------------------------

  if(("sf" %in% class(shp_origen)) && ("sf" %in% class(shp_destino))){

    ## Verificación del crs de los objetos de orígen y destino
    if(as.numeric(str_extract(st_crs(shp_origen)$input, "[[:digit:]]+")) != 4326) shp_origen <- shp_origen %>% st_transform(crs = 4326)
    if(as.numeric(str_extract(st_crs(shp_destino)$input, "[[:digit:]]+")) != 4326) shp_destino <- shp_destino %>% st_transform(crs = 4326)

    shp_origen <- shp_origen %>%
      mutate(lon_origen = as.data.frame(st_coordinates(.))[["X"]],
             lat_origen = as.data.frame(st_coordinates(.))[["Y"]]) %>%
      st_transform(crs = crs_projected)

    shp_destino <- shp_destino %>%
      mutate(lon_destino = as.data.frame(st_coordinates(.))[["X"]],
             lat_destino = as.data.frame(st_coordinates(.))[["Y"]]) %>%
      st_transform(crs = crs_projected)

    if(join == T){
      OD <- st_join(shp_origen, shp_destino, join = st_nearest_feature) %>%
        st_drop_geometry()
      origin <- OD %>% select(lon_origen, lat_origen)
      destination <- OD %>% select(lon_destino, lat_destino)

    } else {
      origin <- shp_origen %>% select(lon_origen, lat_origen) %>% st_drop_geometry()
      destination <- shp_destino %>% select(lon_destino, lat_destino) %>% st_drop_geometry()
    }
  } else if(!"sf" %in% class(shp_origen) && (missing(lon_origen) | missing(lat_origen) | missing(lon_destino) | missing(lat_destino))){

    stop("Si el objeto de orígenes y destinos no es de tipo sf, se deben especificar los nombres de las columnas que contienen las coordenadas de orígen y destino")

  } else if((nrow(shp_origen) != nrow(shp_destino)) && pairwise == T) {

    stop("Si el cálculo se hace par a par, los objetos de orígen y destino deben tener la misma longitud")

  } else {

    origin <- shp_origen %>% select(!!lon_origen, !!lat_origen)
    destination <- shp_destino %>% select(!!lon_destino, !!lat_destino)

  }

  # Limpieza de la base de datos para unión -------------------------------------------------------------------------

  if("lon_origen" %in% names(shp_origen)) shp_origen <- shp_origen %>% select(-lon_origen)
  if("lat_origen" %in% names(shp_origen)) shp_origen <- shp_origen %>% select(-lat_origen)
  if("lon_destino" %in% names(shp_origen)) shp_origen <- shp_origen %>% select(-lon_destino)
  if("lat_destino" %in% names(shp_origen)) shp_origen <- shp_origen %>% select(-lat_destino)
  if((!"sf" %in% class(shp_origen)) && (!"sf" %in% class(shp_destino))){
    if(lon_origen %in% names(shp_origen)) shp_origen <- shp_origen %>% select(-lon_origen)
    if(lat_origen %in% names(shp_origen)) shp_origen <- shp_origen %>% select(-lat_origen)
    if(lon_destino %in% names(shp_origen)) shp_origen <- shp_origen %>% select(-lon_destino)
    if(lat_destino %in% names(shp_origen)) shp_origen <- shp_origen %>% select(-lat_destino)
  }

  # Cálculo de las distancias en la red -----------------------------------------------------------------------------

  distancias <- dodgr_dists(net_V2, from = origin, to = destination, pairwise = pairwise)

  # Configuración del archivo de salida -----------------------------------------------------------------------------

  if(pairwise == T && simplified == F){

    distancias <- as.data.frame(distancias) %>%
      rename(distancia = V1)

    if(!is.null(id_destination) && join == T){

      distancias <- shp_origen %>%
        bind_cols(., OD %>% select(!!id_destination)) %>%
        bind_cols(., origin, destination, distancias)

    } else if(!is.null(id_destination)){

      distancias <- shp_origen %>%
        bind_cols(., shp_destino %>% select(!!id_destination)) %>%
        bind_cols(., origin, destination, distancias)

    } else {
      distancias <- shp_origen %>%
        bind_cols(., origin, destination, distancias)
    }

    return(distancias)

  } else if(matrix_as_table == T) {

    require(reshape2)
    distancias <- melt(distancias) %>%
      rename(origin=Var1, destination = Var2)

    return(distancias)

  } else {

    return(distancias)

  }
}
