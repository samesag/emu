#' A function to identify the nearest coordinate in a line for a point, using sf objects.
#'
#' This function allows you to get the coordinates for the closests point in a line for an input point. This function
#' uses the snapPointsToLines from the sp package but accept sf as inputs. The process must be made in geographic
#' coordinates, so if the sf inputs are in lon/lat coordinates, they must be tranformed. If no crs is specified as
#' projected, the default crs is 3116. A crs for the output can also be choosen, by default, the result will be in crs
#' 4326 (WGS84)
#' @param points The sf object containing points
#' @param lines The sf object containing lines
#' @param crs_projected The crs which will be used to find the neares points and calculete euclidean distances
#' @param crs_out The crs used for the coordinates at the final dataframe
#' @keywords st_join nearest point
#' @return A dataframe containing the origin id, coordinates, and the destination coordinates as lon_dest and lat_dest
#' @examples
#' destino_bici <- join_points_lines(direcciones, ciclorruta) %>%
#' select(id, lon_dest, lat_dest)
#' @export

join_points_lines <- function(points, lines, crs_projected = NULL, crs_out = NULL){

  require(sf)
  require(maptools)

  if ((!"sf" %in% class(points)) | (!"sf" %in% class(lines))) {
    stop("objects points and lines must be sf class")
  }
  if(missing(crs_projected)){
    crs_projected <- 3116
  }
  if(missing(crs_out)){
    crs_out <- 4326
  }
  if(st_is_longlat(points)){
    points <- st_transform(points, crs_projected)
  }
  if(st_is_longlat(lines)){
    lines <- st_transform(lines, crs_projected)
  }

  points_sp <- as(points, Class = "Spatial")
  lines_sp <- as(lines, Class = "Spatial")

  union <- snapPointsToLines(points_sp, lines_sp) %>%
    st_as_sf(union) %>%
    st_transform(crs = crs_out) %>%
    mutate(lon_dest = as.data.frame(st_coordinates(.))[["X"]],
           lat_dest = as.data.frame(st_coordinates(.))[["Y"]]) %>%
    st_drop_geometry() %>%
    bind_cols(points, .)

  return(union)

}

