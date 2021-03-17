#' A function that transform a sf object into a dodgr object
#'
#' This function return a dodger object (as a dataframe) from a sf object. Is usefull for net_dist and net_path
#'
#' @param net The sf object
#' @param id_col String character with the name of the column with the id of the network
#' @param wt_profile a json object or custom weight profile (see dodgr package), default is 1
#' @keywords dodgr sf
#' @return A dodgr object
#' @examples
#'
#' ## tm is an sf object which was cleaned by v.clean using GRASS in QGIS
#' sf_to_dodger(tm)

sf_to_dodgr <- function(net, id_col, wt_profile){

  require(sf)
  require(dodgr)

  ## Valores por defecto
  if(missing(id_col)) id_col = "fid"
  if(missing(wt_profile)) wt_profile = 1

  net_V2 <- net %>%
    st_transform(crs = 4326) %>%
    weight_streetnet(id_col = id_col, wt_profile = 1)

  warning("El objeto de salida tiene crs 4326, wt_profile por defecto es 1")
  return(net_V2)

}
