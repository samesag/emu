#' Customizable coordinates zoom for ggplot maps
#'
#' Set coordinate limits for a ggplot map based on a sf object or a bbox object, having an option for expanding the limits (expansion)
#' @param data The dataframe containing the spatial information for the box limits.
#' @param bbox An st_bbox objetc as a named vector for getting the box limits
#' @param expansion a vector trbl for expandin any dimension over the initial limits
#' @param projected_base logical, if T the original source (data or bbox) is in a projected CRS
#' @param r_xmin the sf object to replace the dimension xmin
#' @param r_xmax the sf object to replace the dimension xmax
#' @param r_ymin the sf object to replace the dimension ymin
#' @param r_ymax the sf object to replace the dimension ymax
#' @keywords coordinates crs ggplot expansion zoom
#' @return The coordinates for an accurate plot
#' @examples
#' ## Not run:
#' librar(sf)
#' sf_utam %>%
#' ggplot() +
#' geom_sf(data = bogota_unido, fill = "#EBECF2") +
#' geom_sf(aes(fill = as.factor(ESTRATOPre)), alpha = 0.6) +
#' scale_fill_brewer("Estrato", palette = "YlGnBu") +
#' theme_void(base_family = "Franklin Gothic Book") +
#' theme(plot.background = element_rect(fill = "#EBECF2", color = NA),
#' plot.margin = margin(r = 10),
#' text = element_text(size = 10)) +
#' coord_sf_santi(bbox = a, expansion = space(r = 0.5))
#' @export

sm_coordinates <- function (data = NULL, bbox = NULL, xlim = NULL, ylim = NULL, expand = TRUE, crs = NULL,
                             r_xmin = NULL, r_xmax = NULL, r_ymin = NULL, r_ymax = NULL,
                             datum = sf::st_crs(4326), label_graticule = waiver(), label_axes = waiver(),
                             ndiscr = 100, default = FALSE, clip = "on", projected_base = F,
                             expansion = space(t = 0, r = 0, b = 0, l = 0))
{
  library(sf)

  if(projected_base){
    if(!missing(data)){
      data <- data %>%
        st_transform(crs = 4326)} else {
          bbox = bbox %>%
            st_as_sfc() %>%
            st_transform(crs = 4326) %>%
            st_bbox()
        }
  }
  if(!missing(data)){
    xlim = c(as.numeric(st_bbox(data)[["xmin"]]), as.numeric(st_bbox(data)[["xmax"]]))
    ylim = c(as.numeric(st_bbox(data)[["ymin"]]), as.numeric(st_bbox(data)[["ymax"]]))
  }
  if(!missing(bbox)){
    xlim = c(as.numeric(bbox[["xmin"]]), as.numeric(bbox[["xmax"]]))
    ylim = c(as.numeric(bbox[["ymin"]]), as.numeric(bbox[["ymax"]]))
  }

  ## Reemplazo
  if (!missing(r_xmin)) {
    if (projected_base) {
      r_xmin <- st_transform(r_xmin, crs = 4326)
    }
    xlim[1] <- st_bbox(r_xmin)[["xmin"]]
  }
  if (!missing(r_ymin)) {
    if (projected_base) {
      r_ymin <- st_transform(r_ymin, crs = 4326)
    }
    ylim[1] <- st_bbox(r_ymin)[["ymin"]]
  }
  if (!missing(r_xmax)) {
    if (projected_base) {
      r_xmax <- st_transform(r_xmax, crs = 4326)
    }
    xlim[2] <- st_bbox(r_xmax)[["xmax"]]
  }
  if (!missing(r_ymax)) {
    if (projected_base) {
      r_ymax <- st_transform(r_ymax, crs = 4326)
    }
    ylim[2] <- st_bbox(r_ymax)[["ymax"]]
  }

  ## Expansión
  xlim = c(xlim[1] + abs(xlim[1] * expansion[4]), xlim[2] + abs(xlim[2] * expansion[2]))
  ylim = c(ylim[1] + abs(ylim[1] * expansion[1]), ylim[2] + abs(ylim[2] * expansion[3]))

  if (is.waive(label_graticule) && is.waive(label_axes)) {
    label_graticule <- ""
    label_axes <- "--EN"
  }
  else {
    label_graticule <- label_graticule %|W|% ""
    label_axes <- label_axes %|W|% ""
  }
  if (is.character(label_axes)) {
    label_axes <- parse_axes_labeling(label_axes)
  }
  else if (!is.list(label_axes)) {
    abort("Panel labeling format not recognized.")
    label_axes <- list(left = "N", bottom = "E")
  }
  if (is.character(label_graticule)) {
    label_graticule <- unlist(strsplit(label_graticule,
                                       ""))
  }
  else {
    abort("Graticule labeling format not recognized.")
    label_graticule <- ""
  }
  ggproto(NULL, CoordSf, limits = list(x = xlim, y = ylim),
          datum = datum, crs = crs, label_axes = label_axes, label_graticule = label_graticule,
          ndiscr = ndiscr, expand = expand, default = default,
          clip = clip)
}

waiver <- function() structure(list(), class = "waiver")

is.waive <- function(x) inherits(x, "waiver")

parse_axes_labeling <- function(x) {
  labs = unlist(strsplit(x, ""))
  list(top = labs[1], right = labs[2], bottom = labs[3], left = labs[4])
}

space <- function (t = 0, r = 0, b = 0, l = 0)
{
  u <- c(t/1000, r/1000, b/1000, l/1000)
  u
}
