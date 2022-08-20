#' A function to use the Steer colors in ggplot graphs
#'
#' This function allows you to geocode addresses in Colombia and get the results as a readable dataframe.
#'
#' @param name The name of the palette
#' @param direction The direction for using the colors
#' @keywords Steer colours palette
#' @return The Steer palette for ggplot graphs
#' @examples
#' scale_fill_steer_d(name = "steer", direction = -1,)
#' @export

steer_colours = list(
  steer = c("#00B0DF", "#161B22", "#F8982D", "#01895E",
            "#9E9CA0", "#C03301", "#0073E3", "#D6E0E7")
)

steer_palettes = function(name, n, all_palettes = steer_colours, type = c("discrete", "continuous"), direction = 1) {
  palette = all_palettes[[name]]
  if (missing(n)) {
    n = length(palette)
  }
  type = match.arg(type)
  if (direction == -1){
    if (n <= length(palette)){
      out = switch(type,
                   continuous = rev(grDevices::colorRampPalette(palette)(n)),
                   discrete = rev(palette[1:n])
      )
    } else {
      out = switch(type,
                   continuous = rev(grDevices::colorRampPalette(palette)(n)),
                   discrete = colorRampPalette(colors = all_palettes[[name]])(n)
      )
    }
  } else {
    if (n <= length(palette)) {
      out = switch(type,
                   continuous = grDevices::colorRampPalette(palette)(n),
                   discrete = palette[1:n]
      )
    } else {
      out = switch(type,
                   continuous = grDevices::colorRampPalette(palette)(n),
                   discrete = colorRampPalette(colors = all_palettes[[name]])(n)
      )
    }
  }

  structure(out, name = name, class = "palette")
}

scale_colour_steer_d = function(name, direction = 1) {
  ggplot2::scale_colour_manual(values = steer_palettes(name,
                                                       type = "discrete", direction = direction))
}

scale_fill_steer_d = function(name, direction = 1, n) {
  ggplot2::scale_fill_manual(values = steer_palettes(name, type = "discrete", direction = direction, n = n))
}

scale_colour_steer_c = function(name, direction = 1) {
  ggplot2::scale_colour_gradientn(colours = steer_palettes(name = name,
                                                           type = "continuous", direction = direction))
}

scale_fill_steer_c = function(name, direction = 1) {
  ggplot2::scale_fill_gradientn(colours = steer_palettes(name = name,
                                                         type = "continuous", direction = direction))
}

scale_color_steer_d = scale_colour_steer_d
scale_color_steer_c = scale_colour_steer_c
