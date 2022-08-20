steer_colours_text = list(
  steer = c("black", "white", "black", "white",
            "black", "white", "white", "black")
)

steer_palettes_text = function(name, n, all_palettes = steer_colours_text, type = c("discrete", "continuous"), direction = 1) {
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

scale_colour_steer_text = function(name, direction = 1, n) {
  ggplot2::scale_colour_manual(values = steer_palettes_text(name, type = "discrete", direction = direction, n = n))
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
