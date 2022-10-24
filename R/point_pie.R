#' Draw pie chart at any x,y coordinates.
#'
#' This geom is based on `ggplot2::GeomPolygon`. We used polygon geom to create pie
#' chart than can be mapped to arbitrary x and y coordinates
#'
#' Aesthetics:
#' geom_point_pie understand the following aesthetics (required aesthetics are in
#' bold):
#'
#' - **x**
#' - **y**
#' - **group**
#' - color
#' - fill
#' - size
#' - linetype (will change everything rn)
#' - alpha
#' @param r0 The radius (0-1) of the inner circle, in case a dounut plot is needed.
#'
#' @importFrom ggplot2 layer
#'
#' @export
geom_point_pie <- function(
    mapping = NULL, data = NULL, stat = "point_pie",
    position = "identity", r0 = 0L, na.rm = FALSE, show.legend = NA,
    inherit.aes = TRUE, ...
) {
  layer(
    geom = GeomPointPie, mapping = mapping,  data = data, stat = StatPointPie,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, r0 = r0, shape = 21, ...)
  )
}


#' I think the r1  and r0 are in their own system?
#'
#' ggplot always map size to 1-6. Use scale_size_contineuous to deal with it.
#' In the future, maybe get our own scale...
#'
#' @importFrom ggplot2 ggproto aes GeomPolygon .pt .stroke
#' @importFrom scales alpha
#' @importFrom grid viewport pointsGrob
#' @import dplyr
#'
#' @param ... skip for now
#'
#' @export
GeomPointPie = ggproto(
  "GeomPointPie", GeomPolygon,
  required_aes = c("x", "y", "group"),
  default_aes = aes(
    colour = "black", fill = "gray20", shape = 21,
    size = 0.3, alpha = 1, linetype = 1, r0 = 0
  ),

  draw_key = function(data, params, size) {
    # pieGrob(
    #   0.5, 0.5,
    #   0, size / 50 %||% 0.3,
    #   0, 1.5 * pi,
    #   gp = gpar(
    #     col = alpha(data$colour %||%"black", data$alpha),
    #     fill = alpha(data$fill %||% "white", data$alpha),
    #     lty = data$linetype %||% 1
    #     ),
    #   vp = viewport(clip = "on")
    #   )
    pointsGrob(
      0.5, 0.5,
      pch = 21,
      gp = gpar(
        col = alpha(data$colour %||% "black", data$alpha),
        fill = alpha(data$fill %||% "white", data$alpha),
        fontsize = (data$size * 25 %||% 1.5) * .pt + (data$stroke %||% 0.5) * .stroke/2,
        lwd = (data$stroke %||% 0.5) * .stroke/2,
        lty = data$linetype %||% 1
      ),
      vp = viewport(clip = "on")
    )
  },

  draw_panel = function(data, panel_params, coord) {
    munched <- coord_munch(coord, data, panel_params)
    munched <- munched[order(munched$group), ]
    if (!is.integer(munched$group)) {
      munched$group <- match(munched$group, unique0(munched$group))
    }

    # print(munched)

    coords = coord$transform(data, panel_params)

    coords = coords %>%
      group_by(across(everything())) %>%
      summarize(
        ct = n(),
        .groups = "keep"
      ) %>%
      group_by(x, y) %>%
      mutate(
        theta1 = cumsum(ct),
        theta0 = theta1 - ct,
        theta1 = theta1 / sum(ct) * (2 * pi),
        theta0 = theta0 / sum(ct) * (2 * pi),
      )
    pieGrob(
      coords$x, coords$y,
      coords$r0, coords$size,
      coords$theta0, coords$theta1,
      gp = gpar(
        col = coords$colour,
        fill = coords$fill,
        alpha = coords$alpha,
        lty = coords$linetype
      )
    )
  }
)

stat_point_pie = function(
    mapping = NULL, data = NULL, geom = 'point_pie',
    position = 'identity', n = 360, na.rm = FALSE,
    show.legend = NA, inherit.aes = TRUE, ...
  ) {
  layer(
    stat = StatPointPie, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n = n, ...)
  )
}


#' Calculate the amount for each group
StatPointPie = ggproto(
  "StatPointPie", Stat,
  compute_panel = function(data, scales, m, ...) {
    data = data %>%
      group_by(across(everything())) %>%
      summarize(
        amount = n(),
        .groups = "keep"
      ) %>%
      group_by(x, y) %>%
      mutate(
        theta1 = cumsum(ct),
        theta0 = theta1 - ct,
        theta1 = theta1 / sum(ct) * (2 * pi),
        theta0 = theta0 / sum(ct) * (2 * pi),
      )
    print(data)
    return(data)
  }
)

#' Grob to handle a single pie chart.
#'
#' @description
#' Based on grid::polygonGrob.
#' Sample points along the arc to create a pseudo-circle.
#' thetas are assumed to be in radius, not degree/
#'
#' @details
#' Assume x/y coordinate and radius are in the same coordinate system.
#' In other words, I simplyly added those two values.
#' When building geom, maybe need to looking into how to deal with the size
#' and x/y coords.
#'
#' New appraoch: consider using the combination of circle and polygon
#' pointGrob has consistent aspect ratio.
#' So we take the x, y, and r to make our circle
#' Mapped to its perimeter point coordinates with `grid::grobPoints()`.
#' This will be the full circle. Then we map the perimeter point.
#'
#' @importFrom grid gpar polygonGrob
#'
#' @param ... skip for now
pieGrob = function(
    x = 0.5, y = 0.5,
    r0 = 0.0, r1 = 0.3,
    theta0 = 0, theta1 = pi/2,
    n = 360, pch = 21,
    name = NULL,
    position_unit = "npc",
    size_unit = "snpc",
    gp = gpar(), vp = NULL,
    ...
  ) {

  # circles = circleGrob(
  #   x = x,
  #   y = y,
  #   r = r1,
  #   default.units = default.units,
  #   vp = vp
  # )
  #
  # poly_points = grobCoords(
  #   circles,
  #   closed = TRUE
  # )
  # grid::is.unit(poly_points[[1]]$x) %>% print()
  # xs = lapply(poly_points, function(.x) {.x$x})
  # ys = lapply(poly_points, function(.x) {.x$y})
  #
  # id_length = sapply(xs, length)
  # xs = unlist(xs)
  # ys = unlist(ys)
  # # print(xs)
  # polygonGrob(
  #   xs, ys, id.lengths = id_length,
  #   gp = gp, vp = vp, name = name,
  #   default.units = "cm"
  # )


  if (!is.unit(x))
    x <- unit(x, position_unit)
  if (!is.unit(y))
    y <- unit(y, position_unit)

  xv <- convertX(x, position_unit, TRUE)
  yv <- convertY(y, position_unit, TRUE)


  coords = lapply(seq_len(length(x)), function(i) {
    .calc_pie(0, 0, r0[i], r1[i], theta0[i], theta1[i], n)
  })

  xs = lapply(coords, function(i) {i$x})
  xs = Reduce(c, xs, c())
  ys = lapply(coords, function(i) {i$y})
  ys = Reduce(c, ys, c())

  # Offset with the base xy and yv
  xs = unit(xv, position_unit) + unit(xs, size_unit)
  ys = unit(yv, position_unit) + unit(ys, size_unit)

  ids_length = lapply(coords, function(i) {length(i$x)})
  ids_length = Reduce(c, ids_length, c())
  polygonGrob(
    x = xs, y = ys, id.lengths = ids_length,
    gp = gp, vp = vp, name = name,
    ...
  )

}



GeomSimpleCircle = ggproto(
  "GeomSimpleCircle", Geom,
  draw_panel = function(data, panel_params, coord) {
    coords = coord$transform(data, panel_params)

    pieGrob(
      x = coords$x,
      y = coords$y,
      r1 = coords$r
    )
  }
)

# polygonGrob(
#   grobCoords(circleGrob(), closed = TRUE, units = "npc")[[1]]$x,
#   grobCoords(circleGrob(), closed = TRUE, units = "npc")[[1]]$y
# ) %>%
#   grid.draw()
#
#
# lapply(pts, grobPoints, closed = TRUE)
#
# grobPoints(pts,closed = TRUE)[[1]]
#
#
# pts = circleGrob(c(0.3, 0.7), c(0.3, 0.7), c(0.1, 0.2))

normalize_vec = function(vec, size_max = 1, size_min = 0) {
  size_factor = (max(vec) - min(vec)) / (size_max - size_min)
  vec = (vec - min(vec)) / size_factor + size_min
  vec
}

grob_to_ploygon = function(grob_in, outunit = "npc") {
  poly_points = grobCoords(grob_in, FALSE)
  # print(str(current.viewport()))
  xs = lapply(poly_points, function(.x) {.x$x})
  ys = lapply(poly_points, function(.x) {.x$y})

  id_length = sapply(xs, length)
  xs = unlist(xs)
  ys = unlist(ys)

  # xs = normalize_vec(xs)
  # ys = normalize_vec(ys)

  polygonGrob(xs, ys, id.lengths = id_length, default.units = "cm")
}

#' Generate coordinates along an arc.
#' @param .r The radius of the arc
#' @param .theta0 Start of the arc.
#' @param n The number of points to sample. Note, this is the number to used for the entire circle
#' @param go_cww Should the points be calculate in CounterClockWise order?
#'
#' @return A named list of length 2, specifying the x and y coordinates for the arc required.
.calc_arc = function(.x, .y, .r, .theta0, .theta1, n, go_ccw = TRUE) {
  n_out = abs(.theta0 - .theta1) / (2 * pi) * n
  n_out = ceiling(n_out)

  step = abs(.theta0 - .theta1) / n_out
  rand = rep(.theta0, times = n_out + 1)
  if (go_ccw) {
    rand = rand + 0:n_out * step
  } else {
    rand = rand + n_out:0 * step
  }


  # In local space, center at 0, 0
  out = list(
    x = sapply(rand, function(i) {.r * cos(i)}),
    y = sapply(rand, function(i) {.r * sin(i)})
  )

  # shift to .x, .y
  out$x = out$x + .x
  out$y = out$y + .y

  return(out)
}

#' Generate the coordinate for a single polygonGrob
.calc_pie = function(.x, .y, .r0, .r1, .theta0, .theta1, n = 360) {
  if (.r0 > 0) {
    coords = list()
    outer = .calc_arc(.x, .y, .r1, .theta0, .theta1, n)
    inner = .calc_arc(.x, .y, .r0, .theta0, .theta1, n, go_ccw = FALSE)
    coords$x = c(outer$x, inner$x)
    coords$y = c(outer$y, inner$y)
    return(coords)

  } else {
    coords = .calc_arc(.x, .y, .r1, .theta0, .theta1, n)
    coords$x = c(.x, coords$x)
    coords$y = c(.y, coords$y)
    return(coords)
  }

}



