#' @title
#' Draw pie chart at any x,y coordinates.
#'
#' @rdname point_pie
#' @name point_pie
#'
#' @description
#' This geom is inherits from on [ggplot2::GeomPolygon]. W
#' e used polygon geom to create pie chart than can be mapped to
#' arbitrary x and y coordinates.
#' A advantage of this geom over other implementation is that is keeps its
#' aspect ratio so the point is always a perfect circle.
#' It can also be mapped to both continuous and discrete position scales.
#'
#' @section
#' Aesthetics:
#' [geom_point_pie] understand the following aesthetics (required aesthetics are in
#' bold):
#'
#' - **x**
#' - **y**
#' - **fill**
#' - subgroup
#' - color: line color
#' - r1: the size of the pie chart.
#' - linetype
#' - alpha
#'
#' @section
#' `subgroup`:
#' The subgroup aesthetics refers to a set of observations that belong to the
#' same pie. It is used to calculate the total count.
#' If group is not explicitly specified, we will use the `x` and `y` aesthetics
#' as group. We also assume that if two observation belongs to the same subgroup,
#' they have the same x,y coordinates.
#'
#' **Note:**, if two groups have the same coordinates,
#' such as making two concentric pie charts,
#' the default group calculation is not accurate.
#'
#' On the other hand, fill is referring to the category within each group.
#' In other word, it determines the angle of each slice of the pie.
#' It does not make sense to have fill to be transparent.
#'
#' @section
#' [stat_point_pie]:
#' By default, we assume the data is in a long format, where each row
#' corresponds to one observation. In this case, [stat_point_pie] is called to
#' help generated count for *each slice* of the pie.
#'
#' You can generate your own *per slice* count. In this case, map it to the
#' aesthetics called `amount`.
#'
#' @inheritParams ggplot2::geom_polygon
#' @param r0 The radius (0-1) of the inner circle, in case a donut plot is needed.
#'
#' @importFrom ggplot2 layer ggproto aes
#'
#' @examples
#' data = tibble::tibble(
#'     x = c(1, 1, 1, 2, 2, 2, 2),
#'     y = c(1, 1, 1, 2, 2, 2, 2),
#'     grp = c(a, a, b, a, a, b, b)
#' )
#' ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
#'   ggpointpie::geom_point_pie(ggplot2::aes(group = grp, fill = grp))
NULL

#' @title
#' A geom to plot pie chart with arbitary size and location.
#'
#' @rdname point_pie
#' @export
geom_point_pie <- function(
    mapping = NULL, data = NULL, stat = StatPointPie,
    position = "identity", r0 = 0L, na.rm = FALSE, show.legend = NA,
    inherit.aes = TRUE, ...
) {
  layer(
    geom = GeomPointPie, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, r0 = r0, ...)
  )
}


#' @title
#' PointPieGeom Proto
#'
#' @description
#' ggproto class that descripe combination of pie chart and point.
#'
#' ggplot always map size to 1-6. Use scale_size_continuous to deal with it.
#' In the future, maybe get our own scale...
#'
#' @rdname point_pie
#'
#' @importFrom ggplot2 GeomPolygon .pt .stroke
#' @importFrom scales alpha
#' @importFrom grid viewport pointsGrob
#' @importFrom rlang `%||%`
#' @importFrom dplyr group_by mutate ungroup
#'
#' @param ... skip for now
#'
#' todo: think about edge case where there is only 1 observation
#' todo: deal with empty factor levels, what do we return?
#'
#' @export
#'
GeomPointPie = ggproto(
  "GeomPointPie", GeomPolygon,
  required_aes = c("x", "y", "fill"),
  default_aes = aes(
    colour = "black", fill = NA, r0 = 0, r1 = 0.3,
    alpha = 1, linetype = 1, amount = -1L, subgroup = NA
  ),

  draw_key = function(data, params, size) {
    # For lty:
    if (length(unique(data$linetype)) > 1) {
      return(GeomLine$draw_key(data, params, size))
    }

    pieGrob(
      rep(0.5, 3), rep(0.5, 3),
      0, data$r1 %||% 0.3,
      c(0.5*pi, 1 * pi, 1.6 * pi), c(1 * pi, 1.6 * pi, 0.5 * pi),
      gp = gpar(
        col = alpha(data$colour %||%"black", data$alpha),
        fill = alpha(data$fill %||% "white", data$alpha),
        lty = data$linetype %||% 1
        ),
      vp = viewport(clip = "on")
      )
  },

  draw_panel = function(data, panel_params, coord) {
    coords = coord$transform(data, panel_params)
    coords = coords %>%
      group_by(x, y, subgroup) %>%
      mutate(
        amount = ifelse(amount == -1L, ct, amount), # Handle use of stat_identity
        theta1 = cumsum(amount),
        theta0 = theta1 - amount,
        theta1 = theta1 / sum(amount) * (2 * pi),
        theta0 = theta0 / sum(amount) * (2 * pi),
      ) %>%
      ungroup()
    # print(coords[c('x', 'y', "size", 'theta1', 'theta0', 'amount')])

    # Each line is a
    pieGrob(
      coords$x, coords$y,
      coords$r0, coords$r1,
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


#' @rdname point_pie
#' @export
stat_point_pie = function(
    mapping = NULL, data = NULL, geom = GeomPointPie,
    position = 'identity', na.rm = FALSE,
    show.legend = NA, inherit.aes = TRUE, ...
  ) {
  layer(
    data = data, mapping = mapping, geom = geom, stat = StatPointPie,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' @details
#' Rethink about the design. Use this stat
#' to transform the "long" format data (one observation per row)
#' into the proper plot format, i.e, a piece of the pie per row.
#'
#' This probably allow user to supply different data input, and they can
#' just choose different stat when call geom...
#'
#'
#' Use StatPointPie to turn long format to short format:
#' group by `fill` + either `group` or `x,y`,
#' count the number of observations.
#'
#'
#' if `group` is supplied, Stat will recieve each group
#'
#' @importFrom ggplot2 Stat
#'
#' @rdname geom_point_pie
#'
#' @export
StatPointPie = ggproto(
  "StatPointPie",
  Stat,
  compute_group = function(self, data, scales) {
    # Count each unique combination
    # print(data)
    # ct = nrow(data)
    # data = head(data, n = 1L)
    # data$ct = ct
    # data

    data %>%
      group_by(across(everything())) %>%
      summarize(ct = n()) %>%
      ungroup()
  },
  required_aes = c("x", "y")
)



#' @title
#' Grob for a pie chart.
#'
#' @description
#' Use to create grob that is a piece of the pie chart, with parameters...
#' Build on top of the [grid::polygonGrob()].
#' Sample points along the arc to create a pseudo-circle.
#' Maintain aspect ratio.
#'
#'
#'
#' @importFrom grid gpar polygonGrob convertX convertY unit
#'
#' @param x,y The x and y coordinates for this grob, in `position_unit`.
#' @param r0,r1 The inner and outer arc radius, in `size_unit`.
#' @param thrta0,theta1 The start and end angle of the piece of pie, in radius.
#' @param n The number of points to sample.
#' @param position_unit The grid unit to use for where to put the grob.
#' @param size_unit The grid unit to use for the size of the grob.
#' @param gp,vp,... Parameters for [grid::polygonGrob()].
#'
#' @return A [grid::polygonGrob()] object that represents a piece of the pie chart.
#'
#' @details
#' So [gridExtra::ngonGrob()] is able to maintain aspect ratio even using
#' the [grid::polygonGrob()] under its hood. Inspired by the gridExtra approach, we
#' use separately specify the **position** (npc-based) and **size** (snpc-base)
#' parameters. In the end, the grob will be located at the desired location
#' with its radius proportional to the smaller of the width and height
#' of the current viewport. Everything is normalized so [0-1] can be used.
#'
#' @export
pieGrob = function(
    x = 0.5, y = 0.5,
    r0 = 0.0, r1 = 0.3,
    theta0 = 0, theta1 = pi/2,
    n = 360,
    position_unit = "npc",
    size_unit = "snpc",
    gp = gpar(), vp = NULL,
    ...
  ) {
  exp_len = length(x)
  stopifnot(length(y) == exp_len)

  r0 = rep(r0, length.out = exp_len)
  r1 = rep(r1, length.out = exp_len)
  theta0 = rep(theta0, length.out = exp_len)
  theta1 = rep(theta1, length.out = exp_len)

  if (!is.unit(x))
    x <- unit(x, position_unit)
  if (!is.unit(y))
    y <- unit(y, position_unit)

  xv <- convertX(x, position_unit, TRUE)
  yv <- convertY(y, position_unit, TRUE)

  coords = lapply(seq_len(length(x)), function(i) {
    out = .calc_pie(0, 0, r0[i], r1[i], theta0[i], theta1[i], n)

    # Offset with the base xy and yv
    out$x = unit(xv[[i]], position_unit) + unit(out$x, size_unit)
    out$y = unit(yv[[i]], position_unit) + unit(out$y, size_unit)

    return(out)
  })

  xs = lapply(coords, function(i) {i$x})
  xs = Reduce(unit.c, xs)
  ys = lapply(coords, function(i) {i$y})
  ys = Reduce(unit.c, ys)
  ids_length = lapply(coords, function(i) {length(i$x)})
  ids_length = Reduce(c, ids_length, c())

  polygonGrob(
    x = xs, y = ys, id.lengths = ids_length,
    gp = gp, vp = vp,
    ...
  )
}


#---------------------------------- Helpers -----------------------------------#

#' Generate coordinates along an arc.
#' @param .r The radius of the arc
#' @param .theta0 Start of the arc.
#' @param n The number of points to sample. Note, this is the number to used for the entire circle
#' @param go_cww Should the points be calculate in CounterClockWise order?
#'
#' @return A named list of length 2, specifying the x and y coordinates for the arc required.
#'
#' @keywords internal
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
#' In the case where delta theta > 2 * pi,
#' will not draw the center.
#' @keywords internal
.calc_pie = function(.x, .y, .r0, .r1, .theta0, .theta1, n = 360) {
  is_less_2pi = (.theta1 - .theta0) < (2 * pi)
  if (.r0 > 0) {
    coords = list()
    outer = .calc_arc(.x, .y, .r1, .theta0, .theta1, n)
    inner = .calc_arc(.x, .y, .r0, .theta0, .theta1, n, go_ccw = FALSE)
    coords$x = c(outer$x, inner$x)
    coords$y = c(outer$y, inner$y)
    return(coords)

  } else {
    coords = .calc_arc(.x, .y, .r1, .theta0, .theta1, n)
    if (is_less_2pi) {
      coords$x = c(.x, coords$x)
      coords$y = c(.y, coords$y)
    }
    return(coords)
  }

}



