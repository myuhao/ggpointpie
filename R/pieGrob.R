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



