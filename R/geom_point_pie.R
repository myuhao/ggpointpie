#' @title
#' Draw pie chart at any x,y coordinates.
#'
#' @rdname point_pie
#' @name point_pie
#'
#' @description
#' This geom is used to draw any number of pie chart at any
#' arbitrary x and y coordinates.
#' A advantage of this geom over other implementation is that is keeps its
#' aspect ratio so the point is always a perfect circle.
#' It can also be mapped to both continuous and discrete position scales.
#'
#' @section
#' Aesthetics:
#' [geom_point_pie()] understand the following aesthetics
#' (required aesthetics are in bold):
#'
#' - **x**
#' - **y**
#' - **fill**: use this to specify the category.
#' - subgroup: see [@details ]
#' - amount: see [@details ]
#' - color: line color.
#' - r1: the size of the pie chart.
#' - linetype
#' - alpha
#'
#' @details
#' `subgroup`:
#' The subgroup aesthetics refers to a set of observations that belong to the
#' same pie. It is used to calculate the total count. Most of the time you
#' don't need to map this, and we will use the combination of x and y
#' coordiantes to decide which observations belong to the same pie chart.
#' **However**, if two subgroups have the same coordinates,
#' such as when making two concentric pie charts,
#' the default subgroups calculation is not accurate.
#' You will need to supply the subgroup.
#'
#' `amount`:
#' Alternatively, you can calculate the per-subgroup total count yourself and
#' map it to amount.
#'
#' @section
#' Stat:
#' By default, we assume the data is in a long format, where each row
#' corresponds to one observation. In this case, [stat_point_pie()] is called to
#' help generated count for *each slice* of the pie.
#'
#' You can generate your own *per slice* count. In this case, map it to the
#' aesthetics called `amount`.
#'
#' @inheritParams ggplot2::geom_polygon
#' @param r0 The radius (0-1) of the inner circle, in case a donut plot is needed.
#'
#' @importFrom ggplot2 layer ggproto aes
#' @importFrom magrittr %>%
#'
#' @examples
#' # Two pie charts:
#' data = tibble::tibble(
#'     x = c(1, 1, 1, 2, 2, 2, 2),
#'     y = c(1, 1, 1, 2, 2, 2, 2),
#'     grp = c("a", "a", "b", "a", "a", "b", "b")
#' )
#' ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
#'   ggpointpie::geom_point_pie(ggplot2::aes(fill = grp))
NULL


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
#' PointPie ggproto classes
#'
#' @description
#' [ggproto()] class that describes combination of pie chart and point.
#'
#'
#' @name ggproto-subclass
#' @rdname ggproto-subclass
#'
#' @importFrom ggplot2 GeomPolygon .pt .stroke
#' @importFrom scales alpha
#' @importFrom grid viewport pointsGrob
#' @importFrom rlang `%||%`
#' @importFrom dplyr group_by mutate ungroup
#'
#' @param amount Useful when the user calculate the amount for each subgroup.
#' @param subgroup Each complete pie chart
#'
#' @details
#' This is the subclass inherited from [ggproto()]. Use the main layer functions
#' [geom_point_pie()] instead of this object.
#'
#'
#' @export
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
