


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


#' @title
#' StatPointPie
#'
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
#' @importFrom dplyr across everything n
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
      summarize(ct = n(), .groups = "keep") %>%
      ungroup()
  },
  required_aes = c("x", "y")
)



