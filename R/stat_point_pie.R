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
#' The stat to calculate the count of each subgroup to handle the long data
#' format
#'
#' @importFrom ggplot2 Stat
#' @importFrom dplyr across everything n summarize
#'
#' @name ggproto-subclass
#' @rdname ggproto-subclass
#'
#' @export
StatPointPie = ggproto(
  "StatPointPie",
  Stat,
  compute_group = function(self, data, scales) {
    data %>%
      group_by(across(everything())) %>%
      summarize(ct = n(), .groups = "keep") %>%
      ungroup()
  },
  required_aes = c("x", "y")
)



