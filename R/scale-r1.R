#' @title
#' The scales to map the point pie radius.
#'
#' @name scale_r1
#' @description
#' These scales are used for the `r1` aes in [geom_point_pie] layer as default.
#' We currently do not support any discrete scale.
#' @inheritParams ggplot2::continuous_scale
#'
#' @seealso [ggplot2::scale_size_continuous()]
#'
#' @importFrom ggplot2 waiver
NULL


scale_r1 = function(
    name = waiver(), breaks = waiver(), labels = waiver(),
    limits = NULL, range = c(0.1, 0.4), trans = "identity",
    guide = "legend"
  ) {
  ggplot2::continuous_scale(
    "r1", "r1_c",
    scales::rescale_pal(range),
    name = name,
    breaks = breaks,
    labels = labels,
    limits = limits,
    trans = trans,
    guide = guide
  )
}

#' Discrete scale doesn't make sense for r1.
#' @export
scale_r1_discrete <- function(...) {
  rlang::abort("Pie point size cannot be used with discrete data at this moment")
}

#' @rdname scale_r1
#' @export
scale_r1_continuous = scale_r1
