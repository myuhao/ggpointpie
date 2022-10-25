#' @title
#' The scales to map the point pie radius.
#'
#' @description
#' These scales are used for the `r1` aes in [geom_point_pie] layer as default.
#' We currently do not support any discrete scale.
#'
scale_r1 = function(..., range = c(0.1, 0.4)) {
  ggplot2::continuous_scale("r1", "r1_c", scales::rescale_pal(range))
}

#' @export
scale_r1_discrete <- function(...) {
  rlang::abort("Pie point size cannot be used with discrete data at this moment")
}

#' @export
scale_r1_continuous = scale_r1
