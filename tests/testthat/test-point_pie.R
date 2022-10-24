# test_that(
#   "Test GeomPointPie Object",
#   code = {
#     iris %>%
#       ggplot(aes(x = Sepal.Length, y = Sepal.Width)) +
#       geom_point_pie(aes(color = Species))
#   }
# )
#
#
#
# test_that("Test pieGrob", {
#   out = pieGrob(
#     x = c(0.5, 0.5),
#     y = c(0.5, 0.5),
#     r0 = c(0, 0),
#     r1 = c(0.3, 0.3),
#     theta0 = c(0, pi),
#     theta1 = c(pi,2 * pi),
#     gp = gpar(fill = c("red", "blue"))
#   )
#   grid.draw(out)
# }
# )
#
# test_that("Basic geom_pie_point", {
#   dat = tibble::tibble(
#     x = rep(1, 5),
#     y = rep(1, 5)
#   ) %>%
#     dplyr::mutate(
#       r_idx = dplyr::row_number(),
#       grp = sample(1:10, 5),
#       grp = map2(grp, r_idx, ~rep(letters[.y], times = .x))
#     ) %>%
#     tidyr::unnest(grp)
#   dat %>%
#     ggplot(aes(x = x, y = y)) +
#     geom_point_pie(aes(group = grp, fill = grp))
#
#
#   dat2 = tibble::tibble(
#     x = 1:5,
#     y = 1:5
#   ) %>%
#     mutate(
#       data = map(x, ~select(dat, -c("x", "y")))
#     ) %>%
#     tidyr::unnest(data)
#
#   dat2 %>%
#     mutate(x = as.character(x), size = normalize_size(y)) %>%
#     ggplot(aes(x = x, y = y)) +
#     geom_point_pie(aes(fill = grp, size = size, linetype = y == 2)) +
#     scale_fill_viridis_d() +
#     scale_x_discrete(expand = expansion(add = 1)) +
#     scale_y_continuous(expand = expansion(add = 1)) +
#     scale_size_continuous(range = c(0.05, 0.30)) +
#     facet_wrap(
#       vars(x),
#       scales = "free"
#     ) +
#     guides(
#       linetype = guide_legend(
#         override.aes = list(fill = NA)
#       ),
#       size = guide_legend(
#         override.aes = list(fill = NA)
#       )
#     )
#
#   dat2 %>%
#     mutate(x = as.character(x), size = normalize_size(y)) %>%
#     ggplot(aes(x = x, y = y)) +
#     geom_point(size = 20)  +
#     facet_wrap(
#       vars(x),
#       scales = "free"
#     ) +
#     scale_y_discrete(limits = c(4, 6))
#
#   tibble::tibble(
#     x = letters[1:5],
#     y = letters[1:5]
#   ) %>%
#     mutate(
#       data = map(x, ~select(dat, -c("x", "y")))
#     ) %>%
#     tidyr::unnest(data) %>%
#     mutate(x = as.character(x)) %>%
#     ggplot(aes(x = x, y = y)) +
#     geom_point_pie(aes(fill = grp, linetype = y == "a")) +
#     # scale_fill_viridis_d() +
#     # scale_x_discrete(expand = expansion(add = 1)) +
#     # scale_y_continuous(expand = expansion(add = 1)) +
#     # scale_size_continuous(range = c(0.05, 0.10)) +
#     facet_wrap(
#       vars(x),
#       scales = "free"
#     ) +
#     guides(
#       linetype = guide_legend(
#         override.aes = list(fill = NA)
#       ),
#       size = guide_legend(
#         override.aes = list(fill = NA)
#       )
#     )
# })


test_that("Check circlGrob and pieGrob look the same", {
  xs = c(0.3, 0.7)
  ys = c(0.3, 0.7)
  rs = c(0.1, 0.3)

  vdiffr::expect_doppelganger("pointGrob", grid.draw(circleGrob(xs, ys, rs)))

  vdiffr::expect_doppelganger("pieGrob", grid.draw(pieGrob(xs, ys, r1 = rs, theta0 = 0, theta1 = 2 * pi)))
})



dat = tibble::tibble(
  x = rep(1, 5),
  y = rep(1, 5)
  ) %>%
  dplyr::mutate(
    r_idx = dplyr::row_number(),
    grp = sample(1:10, 5),
    grp = map2(grp, r_idx, ~rep(letters[.y], times = .x))
  ) %>%
  tidyr::unnest(grp)


tibble::tibble(
  x = letters[1:5],
  y = letters[1:5],
  r = 1:5 / 10,
  # x = 1:5,
  # y = 1:5
) %>%
  mutate(
    data = map(x, ~select(dat, -c("x", "y")))
  ) %>%
  tidyr::unnest(data) %>%
  group_by(across(everything())) %>%
  summarise(amount = n()) %>%
  ungroup() %>%
  ggplot(aes()) +
  layer(
    geom = GeomPointPie, stat = StatIdentity, mapping = aes(x = x, y = y, r1 = 0.1), position = PositionIdentity
  ) +
  facet_wrap(vars(x), ncol = 1, scales = "free")
