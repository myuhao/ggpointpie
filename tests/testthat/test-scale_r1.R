make_ggplot2_test_data = function(n_points = 5) {
  dat = tibble::tibble(
    x = rep(1, 5),
    y = rep(1, 5)
  ) %>%
    dplyr::mutate(
      r_idx = dplyr::row_number(),
      grp = c(9, 2, 10, 1, 7),
      grp = map2(grp, r_idx, ~rep(letters[.y], times = .x))
    ) %>%
    tidyr::unnest(grp)

  tibble::tibble(
    r = 1:n_points / (2 * n_points),
    x_c = 1:n_points,
    y_c = 1:n_points,
    x_d = LETTERS[x_c],
    y_d = LETTERS[y_c]
  ) %>%
    mutate(
      data = map(x_c, ~select(dat, -c("x", "y")))
    ) %>%
    tidyr::unnest(data)
}

test_that("Test scale default", {
  continuous = make_ggplot2_test_data() %>%
    ggplot(aes(x = x_d, y = y_d)) +
    geom_point_pie(
      aes(
        group = grp,
        fill = r * 10,
        r1 = r
      )
    ) +
    facet_wrap(vars(x_d), scales = "free") +
    scale_color_viridis_c() +
    scale_fill_viridis_c(option = "E")

  vdiffr::expect_doppelganger("Default continuous scale", continuous)

  discrete_error = make_ggplot2_test_data() %>%
    ggplot(aes(x = x_d, y = y_d)) +
    geom_point_pie(
      aes(
        group = grp,
        fill = r * 10,
        r1 = grp
      )
    ) +
    facet_wrap(vars(x_d), scales = "free") +
    scale_color_viridis_c() +
    scale_fill_viridis_c(option = "E")

  testthat::expect_error(discrete_error %>% print())
})
