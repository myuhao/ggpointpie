make_ggplot2_test_data = function(n_points = 5) {
  dat = tibble::tibble(
    x = rep(1, 5),
    y = rep(1, 5)
  ) %>%
    dplyr::mutate(
      r_idx = dplyr::row_number(),
      grp = c(9, 2, 10, 1, 7),
      grp = map2(grp, r_idx, ~rep(letters[.y], times = .x)),
      grp_2 = rep(c("m", "n"), length.out = n())
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

test_that("Test using amount", {
  plot_1 = make_ggplot2_test_data() %>%
    group_by(across(everything())) %>%
    summarize(am = n()) %>%
    ggplot(aes(x = x_d, y = y_d)) +
    geom_point_pie(aes(fill = grp, amount = am)) +
    facet_wrap(vars(x_d), scales = "free", nrow = 1)

  vdiffr::expect_doppelganger("Use amouny + stat_identity", plot_1)
})


test_that("Test group work in concentric circle", {
  plot_1 = make_ggplot2_test_data() %>%
    mutate(
      grp_2 = ifelse(x_d %in% c("A", "B"), "m", "n"),
      x_d = "A", y_d = "A",
      r = ifelse(grp_2 == "m", 0.3, 0.1),
    ) %>%
    ggplot(aes(x = x_d, y = y_d)) +
    geom_point_pie(aes(fill = grp, subgroup = grp_2, r1 = r))

  vdiffr::expect_doppelganger("Use subgroup", plot_1)
})
