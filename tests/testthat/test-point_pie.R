library(tidyverse)
library(grid)
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

test_that("Test pieGrob with default", {
  vdiffr::expect_doppelganger(
    "Draw pieGrob directly as circle",
    grid.draw(pieGrob())
  )
})


test_that("Test pieGrob with multiple groups.", {
  xs = rep(0.5, 5)
  ys = rep(0.5, 5)
  amount = c(3, 8, 3, 2, 4)
  theta1 = cumsum(amount)
  theta0 = theta1 - amount
  theta1 = theta1 / sum(amount) * (2 * pi)
  theta0 = theta0 / sum(amount) * (2 * pi)
  fills = scales::viridis_pal()(length(xs))

  vdiffr::expect_doppelganger(
    "Check a single pie works with pieGrob",
    grid.draw(
      pieGrob(xs, ys, 0, 0.3, theta0, theta1, gp = gpar(fill = fills, lty = 2))
    )
  )
})


test_that("Test pieGrob() can vectorize parameters", {
  xs = c(0.3, 0.7)
  ys = c(0.3, 0.7)

  vdiffr::expect_doppelganger(
    "One group.",
    grid.draw(pieGrob(xs, ys, 0, 0.3, 0, 2 * pi, n = 90))
  )

  coords = tibble::tibble(
    xs = rep(c(0.2, 0.7), each = 3),
    ys = rep(c(0.2, 0.7), each = 3),
    amount = c(3, 8, 3, 2, 4, 5)
  ) %>%
    group_by(xs, ys) %>%
    mutate(
      theta1 = cumsum(amount),
      theta0 = theta1 - amount,
      theta1 = theta1 / sum(amount) * (2 * pi),
      theta0 = theta0 / sum(amount) * (2 * pi)
    )

  vdiffr::expect_doppelganger(
    "More than one group",
    grid.draw(pieGrob(coords$xs, coords$ys, 0, 0.3, coords$theta0, coords$theta1))
  )
})


#' geom testings
test_that("Test geom in continuous_x|y_scales", {

  plot_single = make_ggplot2_test_data() %>%
    filter(x_c == 1) %>%
    ggplot(aes(x = x_c, y = y_c)) +
    geom_point_pie(aes(fill = grp))

  plot_all = make_ggplot2_test_data() %>%
    ggplot(aes(x = x_c, y = y_c)) +
    geom_point_pie(aes(fill = grp))

  plot_facet = make_ggplot2_test_data() %>%
    ggplot(aes(x = x_c, y = y_c)) +
    geom_point_pie(aes(group = grp, fill = grp)) +
    facet_wrap(vars(x_d), scales = "free", nrow = 1)

  vdiffr::expect_doppelganger("Continuous Single Pie Geom", plot_single)
  vdiffr::expect_doppelganger("Continuous Multiple Pie Geom", plot_all)
  vdiffr::expect_doppelganger("Continuous Facet Pie Geom", plot_facet)

})

test_that("Test geom in discrete_x|y_scales", {
  plot_single = make_ggplot2_test_data() %>%
    filter(x_c == 1) %>%
    ggplot(aes(x = x_d, y = y_d)) +
    geom_point_pie(aes(group = grp, fill = grp))

  plot_all = make_ggplot2_test_data() %>%
    ggplot(aes(x = x_d, y = y_d)) +
    geom_point_pie(aes(group = grp, fill = grp))

  plot_facet = make_ggplot2_test_data() %>%
    ggplot(aes(x = x_d, y = y_d)) +
    geom_point_pie(aes(group = grp, fill = grp)) +
    facet_wrap(vars(x_d), scales = "free", nrow = 1)
  vdiffr::expect_doppelganger("Discrete Single Pie Geom", plot_single)
  vdiffr::expect_doppelganger("Discrete Multiple Pie Geom", plot_all)
  vdiffr::expect_doppelganger("Discrete Facet Pie Geom", plot_facet)
})


test_that("Test aes mapping", {
  plot_discrete_aes = make_ggplot2_test_data() %>%
    ggplot(aes(x = x_d, y = y_d)) +
    geom_point_pie(
      aes(
        group = grp,
        fill = grp,
        color = letters[x_c],
        linetype = grp == "a"
      )
    ) +
    facet_wrap(vars(x_d), scales = "free")

  plot_continuous_aes = make_ggplot2_test_data() %>%
    ggplot(aes(x = x_d, y = y_d)) +
    geom_point_pie(
      aes(
        group = grp,
        fill = r * 10,
        color = r + 1,
        r1 = r
      )
    ) +
    facet_wrap(vars(x_d), scales = "free") +
    scale_color_viridis_c() +
    scale_fill_viridis_c(option = "E")

  vdiffr::expect_doppelganger("Check discrete variables work", plot_discrete_aes)
  vdiffr::expect_doppelganger("Check continuous variables work", plot_continuous_aes)

})


test_that("Test guides", {
  plot_guides = make_ggplot2_test_data() %>%
    ggplot(aes(x = x_d, y = y_d)) +
    geom_point_pie(
      aes(
        group = grp,
        fill = grp,
        r1 = r,
        color = letters[x_c],
        linetype = grp == "a",
        alpha = grp == "c"
      )
    ) +
    facet_wrap(vars(x_d), scales = "free")
  vdiffr::expect_doppelganger("Test draw_key works", suppressWarnings(plot_guides))
})

test_that("Test one category, delta theta = 2 * pi", {
  one_category = make_ggplot2_test_data() %>%
    filter(grp == "a") %>%
    ggplot(aes(x = x_d, y = y_d)) +
    geom_point_pie(
      aes(
        group = grp,
        fill = grp,
        r1 = r
      )
    ) +
    facet_wrap(vars(x_d), scales = "free")
  vdiffr::expect_doppelganger("Test only one category", one_category)
})

test_that("Test NAs", {
  one_NA_per_panel = make_ggplot2_test_data() %>%
    group_by(x_d) %>%
    mutate(
      grp = map_at(grp, 1, ~NA_character_),
      grp = map_chr(grp, ~.x)
    ) %>%
    ungroup() %>%
    ggplot(aes(x = x_d, y = y_d)) +
    geom_point_pie(
      aes(
        group = grp,
        fill = grp,
        r1 = r
      ), na.rm = TRUE
    ) +
    facet_wrap(vars(x_d), scales = "free")
  vdiffr::expect_doppelganger("Remove NAs", one_NA_per_panel)
})

test_that("Test using amount", {
  plot_1 = make_ggplot2_test_data() %>%
    group_by(across(everything())) %>%
    summarize(am = n()) %>%
    ggplot(aes(x = x_d, y = y_d)) +
    geom_point_pie(aes(fill = grp, amount = am), stat = "identity") +
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


test_that("Deal with missing in discrete scale", {
  tidyr::expand_grid(
    x = letters[1:5],
    y = c("m", "n")
  ) %>%
    mutate(
      x = factor(x),
      y = factor(y),
      grp = map(x, ~make_ggplot2_test_data()$grp)
    ) %>%
    tidyr::unnest(grp) %>%
    filter(x != "b" | y != "n") %>%
    ggplot(aes(x = x, y = y)) +
    geom_point_pie(aes(group = grp, fill = grp), r1 = 0.2) +
    facet_grid(
      rows = vars(y),
      cols = vars(x),
      scales = "free",
      drop = FALSE
    )
})


