test_that(
  "Test GeomPointPie Object",
  code = {
    iris %>%
      ggplot(aes(x = Sepal.Length, y = Sepal.Width)) +
      geom_point_pie(aes(color = Species))
  }
)



test_that("Test pieGrob", {
  out = pieGrob(
    x = c(0.5, 0.5),
    y = c(0.5, 0.5),
    r0 = c(0, 0),
    r1 = c(0.3, 0.3),
    theta0 = c(0, pi),
    theta1 = c(pi,2 * pi),
    gp = gpar(fill = c("red", "blue"))
  )
  grid.draw(out)
}
)

test_that("Basic geom_pie_point", {
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
  dat %>%
    ggplot(aes(x = x, y = y)) +
    geom_point_pie(aes(group = grp, fill = grp))


  dat2 = tibble::tibble(
    x = 1:5,
    y = 1:5
  ) %>%
    mutate(
      data = map(x, ~select(dat, -c("x", "y")))
    ) %>%
    tidyr::unnest(data)

  dat2 %>%
    mutate(x = as.character(x), size = normalize_size(y)) %>%
    ggplot(aes(x = x, y = y)) +
    geom_point_pie(aes(fill = grp, size = size, linetype = y == 2)) +
    scale_fill_viridis_d() +
    scale_x_discrete(expand = expansion(add = 1)) +
    scale_y_continuous(expand = expansion(add = 1)) +
    scale_size_continuous(range = c(0.05, 0.10)) +
    facet_wrap(
      vars(x),
      scales = "free"
    ) +
    guides(
      linetype = guide_legend(
        override.aes = list(fill = NA)
      ),
      size = guide_legend(
        override.aes = list(fill = NA)
      )
    )

  tibble::tibble(
    x = letters[1:5],
    y = letters[1:5]
  ) %>%
    mutate(
      data = map(x, ~select(dat, -c("x", "y")))
    ) %>%
    tidyr::unnest(data) %>%
    mutate(x = as.character(x), size = y) %>%
    ggplot(aes(x = x, y = y)) +
    geom_point_pie(aes(fill = grp, size = size, linetype = y == "a")) +
    # scale_fill_viridis_d() +
    # scale_x_discrete(expand = expansion(add = 1)) +
    # scale_y_continuous(expand = expansion(add = 1)) +
    # scale_size_continuous(range = c(0.05, 0.10)) +
    facet_wrap(
      vars(x),
      scales = "free"
    )
})
