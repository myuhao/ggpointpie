library(grid)

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
