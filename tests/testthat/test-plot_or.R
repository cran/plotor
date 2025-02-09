
testthat::test_that("plotor output does not produce messages or warnings", {
  testthat::expect_silent({

        df <- datasets::Titanic |>
          dplyr::as_tibble() |>
          dplyr::filter(n > 0) |>
          tidyr::uncount(weights = n) |>
          dplyr::mutate(
            Class = Class |>
              forcats::fct(levels = c('1st', '2nd', '3rd', 'Crew')),
            Sex = Sex |> forcats::fct_infreq(),
            Age = Age |> forcats::fct_infreq(),
            Survived = Survived |> forcats::fct(levels = c('No', 'Yes'))
          )

        lr <- stats::glm(
          data = df,
          family = 'binomial',
          formula = Survived ~ Class + Sex + Age
        )

        plotor::plot_or(lr)

  })
})
