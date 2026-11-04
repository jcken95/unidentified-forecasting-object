# fit all models to data

source("analysis/01-exploratory.R")

model_ready <- ufo_complete |>
  dplyr::arrange(month_year) |>
  dplyr::mutate(
    t_numeric = dplyr::row_number(),
    .by = month_year
  ) |>
  dplyr::mutate(month = forcats::as_factor(format(month_year, "%B")),
                country_code = forcats::as_factor(country_code))


gaussian_process(model_ready, 5)
