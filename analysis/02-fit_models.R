# fit all models to data

source("analysis/01-exploratory.R")

model_files <- fs::dir_ls(here::here("models"))

purrr::walk(model_files, source)

model_ready <- ufo_complete |>
  dplyr::arrange(month_year) |>
  dplyr::mutate(
    t_numeric = dplyr::row_number(),
    .by = country_code
  ) |>
  dplyr::mutate(month = forcats::as_factor(format(month_year, "%B")),
                country_code = forcats::as_factor(country_code))


gaussian_process(model_ready, 5)
