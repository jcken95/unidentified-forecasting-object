library(lubridate)
cubic_regression <- function(
    input_data,
    prediction_length,
    training_length = 240
) {
  
  input_data <- input_data |>
    dplyr::filter(month_year >= max(month_year) - months(training_length))
  
  lengthscale <- 0.3
  transformed_lengthscale <- lengthscale * max(input_data$t_numeric)
  
  fitted_model <- mgcv::gam(
    sightings_month ~ s(t_numeric, by = country_code, bs = "cr", k = 3) +
      s(country_code, bs = "re") +
      s(month, bs = "re"),
    data = input_data,
    family = "nb"
  )
  new_data <- tibble::tibble(
    t_numeric = max(input_data$t_numeric) + seq_len(prediction_length)
  ) |>
    dplyr::mutate(
      month_year = max(input_data$month_year) %m+% months(t_numeric - min(t_numeric) + 1),
      month = forcats::as_factor(format(month_year, "%B")),
      sightings_month = NA_integer_
    ) |>
    tidyr::expand_grid(
      country_code = unique(input_data$country_code)
    )
  
  prediction_data <- input_data |>
    dplyr::bind_rows(new_data) |>
    dplyr::mutate(.row = dplyr::row_number())
  
  samples <- gratia::posterior_samples(
    fitted_model,
    data = prediction_data,
    method = "mh",
    n = 500
  ) |>
    dplyr::left_join(prediction_data, by = ".row") |>
    dplyr::mutate(
      type = dplyr::if_else(month_year > max(input_data$month_year), "forecast", "fit"),
      name = "cubic_regression"
    )
  
  samples
  
}
