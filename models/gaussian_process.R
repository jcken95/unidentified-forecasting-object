library(lubridate)
gaussian_process <- function(
    input_data,
    prediction_length
) {
  
  lengthscale <- 0.3
  transformed_lengthscale <- lengthscale * max(input_data$t_numeric)
  
  fitted_model <- mgcv::gam(
    sightings_month ~ s(t_numeric, by = country_code, bs = "gp", m = c(-2, transformed_lengthscale, 2)) + country_code,
    data = input_data,
    family = "nb"
  )
  new_data <- input_data |>
    dplyr::distinct(country_code, month) |>
    tidyr::expand_grid(
      t_numeric = max(input_data$t_numeric) + seq_len(prediction_length)
    ) |>
    dplyr::mutate(
      month_year = max(input_data$month_year) %m+% months(t_numeric - min(t_numeric) + 1),
      sightings_month = NA_integer_
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
      type = dplyr::if_else(month_year > max(input_data$month_year), "forecast", "fit")
    )
  
  samples
  
}
