gaussian_process <- function(
    input_data,
    prediction_length
) {
  
  fitted_model <- mgcv::gam(
    sightings_month ~ s(t_numeric, by = country_code, bs = "gp", m = c(2, 0.2, 2)) +
      # i would prefer random effects, but these are simpler
     country_code +
     month,
    data = input_data,
    family = "nb"
  )

  new_data <- input_data |>
    dplyr::distinct(country_code, month) |>
    tidyr::expand_grid(
      t_numeric = max(input_data$t_numeric) + seq_along(prediction_length)
    ) |>
    dplyr::mutate(sightings_month = NA_integer_)
  
  prediction_data <- input_data |>
    dplyr::bind_rows(new_data)
  
 samples <- gratia::posterior_samples(
   fitted_model,
   new_data = new_data,
   method = "mh"
 )

 samples
  
}
