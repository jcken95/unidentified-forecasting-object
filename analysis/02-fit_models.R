# fit all models to data

source("analysis/01-exploratory.R")

# identidy model files
model_files <- fs::dir_ls(here::here("models"))
# extract the names of the models from these files
model_names <- purrr::map(
  model_files,
  \(.m) {
    .m |>
      stringr::str_remove(here::here("models")) |>
      stringr::str_remove("/") |>
      stringr::str_remove(".R")
  }
)
# read in the models and give the resulting list elements useful names
models <- purrr::map(model_files, \(m) source(m)$value)
names(models) <- model_names

model_ready <- ufo_complete |>
  # basically zero monthly sightings before this date
  dplyr::filter(month_year >= lubridate::ymd("1990-01-01")) |> 
  dplyr::arrange(month_year) |>
  dplyr::mutate(
    t_numeric = dplyr::row_number(),
    .by = country_code
  ) |>
  dplyr::mutate(month = forcats::as_factor(format(month_year, "%B")),
                country_code = forcats::as_factor(country_code))

fit_data <- model_ready |>
  dplyr::filter(month_year < lubridate::ymd("2010-01-01"))

# predict two years ahead (monthly data)
prediction_length <- 24

# fit all models in one go
model_fits <- purrr::map(
  models,
  \(.m) .m(fit_data, prediction_length)
) |>
  dplyr::bind_rows()

# summarise each model
model_summary <- model_fits |>
  dplyr::summarise(
    
    pi_05 = quantile(.response, 0.05),
    pi_25 = quantile(.response, 0.25),
    pi_50 = quantile(.response, 0.50),
    pi_75 = quantile(.response, 0.75),
    pi_95 = quantile(.response, 0.95),
    
    .by = c(month_year, country_code, name, type)
  ) |>
  dplyr::left_join(
    dplyr::select(model_ready, month_year, country_code, sightings_month),
    by = c("month_year", "country_code")
  )

# visualise model fits

model_summary |>
  ggplot2::ggplot() +
  ggplot2::geom_ribbon(
    ggplot2::aes(x = month_year, ymin = pi_05, ymax = pi_95, fill = type),
    alpha = 0.2
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(x = month_year, ymin = pi_25, ymax = pi_75, fill = type),
    alpha = 0.2
  ) +
  ggplot2::geom_line(ggplot2::aes(x = month_year, y = pi_50, colour = type)) +
  ggplot2::geom_point(ggplot2::aes(x = month_year, y = sightings_month), size = 0.1) +
  ggplot2::facet_grid(country_code ~ name, scale = "free_y")

# zoom in on some others
model_summary |> 
  ggplot2::ggplot() +
  ggplot2::geom_ribbon(
    ggplot2::aes(x = month_year, ymin = pi_05, ymax = pi_95, fill = type),
    alpha = 0.2
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(x = month_year, ymin = pi_25, ymax = pi_75, fill = type),
    alpha = 0.2
  ) +
  ggplot2::geom_line(ggplot2::aes(x = month_year, y = pi_50, colour = type)) +
  ggplot2::geom_point(ggplot2::aes(x = month_year, y = sightings_month), size = 0.1) +
  ggplot2::facet_grid(country_code ~ name, scale = "free_y")


# generate ensemble by taking quantiles over all .response values
ensemble_summary <- model_fits |>
  dplyr::summarise(
    
    pi_05 = quantile(.response, 0.05),
    pi_25 = quantile(.response, 0.25),
    pi_50 = quantile(.response, 0.50),
    pi_75 = quantile(.response, 0.75),
    pi_95 = quantile(.response, 0.95),
    
    .by = c(month_year, country_code, type)
  ) |>
  dplyr::left_join(
    dplyr::select(model_ready, month_year, country_code, sightings_month),
    by = c("month_year", "country_code")
  )

ensemble_summary |>
  dplyr::mutate(
    type = dplyr::if_else(type == "forecast", "forecast", "null")
  ) |> 
  ggplot2::ggplot() +
  ggplot2::geom_ribbon(
    ggplot2::aes(x = month_year, ymin = pi_05, ymax = pi_95, fill = type),
    alpha = 0.2
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(x = month_year, ymin = pi_25, ymax = pi_75, fill = type),
    alpha = 0.2
  ) +
  ggplot2::geom_line(ggplot2::aes(x = month_year, y = pi_50, colour = type)) +
  ggplot2::geom_point(ggplot2::aes(x = month_year, y = sightings_month), size = 0.1) +
  ggplot2::geom_point(
    data = fit_data,
    mapping = ggplot2::aes(x = month_year, y = sightings_month), size = 0.1) +
  ggplot2::facet_wrap(~country_code, scale = "free_y", ncol = 1) +
  ggplot2::scale_fill_manual(
    values = c("forecast" = "blue")
  ) +
  ggplot2::scale_colour_manual(
    values = c("forecast" = "blue")
  )

# add up predictions across each country
ensemble_summary_global <- model_fits |>
  dplyr::summarise(
    .response = sum(.response),
    .by = c(month_year, name, .draw)
  ) |> 
  dplyr::summarise(
    
    pi_05 = quantile(.response, 0.05),
    pi_25 = quantile(.response, 0.25),
    pi_50 = quantile(.response, 0.50),
    pi_75 = quantile(.response, 0.75),
    pi_95 = quantile(.response, 0.95),
    
    .by = c(month_year)
  ) |>
  dplyr::left_join(
    model_ready |>
      dplyr::summarise(sightings_month = sum(sightings_month), .by = month_year),
    by = c("month_year")
  )

fit_data_global <- fit_data |>
  dplyr::summarise(sightings_month = sum(sightings_month), .by = month_year)

ensemble_summary_global |>
  dplyr::mutate(
    type = dplyr::if_else(month_year > max(fit_data$month_year),"forecast", "null")
  ) |> 
  ggplot2::ggplot() +
  ggplot2::geom_ribbon(
    ggplot2::aes(x = month_year, ymin = pi_05, ymax = pi_95, fill = type),
    alpha = 0.2
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(x = month_year, ymin = pi_25, ymax = pi_75, fill = type),
    alpha = 0.2
  ) +
  ggplot2::geom_line(ggplot2::aes(x = month_year, y = pi_50, colour = type)) +
  ggplot2::geom_point(ggplot2::aes(x = month_year, y = sightings_month), size = 0.1) +
  ggplot2::facet_wrap(~"Global", scale = "free_y", ncol = 1) +
  ggplot2::scale_fill_manual(
    values = c("forecast" = "blue")
  ) +
  ggplot2::scale_colour_manual(
    values = c("forecast" = "blue")
  )
# around 7 times as many forecast rows per day

