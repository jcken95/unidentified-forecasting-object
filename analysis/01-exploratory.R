# exploratory analysis of ufo data set
set.seed(2122)
ufo_sightings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-06-20/ufo_sightings.csv')
ufo_sightings |>
  utils::head()

total_sights_per_country <- ufo_sightings |>
  dplyr::summarise(
    n = dplyr::n(),
    .by = country_code
  )

total_sights_per_country |>
  dplyr::arrange(-n)

low_sight_countries <- total_sights_per_country |>
  # we're going to exclude those with 100 or fewer reports
  # just because it's going to make modelling easier
  # in practice, build better models :)
  dplyr::filter(n < 200) |>
  dplyr::pull(country_code)

ufo_monthly <- ufo_sightings |>
  dplyr::filter(!(country_code %in% low_sight_countries)) |> 
  dplyr::mutate(date = reported_date_time) |> 
  tidyr::drop_na(date, country_code) |>
  dplyr::mutate(
    month_year = lubridate::round_date(date, unit = "month")
  ) |>
  dplyr::summarise(
    sightings_month = dplyr::n(),
    .by = c(month_year, country_code)
  ) 
 
date_country <- tibble::tibble(
  month_year = seq(
    from = min(ufo_monthly$month_year),
    to = max(ufo_monthly$month_year),
    by = "month"
  )
) |>
  tidyr::expand_grid(
    country_code = unique(ufo_monthly$country_code)
  )

ufo_complete <- ufo_monthly |>
  dplyr::right_join(date_country, by = dplyr::join_by(month_year, country_code)) |>
  dplyr::mutate(sightings_month = dplyr::coalesce(sightings_month, 0))

# select a subset for simplified plotting
selected_countries <- unique(ufo_complete$country_code)
ufo_complete |> 
  dplyr::filter(country_code %in% selected_countries) |> 
  ggplot2::ggplot(
    ggplot2::aes(x = month_year, y = sightings_month)
  ) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(ggplot2::vars(country_code), scales = "free_y") +
  ggplot2::theme_minimal()
