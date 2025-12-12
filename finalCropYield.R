library(httr2)
library(dplyr)
library(purrr)
library(tidyr)
library(tibble)

county_coords <- tribble(
  ~county,      ~lat,      ~lon,
  "THOMAS",     39.3645,  -101.0855,
  "FINNEY",     38.0453,  -100.7480,
  "GRAY",       37.7402,  -100.4395,
  "SCOTT",      38.4824,  -100.9026,
  "FORD",       37.6859,   -99.8800,
  "KEARNY",     38.0030,  -101.3228
)


query_local_openmeteo <- function(lat, lon, start_date, end_date, daily_vars, models = "era5,era5_land") {
  
  url <- "http://127.0.0.1:8080/v1/archive"
  
  req <- request(url) |>
    req_url_query(
      latitude = lat,
      longitude = lon,
      start_date = start_date,
      end_date = end_date,
      daily = paste(daily_vars, collapse = ","),
      models = models
    )
  
  resp <- req_perform(req)
  
  json <- resp_body_json(resp)
  
  df <- as_tibble(json$daily) |>
    mutate(time = as.Date(time))
  
  return(df)
}

daily_vars <- c(
  "temperature_2m_min",
  "temperature_2m_max",
  "temperature_2m_mean",
  "dew_point_2m_mean",
  "precipitation_sum",
  "shortwave_radiation_sum",
  "soil_moisture_0_to_7cm_mean",
  "soil_moisture_7_to_28cm_mean",
  "soil_moisture_28_to_100cm_mean",
  "soil_temperature_0_to_7cm_mean",
  "soil_temperature_7_to_28cm_mean",
  "soil_temperature_28_to_100cm_mean"
)



all_data <- county_coords %>%
  mutate(
    data = pmap(
      list(lat, lon),
      ~ query_local_openmeteo(
        lat = ..1,
        lon = ..2,
        start_date = "2001-01-01",
        end_date = "2024-12-31",
        daily_vars = daily_vars
      )
    )
  ) %>%
  unnest(data)

