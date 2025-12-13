install.packages(c("httr2", "jsonlite", "dplyr", "purrr", "tibble", "lubridate"))

library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)
library(lubridate)
library(readr)

counties <- c(
  "Sherman","Cheyenne","Thomas","Wallace","Logan","Gove","Trego","Scott","Wichita","Greeley",
  "Hamilton","Kearny","Finney","Gray","Ford","Hodgeman","Ness","Stanton","Morton","Stevens",
  "Barton","Ellis","Rush","Russell","Rice","Reno","McPherson","Saline","Stafford","Pawnee",
  "Douglas","Franklin","Miami","Johnson","Brown","Atchison","Doniphan"
)

county_coords <- tribble(
  ~county,      ~lat,   ~lon,
  "Sherman",    39.35, -101.72,
  "Cheyenne",   39.78, -101.73,
  "Thomas",     39.36, -101.05,
  "Wallace",    38.91, -101.76,
  "Logan",      38.92, -101.15,
  "Gove",       38.95, -100.48,
  "Trego",      38.91,  -99.87,
  "Scott",      38.48, -100.91,
  "Wichita",    38.48, -101.35,
  "Greeley",    38.48, -101.81,
  "Hamilton",   37.99, -101.79,
  "Kearny",     38.00, -101.32,
  "Finney",     38.05, -100.75,
  "Gray",       37.74, -100.44,
  "Ford",       37.69,  -99.88,
  "Hodgeman",   38.09,  -99.90,
  "Ness",       38.46,  -99.91,
  "Stanton",    37.57, -101.78,
  "Morton",     37.19, -101.80,
  "Stevens",    37.19, -101.31,
  "Barton",     38.48,  -98.76,
  "Ellis",      38.91,  -99.33,
  "Rush",       38.52,  -99.31,
  "Russell",    38.91,  -98.76,
  "Rice",       38.35,  -98.20,
  "Reno",       37.96,  -98.09,
  "McPherson",  38.39,  -97.65,
  "Saline",     38.78,  -97.65,
  "Stafford",   38.03,  -98.72,
  "Pawnee",     38.18,  -99.23,
  "Douglas",    38.97,  -95.31,
  "Franklin",   38.56,  -95.28,
  "Miami",      38.56,  -94.83,
  "Johnson",    38.88,  -94.85,
  "Brown",      39.83,  -95.57,
  "Atchison",   39.56,  -95.31,
  "Doniphan",   39.79,  -95.15
)


#read data from era and era5_land

get_era5_daily <- function(county, lat, lon,
                           start_date = "2001-01-01",
                           end_date   = "2024-12-31") {
  
  req <- request("http://35.172.180.189:8080/v1/archive") |>
    req_url_query(
      latitude   = lat,
      longitude  = lon,
      start_date = start_date,
      end_date   = end_date,
      daily = "precipitation_sum,shortwave_radiation_sum",
      models = "era5"
    )
  
  json <- resp_body_json(req_perform(req))
  
  tibble(
    county = county,
    date   = as.Date(unlist(json$daily$time)),
    precip = as.numeric(unlist(json$daily$precipitation_sum)),
    rad    = as.numeric(unlist(json$daily$shortwave_radiation_sum))
  )
}



get_era5_land_daily <- function(county, lat, lon,
                                start_date = "2001-01-01",
                                end_date   = "2024-12-31") {
  
  req <- request("http://35.172.180.189:8080/v1/archive") |>
    req_url_query(
      latitude   = lat,
      longitude  = lon,
      start_date = start_date,
      end_date   = end_date,
      hourly = paste(
        "temperature_2m",
        "dew_point_2m",
        "soil_moisture_0_to_7cm",
        "soil_moisture_7_to_28cm",
        "soil_temperature_0_to_7cm",
        "soil_temperature_7_to_28cm",
        sep = ","
      ),
      models = "era5_land"
    )
  
  json <- resp_body_json(req_perform(req))
  
  date <- as.Date(sub("T.*", "", unlist(json$hourly$time)))
  
  tibble(
    county = county,
    date   = date,
    
    temp    = as.numeric(unlist(json$hourly$temperature_2m)),
    dew     = as.numeric(unlist(json$hourly$dew_point_2m)),
    sm_0_7  = as.numeric(unlist(json$hourly$soil_moisture_0_to_7cm)),
    sm_7_28 = as.numeric(unlist(json$hourly$soil_moisture_7_to_28cm)),
    st_0_7  = as.numeric(unlist(json$hourly$soil_temperature_0_to_7cm)),
    st_7_28 = as.numeric(unlist(json$hourly$soil_temperature_7_to_28cm))
  ) |>
    group_by(county, date) |>
    summarise(
      tmean = mean(temp, na.rm = TRUE),
      tmax  = max(temp,  na.rm = TRUE),
      tmin  = min(temp,  na.rm = TRUE),
      tdew  = mean(dew,  na.rm = TRUE),
      
      sm_0_7  = mean(sm_0_7,  na.rm = TRUE),
      sm_7_28 = mean(sm_7_28, na.rm = TRUE),
      st_0_7  = mean(st_0_7,  na.rm = TRUE),
      st_7_28 = mean(st_7_28, na.rm = TRUE),
      
      .groups = "drop"
    )
}





test <- get_era5_land_daily("Sherman", 39.35, -101.72,
                            start_date = "2020-01-01",
                            end_date   = "2020-01-03")

str(test)



esat <- function(T) {
  0.6108 * exp((17.27 * T) / (T + 237.3))
}

psychrometric_constant <- function(elev) {
  0.000665 * (101.3 * ((293 - 0.0065 * elev) / 293)^5.26)
}


weather_all_counties <- county_coords |>
  filter(county %in% counties) |>
  pmap_dfr(function(county, lat, lon) {
    
    era5 <- get_era5_daily(county, lat, lon)
    land <- get_era5_land_daily(county, lat, lon)
    
    left_join(era5, land, by = c("county", "date"))
  })

weather_all_counties
weather_all_counties[c(2200),]




library(tidyverse)

veg_files <- list.files(
  path = "/Users/abdi/Downloads/crop_yield_project/EVI_Kansas",
  pattern = "\\.csv$",
  full.names = TRUE
)

evi_all <- veg_files |>
  map_dfr(~ read_csv(.x, show_col_types = FALSE) |>
            transmute(
              county = NAME,
              year   = as.integer(year),
              stage  = stage,
              evi   = as.numeric(mean)
            )
  )
evi_all

library(tidyverse)

ndvi_files <- list.files(
  path = "/Users/abdi/Downloads/crop_yield_project/NDVI_Kansas",
  pattern = "\\.csv$",
  full.names = TRUE
)

ndvi_all <- ndvi_files |>
  map_dfr(~ read_csv(.x, show_col_types = FALSE) |>
            transmute(
              county = NAME,
              year   = as.integer(year),
              stage  = stage,
              ndvi   = as.numeric(mean)
            )
  )

ndvi_all

ndvi_all <- ndvi_all |>
  mutate(county = str_trim(str_replace(county, " County$", "")))

evi_all <- evi_all |>
  mutate(county = str_trim(str_replace(county, " County$", "")))

veg_all <- ndvi_all |>
  left_join(
    evi_all,
    by = c("county", "year", "stage")
  )

veg_all

count(veg_all, stage)
count(veg_all, year)

# check missing joins
veg_all |>
  filter(is.na(ndvi) | is.na(evi)) |>
  count(stage)


anti_join(
  veg_all,
  weather_all_counties |> distinct(county),
  by = "county"
)

weather_all_counties

veg_all


#PIVOTING WIDER TO INCORPORATE SEASON
library(dplyr)
library(lubridate)

weather_staged <- weather_all_counties %>%
  mutate(
    year  = year(date),
    month = month(date),
    stage = case_when(
      month %in% 4:5 ~ "early",
      month == 6     ~ "veg",
      month == 7     ~ "flower",
      month == 8     ~ "fill",
      month == 9     ~ "late",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(stage))


weather_stage_summary <- weather_staged %>%
  group_by(county, year, stage) %>%
  summarise(
    precip_sum   = sum(precip, na.rm = TRUE),
    rad_mean     = mean(rad, na.rm = TRUE),
    tmean_mean   = mean(tmean, na.rm = TRUE),
    tmax_mean    = mean(tmax, na.rm = TRUE),
    tmin_mean    = mean(tmin, na.rm = TRUE),
    tdew_mean    = mean(tdew, na.rm = TRUE),
    sm_0_7_mean  = mean(sm_0_7, na.rm = TRUE),
    sm_7_28_mean = mean(sm_7_28, na.rm = TRUE),
    st_0_7_mean  = mean(st_0_7, na.rm = TRUE),
    st_7_28_mean = mean(st_7_28, na.rm = TRUE),
    .groups = "drop"
  )

library(tidyr)


weather_wide <- weather_stage_summary %>%
  pivot_wider(
    names_from  = stage,
    values_from = -c(county, year, stage),
    names_glue  = "{.value}_{stage}"
  )

weather_wide

veg_wide <- veg_all %>%
  pivot_wider(
    names_from  = stage,
    values_from = c(ndvi, evi),
    names_glue  = "{.value}_{stage}"
  )

veg_wide

final_df <- weather_wide %>%
  left_join(veg_wide, by = c("county", "year"))

final_df

colnames(weather_wide)

#GETTING YIELDS

library(httr2)
library(dplyr)
library(purrr)
library(tibble)
library(readr)
library(stringr)

get_all_yields <- function(county, commodity = "CORN") {
  
  api_key <- "337A3109-127F-3A0F-B90B-E2AA50E94338"
  
  req <- request("https://quickstats.nass.usda.gov/api/api_GET/") |>
    req_url_query(
      key = api_key,
      commodity_desc = commodity,
      state_name = "KANSAS",
      county_name = str_to_upper(county),
      statisticcat_desc = "YIELD",
      format = "JSON"
    )
  
  data <- req_perform(req) |> resp_body_json()
  if (length(data$data) == 0) return(NULL)
  
  data$data |>
    map_dfr(as_tibble) |>
    mutate(
      county = str_to_title(county),
      year   = as.integer(year),
      yield  = readr::parse_number(Value)
    ) |>
    filter(
      !is.na(yield),
      statisticcat_desc == "YIELD",
      prodn_practice_desc == "ALL PRODUCTION PRACTICES",
      util_practice_desc == "GRAIN",
      class_desc == "ALL CLASSES",
      domain_desc == "TOTAL"
    ) |>
    distinct(county, year, yield) |>
    arrange(county, year)
}



yield_df <- map_dfr(counties, get_all_yields)
yield_df

yield_df %>%
  count(county, year) %>%
  filter(n > 1)

yield_df
yield_df <- yield_df %>%
  filter(year >= 2001, year <= 2024)
yield_df

final_df

final_df <- final_df %>%
  mutate(county = str_to_title(county))
final_df


model_df <- final_df %>%
  left_join(yield_df, by = c("county", "year")) %>%
  filter(!is.na(yield))
model_df




#MODELING

features_baseline <- c(
  # Water availability
  "precip_sum_early",
  "precip_sum_fill",
  
  # Heat stress
  "tmax_mean_flower",
  "tmax_mean_fill",
  
  # Crop condition
  "ndvi_flower",
  "ndvi_fill",
  
  # Soil moisture (root zone)
  "sm_7_28_mean_early",
  "sm_7_28_mean_fill"
)

lm_base <- lm(
  yield ~ .,
  data = model_df %>% select(yield, all_of(features_baseline))
)

summary(lm_base)

#county fixed effects

lm_fe <- lm(
  yield ~ . + factor(county),
  data = model_df %>% select(county, yield, all_of(features_baseline))
)

summary(lm_fe)


#elastic net

#Predict absolute county-level yield for known counties
