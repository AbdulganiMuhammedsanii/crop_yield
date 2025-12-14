install.packages(c("httr2", "jsonlite", "dplyr", "purrr", "tibble", "lubridate", "tidyr"))

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

#Predict Yield Anomaly

getwd()
library(readr)
ls()
list.files("../../Users/abdi/Downloads") # see what’s one level up

wasde <- read_csv("../../Users/abdi/Downloads/wasde_info.csv")
wasde

corn_wasde <- wasde %>%
  filter(
    Commodity == "Corn",
    Region == "United States",
    Attribute %in% c(
      "Yield",
      "Production",
      "Ending Stocks"
    )
  )
corn_wasde[c(100, 200, 300, 400, 500, 600),]



#FINAL MODEL APPROACH all the corn counties

library(httr2)
library(dplyr)
library(purrr)
library(readr)
install.packages("stringr")   # only once, if not installed
library(stringr)

api_key <- "337A3109-127F-3A0F-B90B-E2AA50E94338"

get_corn_production_year <- function(year) {
  req <- request("https://quickstats.nass.usda.gov/api/api_GET/") |>
    req_url_query(
      key = api_key,
      commodity_desc = "CORN",
      statisticcat_desc = "PRODUCTION",
      agg_level_desc = "COUNTY",
      year = year,
      format = "JSON"
    )
  
  resp <- req_perform(req)
  json <- resp_body_json(resp)
  
  if (length(json$data) == 0) return(NULL)
  
  json$data |>
    map_dfr(as_tibble) |>
    mutate(
      year = as.integer(year),
      county = str_to_title(county_name),
      state  = str_to_title(state_name),
      production = parse_number(Value)
    ) |>
    select(state, county, year, production)
}

years <- 2000:2023

corn_prod <- map_dfr(
  years,
  get_corn_production_year,
  .progress = TRUE
)

nrow(corn_prod)

corn_prod |> count(year)

corn_prod

library(dplyr)

corn_prod_clean <- corn_prod |>
  filter(!grepl("Other \\(Combined\\)", county)) |>
  filter(!is.na(production), production > 0)


corn_prod_clean |> count(state) |> arrange(desc(n))

county_prod_totals <- corn_prod_clean |>
  group_by(state, county) |>
  summarise(
    total_production = sum(production, na.rm = TRUE),
    mean_production  = mean(production, na.rm = TRUE),
    years_present    = n(),
    .groups = "drop"
  )

county_ranked <- county_prod_totals |>
  arrange(desc(total_production)) |>
  mutate(
    cumulative_share = cumsum(total_production) / sum(total_production),
    rank = row_number()
  )

county_ranked |> slice_head(n = 10)

core_counties_90 <- county_ranked |>
  filter(cumulative_share <= 0.90)

nrow(core_counties_90)

core_counties_90 |>
  count(state, sort = TRUE)

max(core_counties_90$cumulative_share)

core_county_list <- core_counties_90 |>
  select(state, county)

core_county_list

#smaller subset

model_counties <- county_ranked |>
  filter(
    mean_production >= quantile(mean_production, 0.70)
  )

nrow(model_counties)

model_counties <- county_ranked |>
  slice_head(n = 400)
api_key <- "337A3109-127F-3A0F-B90B-E2AA50E94338"

get_corn_yield_year <- function(year) {
  req <- request("https://quickstats.nass.usda.gov/api/api_GET/") |>
    req_url_query(
      key = api_key,
      commodity_desc = "CORN",
      statisticcat_desc = "YIELD",
      agg_level_desc = "COUNTY",
      year = year,
      format = "JSON"
    )
  
  resp <- req_perform(req)
  json <- resp_body_json(resp)
  
  if (length(json$data) == 0) return(NULL)
  
  json$data |>
    map_dfr(as_tibble) |>
    mutate(
      year   = as.integer(year),
      county = str_to_title(county_name),
      state  = str_to_title(state_name),
      yield  = parse_number(Value)
    ) |>
    filter(
      !is.na(yield),
      prodn_practice_desc == "ALL PRODUCTION PRACTICES",
      util_practice_desc == "GRAIN",
      class_desc == "ALL CLASSES",
      domain_desc == "TOTAL"
    ) |>
    select(state, county, year, yield)
}

years <- 2000:2023

corn_yield_raw <- map_dfr(
  years,
  get_corn_yield_year,
  .progress = TRUE
)

corn_yield_core <- corn_yield_raw |>
  inner_join(model_counties, by = c("state", "county"))
# Should be zero
corn_yield_core |>
  count(state, county, year) |>
  filter(n > 1)

summary(corn_yield_core$yield)
corn_yield_core

write_csv(
  corn_yield_core,
  "county_yield_core.csv"
)

corn_yield_raw <- map_dfr(2000:2023, get_corn_yield_year)
corn_yield_core <- corn_yield_raw |>
  inner_join(model_counties, by = c("state", "county"))

#literally all the counties
corn_yield_core

#these are allt he counties we select
model_counties
write_csv(
  model_counties,
  "model_counties"
)
install.packages("USpopcenters")
library(USpopcenters)
data(county2010)

county_coords <- county2010 |>
  mutate(
    state  = str_to_title(STNAME),
    statefp = str_to_title(STATEFP),
    county = str_to_title(COUNAME),
    latitude  = LATITUDE,
    longitude = LONGITUDE
  ) |>
  select(state, statefp, county, latitude, longitude)

county_coords
write_csv(county_coords, "county_coords_with_statefp.csv")
county_coords_core <- model_counties |>
  mutate(
    state  = str_to_title(state),
    county = str_to_title(county)
  ) |>
  inner_join(county_coords, by = c("state", "county"))

county_coords_core

write_csv(county_coords_core, "county_coords_required.csv")

nrow(county_coords_core)   # should be 400

#now collecting the needed data from the cloud api 
get_era5_daily <- function(state, county, lat, lon,
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
    state  = state,
    county = county,
    date   = as.Date(unlist(json$daily$time)),
    precip = as.numeric(unlist(json$daily$precipitation_sum)),
    rad    = as.numeric(unlist(json$daily$shortwave_radiation_sum))
  )
}

get_era5_land_daily <- function(state, county, lat, lon,
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
    state  = state,
    county = county,
    date   = date,
    temp   = as.numeric(unlist(json$hourly$temperature_2m)),
    dew    = as.numeric(unlist(json$hourly$dew_point_2m)),
    sm_0_7 = as.numeric(unlist(json$hourly$soil_moisture_0_to_7cm)),
    sm_7_28= as.numeric(unlist(json$hourly$soil_moisture_7_to_28cm)),
    st_0_7 = as.numeric(unlist(json$hourly$soil_temperature_0_to_7cm)),
    st_7_28= as.numeric(unlist(json$hourly$soil_temperature_7_to_28cm))
  ) |>
    group_by(state, county, date) |>
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


#run the long process

library(purrr)
library(readr)
library(lubridate)

dir.create("weather_cache", showWarnings = FALSE)

for (i in seq_len(nrow(county_coords_core))) {
  
  row <- county_coords_core[i, ]
  
  fname <- paste0(
    "weather_cache/",
    gsub(" ", "_", row$state), "_",
    gsub(" ", "_", row$county),
    ".csv"
  )
  
  if (file.exists(fname)) {
    message("Skipping (cached): ", row$county)
    next
  }
  
  message("Fetching: ", row$state, " - ", row$county)
  
  Sys.sleep(1)  # DO NOT REMOVE
  
  era5  <- get_era5_daily(
    row$state, row$county,
    row$latitude, row$longitude
  )
  
  Sys.sleep(1)
  
  land <- get_era5_land_daily(
    row$state, row$county,
    row$latitude, row$longitude
  )
  
  weather_daily <- left_join(
    era5, land,
    by = c("state", "county", "date")
  )
  
  write_csv(weather_daily, fname)
}


getwd()

list.files("weather_cache")

length(list.files("weather_cache"))
v <- read_csv("weather_cache/Texas_Castro.csv")
v

#i want to iterate through this table
#widen it to split these variables across season and condense them




summarise_county_weather <- function(df) {
  
  df %>%
    mutate(
      year  = year(date),
      month = month(date)
    ) %>%
    
    # Keep only growing season months
    filter(month %in% 4:9) %>%
    
    # Assign growth stage
    mutate(
      stage = case_when(
        month %in% 4:5 ~ "early",
        month == 6     ~ "veg",
        month == 7     ~ "flower",
        month == 8     ~ "fill",
        month == 9     ~ "late"
      )
    ) %>%
    
    group_by(state, county, year, stage) %>%
    summarise(
      precip_sum = sum(precip, na.rm = TRUE),
      rad_mean   = mean(rad, na.rm = TRUE),
      
      tmean_mean = mean(tmean, na.rm = TRUE),
      tmax_mean  = mean(tmax,  na.rm = TRUE),
      tmin_mean  = mean(tmin,  na.rm = TRUE),
      tdew_mean  = mean(tdew,  na.rm = TRUE),
      
      sm_0_7_mean  = mean(sm_0_7,  na.rm = TRUE),
      sm_7_28_mean = mean(sm_7_28, na.rm = TRUE),
      st_0_7_mean  = mean(st_0_7,  na.rm = TRUE),
      st_7_28_mean = mean(st_7_28, na.rm = TRUE),
      
      .groups = "drop"
    ) %>%
    
    # Wide format: one row per county-year
    pivot_wider(
      names_from  = stage,
      values_from = c(
        precip_sum, rad_mean,
        tmean_mean, tmax_mean, tmin_mean, tdew_mean,
        sm_0_7_mean, sm_7_28_mean,
        st_0_7_mean, st_7_28_mean
      ),
      names_sep = "_"
    )
}


library(readr)
library(purrr)

weather_files <- list.files("weather_cache", full.names = TRUE)

county_year_weather <- map_dfr(
  weather_files,
  ~ {
    df <- read_csv(.x, show_col_types = FALSE)
    summarise_county_weather(df)
  }
)
county_year_weather

dim(county_year_weather)
summary(select(county_year_weather, starts_with("precip")))
summary(select(county_year_weather, starts_with("tmean")))

colSums(is.na(county_year_weather))

write_csv(
  county_year_weather,
  "county_year_weather_stage_aggregates.csv"
)
n_distinct(county_year_weather$state,county_year_weather$county)

county_year_weather %>%
  group_by(state, county) %>%
  summarise(
    min_year = min(year),
    max_year = max(year),
    n_years  = n(),
    .groups = "drop"
  ) %>%
  summary()

county_year_weather %>%
  count(state, county, year) %>%
  filter(n > 1)

county_year_weather

#joining 
county_year_weather_with_yield <- county_year_weather %>%
  left_join(
    corn_yield_core %>% select(state, county, year, yield),
    by = c("state", "county", "year")
  )
county_year_weather_with_yield
sum(is.na(county_year_weather_with_yield$yield))


model_df <- county_year_weather_with_yield %>%
  filter(!is.na(yield))

model_df


#LOADING COUNTY NDVI AND EVI

library(tidyverse)

ndvi_dirs <- c(
  "/Users/abdi/Downloads/NDVI_CoreCounties",
  "/Users/abdi/Downloads/NDVI_CoreCounties_2"
)

evi_dirs <- c(
  "/Users/abdi/Downloads/EVI_CoreCounties",
  "/Users/abdi/Downloads/EVI_CoreCounties_2",
  "/Users/abdi/Downloads/EVI_CoreCounties2002",
  "/Users/abdi/Downloads/EVI_CoreCounties2003",
  "/Users/abdi/Downloads/EVI_CoreCounties2004"
)

read_veg_folder <- function(dirs, value_name) {
  dirs |>
    map(list.files,
        pattern = "\\.csv$",
        full.names = TRUE
    ) |>
    unlist() |>
    map_dfr(
      ~ read_csv(.x, show_col_types = FALSE) |>
        transmute(
          state  = str_to_title(STATEFP),   # if present; otherwise drop
          county = str_trim(str_replace(NAME, " County$", "")),
          year   = as.integer(year),
          stage  = stage,
          !!value_name := as.numeric(mean)
        ),
      .progress = TRUE
    )
}

ndvi_all <- read_veg_folder(ndvi_dirs, "ndvi")
evi_all  <- read_veg_folder(evi_dirs,  "evi")

count(ndvi_all, stage)
count(ndvi_all, year)

count(evi_all, stage)
count(evi_all, year)

veg_all <- ndvi_all |>
  left_join(
    evi_all,
    by = c("county", "year", "stage")
  )
veg_all

#annoying but necessary back to state name 

state_lookup <- county2010 |>
  distinct(
    statefp = STATEFP,
    state   = STNAME
  ) |>
  mutate(
    statefp = as.character(statefp),
    state   = str_to_title(state)
  )


veg_all_named <- veg_all |>
  left_join(
    state_lookup,
    by = c("state.x" = "statefp")
  ) |>
  rename(state_x_name = state) |>
  left_join(
    state_lookup,
    by = c("state.y" = "statefp")
  ) |>
  rename(state_y_name = state)

veg_all_named <- veg_all_named |>
  mutate(state = state_x_name) |>
  select(-state.x, -state.y, -state_x_name, -state_y_name)

veg_all_named
#











veg_all_clean <- veg_all_named |>
  group_by(state, county, year, stage) |>
  summarise(
    ndvi = mean(ndvi, na.rm = TRUE),
    evi  = mean(evi,  na.rm = TRUE),
    .groups = "drop"
  )

veg_all_clean

veg_wide <- veg_all_clean |>
  pivot_wider(
    id_cols = c(state, county, year),
    names_from = stage,
    values_from = c(ndvi, evi)
  )
veg_wide

model_df_with_veg <- model_df |>
  left_join(
    veg_wide,
    by = c("state", "county", "year")
  )

model_df_with_veg

#event though we join on left for the ones with yield that are missing, there will be some null ndvi and evi , likely because of the years included in the left table not included in right
colSums(is.na(model_df_with_veg[, grep("^ndvi_|^evi_", names(model_df_with_veg))]))



model_df_complete <- model_df_with_veg |>
  drop_na(starts_with("ndvi_"), starts_with("evi_"))

model_df_complete

nrow(model_df_with_veg)
nrow(model_df_complete)

#Write cleaned data into directory with project

write_csv(model_df_complete, "cleaned_dataset")

#because names are replicated across state
model_df_complete <- model_df_complete |>
  mutate(
    county_id = interaction(state, county, drop = TRUE)
  )

model_df_complete$county_id

#simplest linear regression model
lm_baseline <- lm(
  yield ~ precip_sum_veg + tmean_mean_veg + ndvi_veg + evi_veg,
  data = model_df_complete
)

summary(lm_baseline)

#trying to use solely weather to predict outcomes? not strong because heavily county and year

#fixed county effects ! with extremely improve r^2 because expected outcome is heavily county dependent

lm_fe <- lm(
  yield ~
    precip_sum_early + precip_sum_flower + precip_sum_fill +
    tmean_mean_early + tmean_mean_flower + tmean_mean_fill +
    ndvi_early + ndvi_flower + ndvi_fill +
    evi_early + evi_flower + evi_fill +
    factor(county_id),
  data = model_df_complete
)

#much stronger when factoring county production levels

summary(lm_fe)

#factors for year, to control for technology improvements

lm_fe2 <- lm(
  yield ~
    precip_sum_early + precip_sum_flower + precip_sum_fill +
    tmean_mean_early + tmean_mean_flower + tmean_mean_fill +
    ndvi_early + ndvi_flower + ndvi_fill +
    evi_early + evi_flower + evi_fill +
    factor(county_id) + factor(year),
  data = model_df_complete
)

summary(lm_fe2)

#strongest model showing potential


# Want to optimize for early season prediction power so not able to use late season values not available yet!
#how early can I be less wrong than the market

lm_fe3 <- lm(yield ~
               ndvi_veg +
               tmax_mean_veg +
               ndvi_veg:tmax_mean_veg +
               precip_sum_veg +
               factor(county) + factor(year),
             data = model_df_complete
)

summary(lm_fe3)

#using solely early signal ADJ R^2 is 0.65 and Residual standard error is 17.6

#county demeaned yield

df_dm <- model_df_complete |>
  group_by(county) |>
  mutate(yield_demeaned = yield - mean(yield)) |>
  ungroup()

lm_dm <- lm(
  yield_demeaned ~
    ndvi_veg +
    tmax_mean_veg +
    ndvi_veg:tmax_mean_veg +
    precip_sum_veg +
    factor(year),
  data = df_dm
)

summary(lm_dm)

#results are 0.51 -> we are able to explain 51% of YoY yield deviations

#Now I will show as I go through time, early (APRIL/MAY), and flower(JULY)

lm_early <- lm(
  yield_demeaned ~
    ndvi_early +
    tmax_mean_early +
    precip_sum_early +
    factor(year),
  data = df_dm
)
summary(lm_early)

lm_flower <- lm(
  yield_demeaned ~
    ndvi_flower +
    tmax_mean_flower +
    precip_sum_flower +
    factor(year),
  data = df_dm
)
summary(lm_flower)


# CREATE SEPARATE DATASETS

df_early <- model_df_complete |>
  select(
    state, county, year, yield,
    ndvi_early, evi_early,
    precip_sum_early,
    tmean_mean_early, tmax_mean_early
  )

df_veg <- model_df_complete |>
  select(
    state, county, year, yield,
    # early
    ndvi_early, evi_early,
    precip_sum_early,
    tmean_mean_early, tmax_mean_early,
    # veg
    ndvi_veg, evi_veg,
    precip_sum_veg,
    tmean_mean_veg, tmax_mean_veg
  )

df_flower <- model_df_complete |>
  select(
    state, county, year, yield,
    # early
    ndvi_early, evi_early,
    precip_sum_early,
    tmean_mean_early, tmax_mean_early,
    # veg
    ndvi_veg, evi_veg,
    precip_sum_veg,
    tmean_mean_veg, tmax_mean_veg,
    # flower
    ndvi_flower, evi_flower,
    precip_sum_flower,
    tmean_mean_flower, tmax_mean_flower
  )

df_fill <- model_df_complete |>
  select(
    state, county, year, yield,
    # early
    ndvi_early, evi_early,
    precip_sum_early,
    tmean_mean_early, tmax_mean_early,
    # veg
    ndvi_veg, evi_veg,
    precip_sum_veg,
    tmean_mean_veg, tmax_mean_veg,
    # flower
    ndvi_flower, evi_flower,
    precip_sum_flower,
    tmean_mean_flower, tmax_mean_flower,
    # fill
    ndvi_fill, evi_fill,
    precip_sum_fill,
    tmean_mean_fill, tmax_mean_fill
  )

model_df_complete

county_baseline <- model_df_complete |>
  group_by(state, county) |>
  summarise(
    mean_yield = mean(yield, na.rm = TRUE),
    .groups = "drop"
  )

county_baseline

county_pred <- test |>
  left_join(county_baseline, by = c("state", "county")) |>
  mutate(
    yield_pred = mean_yield + pred
  )
county_pred

model_df_with_baseline <- model_df_complete |>
  left_join(county_baseline, by = c("state", "county"))

model_df_with_baseline

sum(is.na(model_df_with_baseline$mean_yield))          # should be 0
nrow(county_baseline) == n_distinct(model_df_complete$state, model_df_complete$county)

#county baselines -> ex ante
#baseline adj target
model_df_with_baseline

write_csv(model_df_with_baseline, "final_model_df")
