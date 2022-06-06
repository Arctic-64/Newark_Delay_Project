library(tidyverse)
library(here)
library(janitor)
library(lubridate)

flights <- clean_names(read_csv(here("raw_data/flights.csv"), show_col_types = FALSE))
airline_key <- clean_names(read_csv(here("raw_data/airlines.csv"), show_col_types = FALSE))
weather <- clean_names(read_csv(here("raw_data/weather.csv"), show_col_types = FALSE))
planes <- clean_names(read_csv(here("raw_data/planes.csv"), show_col_types = FALSE))
airports <- clean_names(read_csv(here("raw_data/airports.csv"), show_col_types = FALSE))

flights = rename(flights, c("POSIXct_year"="year", "POSIXct_month"="month", "POSIXct_day"="day", "POSIXct_hour"="hour", "POSIXct_minute"="minute"))
airline_key = rename(airline_key, c("airline_name"="name"))
planes = rename(planes, c("manufacture_year"="year"))
airports = rename(airports, c("airport_name"="name"))

compiled <- left_join(flights, airline_key, by = "carrier") %>%
left_join(weather, by = c("time_hour", "origin" = "origin")) %>%
rename(c("POSIXct_time" = "time_hour")) %>%
left_join(planes, by = "tailnum") %>%
left_join(airports, by = c("origin" = "faa")) %>%
  rename(c("origin_airport_name" = "airport_name", "origin_airport_lat"="lat", "origin_airport_long" = "lon", "origin_airport_alt" = "alt", "origin_airport_tz" = "tz", "origin_airport_dst" = "dst", "origin_airport_tzone" = "tzone")) %>%
left_join(airports, by = c("dest" = "faa")) %>%
  rename(c("destination_airport_name" = "airport_name", "destination_airport_lat"="lat", "destination_airport_long" = "lon", "destination_airport_alt" = "alt", "destination_airport_tz" = "tz", "destination_airport_dst" = "dst", "destination_airport_tzone" = "tzone")) %>%
  mutate(dew_bool = ifelse(temp <= dewp, TRUE, FALSE)) %>%
  mutate(dep_delay = ifelse(is.na(dep_delay), dep_time - sched_dep_time, dep_delay)) %>%
  mutate(arr_delay = ifelse(is.na(arr_delay), arr_time - sched_arr_time, arr_delay)) %>%
  mutate(dep_delay_bool = ifelse(dep_delay > 0, TRUE, FALSE)) %>%
  mutate(arr_delay_bool = ifelse(arr_delay > 0, TRUE, FALSE)) %>%
  mutate(cancelled = ifelse(is.na(arr_delay_bool | dep_delay_bool), TRUE, FALSE)) %>%
  mutate(POSIXct_local_time = as.POSIXct(paste(paste(POSIXct_year, POSIXct_month, POSIXct_day, sep = "-"), 
                                               paste(POSIXct_hour, POSIXct_minute, sep = ":"), sep = " ")), 
                                                tz="EDT",format="%Y-%m-%d %H:%M") %>%
select("flight",
       "tailnum",
       "airline_name",
       "POSIXct_local_time",
       
       "manufacturer",
       "manufacture_year",
       "model", 
       "type", 
       "seats",
       "engine",
       "engines",
       "speed",
       
       "origin_airport_name",
       "origin_airport_lat",
       "origin_airport_long",
       "origin_airport_alt",
       "origin_airport_tzone",
       
       "temp",
       "dewp",
       "dew_bool",
       "humid",
       "precip",
       "wind_dir",
       "wind_speed",
       "wind_gust",
       "pressure",
       "visib",
       
       "destination_airport_name",
       "destination_airport_lat",
       "destination_airport_long",
       "destination_airport_alt",
       "destination_airport_tzone",
       
       "sched_dep_time",
       "dep_time",
       "dep_delay",
       
       "sched_arr_time",
       "arr_time",
       "arr_delay",
       
       "dep_delay_bool",
       "arr_delay_bool",
       
       "cancelled",
       
       "air_time",
       "distance",
       "POSIXct_hour"
) %>%
  arrange(POSIXct_local_time)

write.csv(compiled, here("clean_data\\primary_clean_data.csv"), row.names = FALSE)

print(colnames(compiled))

rm(flights, airline_key, weather, planes, airports, compiled)
