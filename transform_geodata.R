library(tidyverse)
library(sf)
library(ggmap)
library(spData)

test_convert_and_duration <- function() {
    state_test <- tribble(
        ~lat, ~long, ~unixtime,
        -90, 44, 4000,
        -90, 44, 5000,
        -120, 44, 6000,
        -120, 44.5, 7000,
        -123, 44, 8000,
        -100, 24, 8500,
        -120, 24.5, 9000,
        -123, 24, 9350,
        
        -90, 44, 11000,
        -91,43, 33333,
    )
    converted = covert_coords_to_stateid(state_test) 
    
    
    print(converted)
    
    converted_google = covert_coords_to_stateid_google(state_test)
    
    print(converted_google)
    
    converted %>% 
        time_segments %>% 
        return()
}
#test_convert_and_duration()

convert_coords_to_stateid <- function(coords_df, states = spData::us_states) {
    
    pts <- st_as_sf(coords_df, coords = c('lat', 'long'), crs = 4326) %>% st_transform(crs = 3857)
    states <- st_transform(states, crs = 3857)
    state_names <- states[['NAME']]
    ii <- as.integer(st_intersects(pts, states))
    
    coords_df %>% 
        bind_cols(state = state_names[ii]) %>% 
        mutate(state = coalesce(state, 'Out of Country')) %>% 
        return()
}


covert_coords_to_stateid_google <- function(coords_df) {
    coords_df %>% 
        rowwise() %>% 
        mutate(
            address = revgeocode(c(long, lat))
        ) %>% 
        ungroup() %>% 
        separate(
            address,
            c("street_address", "city","county","state", "country"),
            remove = F, 
            sep = ","
        ) %>% 
        return()
}

time_segments <- function(state_time_df) {
    state_time_df %>% 
        filter(
            state != lag(state) | (timestamp == max(timestamp) | timestamp == min(timestamp))
            ) %>% 
        mutate(
            state = state,
            start_datetime = timestamp,
            end_datetime = lead(timestamp),
            duration = lubridate::interval(start_datetime, end_datetime) %>% 
                lubridate::as.duration(),
            start_lat = lat,
            start_long = long,
            
            .keep = 'none'
        )  %>% 
        na.omit() %>% 
        return()
}

read_location_log_file <- function(filename) {
    data = readLines(filename)
    location_lines <- data[str_detect(data, "Current Location")]
    lat_long <- str_extract(location_lines, "-?\\d+\\.\\d+\\, -?\\d+\\.\\d+") %>% 
        str_split(",", simplify = TRUE)
    tibble(
        timestamp = str_extract(location_lines, "\\[.*\\]") %>% gsub("\\[|\\]|shqiphone.shortcuts|Current Location:", "", .),
        lat = lat_long[,2],
        long = lat_long[,1]
        )%>% 
        mutate(
            timestamp = lubridate::mdy_hms(timestamp, tz = 'America/New_York')
        ) %>%
        arrange(timestamp) %>% 
        return()
}

parse_file <- function(filename) {
    location_history = read_location_log_file(filename) 
    parsed_location_history = convert_coords_to_stateid(location_history, states = spData::us_states)
    write_csv(parsed_location_history, 'data/parsed_location_history.csv')
    saveRDS(parsed_location_history, file = 'data/parsed_location_history_serialized.rds')
    location_intervals = time_segments(parsed_location_history)
    write_csv(location_intervals, 'data/location_intervals.csv')
    saveRDS(location_intervals, file = 'data/location_intervals_serialized.rds')
    return(location_intervals)
    
}

parse_file('data/scratch.md')

