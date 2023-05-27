library(tidyverse)
library(sf)
library(ggmap)
library(spData)


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
covert_coords_to_stateid(state_test) %>% time_segments %>%  view


covert_coords_to_stateid <- function(coords_df, states = spData::us_states) {
    
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
        filter(state != lag(state)) %>% 
        mutate(
            end_unixtime = lead(unixtime), 
            duration = end_unixtime - unixtime
        ) %>% 
        return()
}
