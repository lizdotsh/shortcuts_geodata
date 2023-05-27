# Geolocation log parsed into location intervals by state

Takes an input of specifically coded log data from a iOS shortcut that includes a timestamp. Only works with that specific formatting. This is hardcoded in as 'scratch.md' in the function call, but changing argument of main function call will change it. By default, all results are stored in a folder called data/. 

It includes the fully parsed .csv and serialized R objects of the tibble for both the fully parsed log data (for each observation, includes lat/long, parsed timestamp, and which state it is in), as well as a small file showing durations of time spent in each state.

Data not included for obvious privacy reasons. 
