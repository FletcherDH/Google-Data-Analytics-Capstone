library(tidyverse) #helps wrangle data
library(lubridate) #helps wrangle the data attributes

total_trips <- bind_rows(m01_2021, m02_2021, m03_2021, m04_2021,
                       m05_2021, m06_2021, m07_2021, m08_2021,
                       m09_2021, m10_2021, m11_2021, m12_2021)

dim(total_trips) #shows the dimensions of the data frame
colnames(total_trips) #shows the column names

total_trips <- total_trips %>% #removes the latitude, longitude, start_station, and end_station as they will not be needed
  select(-c(start_lat, start_lng, end_lat, end_lng, start_station_id, end_station_id))

# Now I will create new columns that list the date, year, month, day, and start hour of each ride.

total_trips$date <- as.Date(total_trips$started_at) 
total_trips$month <- format(as.Date(total_trips$date), "%m")
total_trips$day <- format(as.Date(total_trips$date), "%d")
total_trips$year <- format(as.Date(total_trips$date), "%Y")
total_trips$day_of_week <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
                           "Friday", "Saturday")[as.POSIXlt(total_trips$date)$wday + 1]
total_trips$hour_of_day <- format(as.POSIXct(total_trips$started_at), format = "%H")

# Now I will add a new column called ride_length to calculate each trip in minutes

total_trips$ride_length <- difftime(total_trips$ended_at,total_trips$started_at, units = "mins")

# Now I will sort the data based on ride_length

total_trips %>%
  arrange(ride_length) %>%
  select(ride_id, started_at, ended_at, ride_length) %>%
  filter(ride_length < 0)

# Now we remove all of the negative values from the dataset

total_trips_v2 <- total_trips[!(total_trips$ride_length<0),]
nrow(total_trips_v2)

total_trips_v2 %>% 
  group_by(start_station_name) %>%
  summarise(number_of_rides = n()) %>%
  arrange(-number_of_rides)

total_trips_v2 %>% 
  group_by(end_station_name) %>%
  summarise(number_of_rides = n()) %>%
  arrange(-number_of_rides)

total_trips_v2$start_station_name <-
  replace(total_trips_v2$start_station_name, is.na(total_trips_v2$start_station_name), "Missing")
total_trips_v2$end_station_name <-
  replace(total_trips_v2$end_station_name, is.na(total_trips_v2$end_station_name), "Missing")

# Now its time to create a CSV file for our new dataset

counts <- write.csv(total_trips_v2, file = 'total_trips.csv')
