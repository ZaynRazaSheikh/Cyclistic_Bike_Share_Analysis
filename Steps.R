Step by Step Procedure

library(tidyverse)  # This library is used to wrangle data efficiently.
library(conflicted)  # Conflicted package is utilized to manage conflicts between function names.

# Set dplyr::filter and dplyr::lag as the default choices to handle potential conflicts.
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

#=====================
# STEP 1: COLLECT DATA
#=====================
# Reading and storing Divvy datasets (csv files) for the first quarters of 2019 and 2020.
q1_2019 <- read_csv("Divvy_Trips_2019_Q1.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Comparing column names of each dataset and ensuring they match perfectly before combining them.
colnames(q1_2019)
colnames(q1_2020)

# Renaming columns of q1_2019 to match the structure of q1_2020 for consistency.
(q1_2019 <- rename(q1_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name
                   ,end_station_id = to_station_id
                   ,member_casual = usertype
))

# Inspecting the structure of the dataframes for any inconsistencies.
str(q1_2019)
str(q1_2020)

# Converting ride_id and rideable_type columns to character to stack them correctly.
q1_2019 <-  mutate(q1_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 

# Combining individual quarter dataframes into a single dataframe.
all_trips <- bind_rows(q1_2019, q1_2020)

# Removing unnecessary columns such as latitude, longitude, birth year, and gender for consistency.
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender,  "tripduration"))

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Reviewing the structure and summary of the combined dataframe.
colnames(all_trips)  #List of column names
nrow(all_trips)  #Number of rows
dim(all_trips)  #Dimensions
head(all_trips)  #First 6 rows
str(all_trips)  #Structure
summary(all_trips)  #Statistical summary

# Replacing different labels for member and casual riders with the current 2020 labels.
table(all_trips$member_casual)
all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

# Adding columns for date, month, day, year, and day of the week for further analysis.
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Adding a calculated field for ride length in seconds.
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)

# Converting ride_length from factor to numeric for calculations.
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))

# Removing "bad" data such as rides with negative trip duration or bikes taken out for quality control.
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length < 0),]

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Calculating descriptive statistics for ride length.
mean(all_trips_v2$ride_length)  # Average ride length
median(all_trips_v2$ride_length)  # Median ride length
max(all_trips_v2$ride_length)  # Longest ride length
min(all_trips_v2$ride_length)  # Shortest ride length

# Using summary() to display summary statistics for ride length.
summary(all_trips_v2$ride_length)

# Comparing ride lengths between member and casual users.
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# Analyzing average ride time by each day for members vs casual users.
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Fixing the order of days of the week.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Re-running the analysis for average ride time by each day for members vs casual users.
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Analyzing ridership data by user type and weekday.
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  
  group_by(member_casual, weekday) %>%  
  summarise(number_of_rides = n()							
            ,average_duration = mean(ride_length)) %>% 		
  arrange(member_casual, weekday)								

# Visualizing the number of rides by rider type.
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Creating a visualization for average duration.
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Exporting summary data to a CSV file for further analysis in Excel, Tableau, or other software.
# Note: Adjust the file location according to your system (e.g., "F:\\R_p1\\summary_data.csv").
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, "F:\\R_p1\\summary_data.csv", row.names = FALSE)

