# =========================================================
# Project: Cyclistic Bike Share Analysis
# Author: Muhammad Zain Raza
# Location: Germany
#
# Description:
# This project analyzes bike-sharing usage patterns
# between casual riders and annual members using
# Divvy bike trip data (Q1 2019 and Q1 2020).
#
# Business Question:
# How do casual riders and annual members use
# Cyclistic bikes differently?
#
# Tools:
# R, tidyverse, dplyr, ggplot2, lubridate
# =========================================================


# =====================
# LOAD LIBRARIES
# =====================
library(tidyverse)
library(conflicted)
library(lubridate)

conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")


# =====================
# STEP 1: COLLECT DATA
# =====================
q1_2019 <- read_csv("Divvy_Trips_2019_Q1.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")


# =========================================
# STEP 2: WRANGLE & COMBINE DATA
# =========================================

q1_2019 <- q1_2019 %>%
  rename(
    ride_id = trip_id,
    rideable_type = bikeid,
    started_at = start_time,
    ended_at = end_time,
    start_station_name = from_station_name,
    start_station_id = from_station_id,
    end_station_name = to_station_name,
    end_station_id = to_station_id,
    member_casual = usertype
  ) %>%
  mutate(
    ride_id = as.character(ride_id),
    rideable_type = as.character(rideable_type)
  )

all_trips <- bind_rows(q1_2019, q1_2020) %>%
  select(
    -start_lat, -start_lng,
    -end_lat, -end_lng,
    -birthyear, -gender,
    -tripduration
  )


# =========================================
# STEP 3: CLEAN & PREPARE DATA
# =========================================

all_trips <- all_trips %>%
  mutate(
    member_casual = recode(
      member_casual,
      "Subscriber" = "member",
      "Customer" = "casual"
    ),
    date = as.Date(started_at),
    month = month(date),
    day = day(date),
    year = year(date),
    day_of_week = wday(date, label = TRUE),
    ride_length = as.numeric(difftime(ended_at, started_at))
  )

all_trips_v2 <- all_trips %>%
  filter(
    start_station_name != "HQ QR",
    ride_length > 0
  )


# =========================================
# STEP 4: DESCRIPTIVE ANALYSIS
# =========================================

summary(all_trips_v2$ride_length)

ride_stats <- all_trips_v2 %>%
  group_by(member_casual) %>%
  summarise(
    mean_ride_length = mean(ride_length),
    median_ride_length = median(ride_length),
    max_ride_length = max(ride_length),
    min_ride_length = min(ride_length),
    .groups = "drop"
  )

ride_stats


# =========================================
# STEP 5: WEEKDAY USAGE ANALYSIS
# =========================================

weekday_summary <- all_trips_v2 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(
    number_of_rides = n(),
    average_duration = mean(ride_length),
    .groups = "drop"
  )

weekday_summary


# =========================================
# STEP 6: VISUALIZATION
# =========================================

ggplot(weekday_summary,
       aes(x = day_of_week,
           y = number_of_rides,
           fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(
    title = "Number of Rides by User Type and Weekday",
    x = "Day of Week",
    y = "Number of Rides"
  )

ggplot(weekday_summary,
       aes(x = day_of_week,
           y = average_duration,
           fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(
    title = "Average Ride Duration by User Type and Weekday",
    x = "Day of Week",
    y = "Average Ride Duration (seconds)"
  )


# =========================================
# STEP 7: EXPORT SUMMARY
# =========================================

write_csv(weekday_summary, "summary_data.csv")

# =====================
# END OF SCRIPT
# =====================
