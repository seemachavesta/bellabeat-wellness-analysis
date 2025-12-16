install.packages("tidyverse")
install.packages("lubridate")
install.packages("janitor")

library(tidyverse)
library(lubridate)
library(janitor)

daily_activity <- read_csv("raw_data/dailyActivity_merged.csv")
View(daily_activity)
glimpse(daily_activity)

sleep_day <- read_csv("raw_data/sleepDay_merged.csv")

glimpse(sleep_day)
View(daily_sleep)

head(daily_activity)
head(sleep_day)

# Cleaning the Columns name
daily_activity <- clean_names(daily_activity)
sleep_day <- clean_names(sleep_day)

colnames(daily_activity)
colnames(sleep_day)

# Fixing the date columns
daily_activity <- daily_activity %>%
  mutate(activity_date = mdy(activity_date))

sleep_day <- sleep_day %>%
  mutate(sleep_date = mdy_hms(sleep_day))

View(sleep_day)
View(daily_activity)

summary(daily_activity$total_steps)
summary(sleep_day$total_minutes_asleep)

# Analysis Udersting the users and Time span
n_distinct(daily_activity$id)
# daily_activity result is 33 and sleep_day result is 24
n_distinct(sleep_day$id)

# Date range of the data
# Result daily activity "2016-04-12" "2016-05-12" and sleep_day result 2016-04-12 UTC" "2016-05-12 UTC
range(daily_activity$activity_date)
range(sleep_day$sleep_date)


# Analysing Daily activity trends
daily_summary <- daily_activity %>% 
  mutate(weekday = wday(activity_date, label=TRUE)) %>%
  mutate(
    total_active_mintues = 
      lightly_active_minutes + fairly_active_minutes + very_active_distance
  )

glimpse(daily_summary)
View(daily_summary)

# Average steps by day of week
steps_by_weekday <- daily_summary %>%
  group_by(weekday) %>%
  summarise(avg_steps = mean(total_steps, na.rm = TRUE))

glimpse(steps_by_weekday)

# What are trends in smart device usage?

ggplot(steps_by_weekday, aes(x = weekday, y= avg_steps)) + 
  geom_col(fill= "steelblue") + 
  labs(
    title = "Average Steps by Day of Week",
    x = "Day of Week",
    y = "Average steps"
  )


# Analyze -- Sedentary vs active behavior

daily_summary %>%
  summarise(
    avg_sedentary = mean(sedentary_minutes),
    avg_active = mean(total_active_mintues)
  )

# Analyze -- Sleep + activity relationship
sleep_clean <- sleep_day %>%
  mutate(sleep_date = as.Date(sleep_date)) %>%   # ensure it's a Date
  select(id, sleep_date, total_minutes_asleep, total_time_in_bed)




# Merging activity and sleep


sleep_activity <- daily_summary %>%
  inner_join(
    sleep_clean,
    by = c("id" = "id", "activity_date" = "sleep_date")
  )

glimpse(sleep_activity)



sleep_activity <- sleep_activity %>%
  mutate(sleep_efficiency = total_minutes_asleep / total_time_in_bed)

summary(sleep_activity$sleep_efficiency)

sleep_activity <- sleep_activity %>%
  mutate(
    activity_level = case_when(
      total_steps < 5000 ~ "Low Activity",
      total_steps >= 5000 & total_steps < 10000 ~ "Moderate Activity",
      total_steps >= 10000 ~ "High Activity"
    )
  )


# Visualization 1 — Distribution of sleep efficiency
ggplot(sleep_activity, aes(x = sleep_efficiency)) + 
  geom_histogram(bins = 30, fill="skyblue", color="black") +
  labs(
    title = "Distribution of Sleep Efficiency",
    x = "Sleep Efficiency (Minutes Asleep / Time in Bed)",
    y = "Number of Days"
    
  )
# KEY FINDING 2 — Activity and sleep are related
# Visualization 2 — Steps vs sleep efficiency
ggplot(
  sleep_activity,
  aes(
    x = total_steps,
    y = sleep_efficiency,
    color = activity_level
  )
) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Daily Steps vs Sleep Efficiency",
    x = "Total Daily Steps",
    y = "Sleep Efficiency",
    color = "Activity Level"
  )



# KEY FINDING 3 - Sedentary behavior dominates
sleep_activity %>%
  summarise(
    avg_steps = mean(total_steps),
    avg_sedentary = mean(sedentary_minutes),
    avg_active = mean(total_active_mintues)
  )

# Visualization 3 — Sedentary vs active minutes
sleep_activity %>%
  summarise(
    sedentary = mean(sedentary_minutes),
    active = mean(total_active_mintues)
  ) %>%
  pivot_longer(everything(), names_to = "type", values_to = "minutes") %>%
  ggplot(aes(x = type, y = minutes)) +
  geom_col(fill = "orchid") +
  labs(
    title = "Average Sedentary vs Active Minutes per Day",
    x = "",
    y = "Minutes"
  )











