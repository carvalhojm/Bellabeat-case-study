library(tidyverse)
library(lubridate)
library(scales)
library(ggpubr)

# Importing datasets

#datasets 03 month
daily_activity_03 <- read_csv("dailyActivity_merged_03.csv")
hourly_calories_03 <- read_csv("hourlyCalories_merged_03.csv")
hourly_steps_03 <- read_csv("hourlySteps_merged_03.csv")
weight_log_info_03 <- read_csv("weightLogInfo_merged_03.csv")
#datasets 04 month
daily_activity_04 <- read_csv("dailyActivity_merged_04.csv")
hourly_calories_04 <- read_csv("hourlyCalories_merged_04.csv")
hourly_steps_04 <- read_csv("hourlySteps_merged_04.csv")
sleep_day_04 <- read_csv("sleepDay_merged_04.csv")
weight_log_info_04 <- read_csv("weightLogInfo_merged_04.csv")

# Checking column names
names(daily_activity_03) == names(daily_activity_04)   
names(hourly_calories_03) == names(hourly_calories_04)
names(hourly_steps_03) == names(hourly_steps_04)
names(weight_log_info_03) == names(weight_log_info_04)

# Checking different IDs 
#03
n_distinct(daily_activity_03$Id)
n_distinct(hourly_calories_03$Id)
n_distinct(hourly_steps_03$Id)
n_distinct(weight_log_info_03$Id)
#04
n_distinct(daily_activity_04$Id)
n_distinct(hourly_calories_04$Id)
n_distinct(hourly_steps_04$Id)
n_distinct(weight_log_info_04$Id)
n_distinct(sleep_day_04$Id)

# Merging months datasets
daily_activity <- bind_rows(daily_activity_03, daily_activity_04)
hourly_calories <- bind_rows(hourly_calories_03, hourly_calories_04)
hourly_steps <- bind_rows(hourly_steps_03, hourly_steps_04)
weight <- bind_rows(weight_log_info_03, weight_log_info_04)

# Checking whether any IDs were added after the merge.
n_distinct(daily_activity$Id)
n_distinct(hourly_calories$Id)
n_distinct(hourly_steps$Id)
n_distinct(weight$Id)

# Previewing our datasets
colnames(daily_activity)
colnames(hourly_calories)
colnames(hourly_steps)
colnames(weight)
colnames(sleep_day_04)

str(daily_activity)
str(hourly_calories)
str(hourly_steps)
str(weight)
str(sleep_day_04)

head(daily_activity)
head(hourly_calories)
head(hourly_steps)
head(weight)
head(sleep_day_04)


## Data cleaning

# Looking for duplicates
sum(duplicated(daily_activity))
sum(duplicated(hourly_calories))
sum(duplicated(hourly_steps))
sum(duplicated(weight))
sum(duplicated(sleep_day_04))

# Checking results reported as duplicates
hourly_calories %>% 
  filter(duplicated(.))
duplicated_hourly_calories <- hourly_calories[duplicated(hourly_calories) | duplicated(hourly_calories, fromLast = TRUE), ]
head(duplicated_hourly_calories)

hourly_steps %>% 
  filter(duplicated(.))
duplicated_hourly_steps <- hourly_steps[duplicated(hourly_steps) | duplicated(hourly_steps, fromLast = TRUE), ]
head(duplicated_hourly_steps)

sleep_day_04 %>% 
  filter(duplicated(.))
sleep_day_04[duplicated(sleep_day_04) | duplicated(sleep_day_04, fromLast = TRUE), ]

# Removing duplicates
hourly_calories <- hourly_calories %>% distinct() 

hourly_steps <- hourly_steps %>% distinct() 

weight <- weight %>% distinct()

sleep_day_04 <- sleep_day_04 %>% distinct()

# Confirming that duplicates have been deleted
sum(duplicated(hourly_calories))
sum(duplicated(hourly_steps))
sum(duplicated(weight))
sum(duplicated(sleep_day_04))


# Formatting date and time columns
daily_activity <- daily_activity %>% 
  rename(date = ActivityDate) %>% 
  mutate(date = as_date(date, format = "%m/%d/%Y"))

weight <- weight %>% 
  rename(date = Date) %>% 
  mutate(date = as_date(date, format = "%m/%d/%Y %I:%M:%S %p"))

hourly_calories <- hourly_calories %>% 
  rename(date = ActivityHour) %>% 
  mutate(date = as.POSIXct(date, format = "%m/%d/%Y %I:%M:%S %p"))

hourly_steps <- hourly_steps %>% 
  rename(date = ActivityHour) %>% 
  mutate(date = as.POSIXct(date, format = "%m/%d/%Y %I:%M:%S %p"))

sleep_day_04 <- sleep_day_04 %>% 
  rename(date = SleepDay) %>% 
  mutate(date = as_date(date, format = "%m/%d/%Y %I:%M:%S %p"))

daily_activity_04 <- daily_activity_04 %>% 
  rename(date = ActivityDate) %>% 
  mutate(date = as_date(date, format = "%m/%d/%Y"))

# Checking date format
class(daily_activity$date)
class(hourly_calories$date)
class(hourly_steps$date)
class(weight$date)
class(sleep_day_04$date)

# Checking if any minute asleep is greater than the time spent in bed
sleep_day_04 %>% filter(TotalTimeInBed < TotalMinutesAsleep)


## Analyse
daily_activity %>%  
  select(TotalSteps, TotalDistance, SedentaryMinutes, Calories) %>%
  summary()

hourly_calories %>%  
  select(Calories) %>%
  summary()

hourly_steps %>%  
  select(StepTotal) %>%
  summary()

daily_activity %>%  
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes) %>%
  summary()

weight %>%  
  select(WeightKg, Fat, BMI, IsManualReport) %>%
  summary()

sleep_day_04 %>%  
  select(TotalSleepRecords, TotalMinutesAsleep,  TotalTimeInBed) %>%
  summary()

# Sorting active users by device usage
daily_usage <- daily_activity %>%
  filter(TotalSteps >100 ) %>% 
  group_by(Id) %>%
  summarize(ActivityDate=sum(n())) %>%
  mutate(usage_type = case_when(
    ActivityDate >= 1 & ActivityDate <= 20 ~ "Low Use",
    ActivityDate >= 21 & ActivityDate <= 40 ~ "Moderate Use", 
    ActivityDate >= 41 & ActivityDate <= 60 ~ "High Use")) %>% 
  mutate(usage_type = factor(usage_type, level = c('Low Use','Moderate Use','High Use'))) %>% 
  rename(days_used = ActivityDate) %>% 
  group_by(usage_type)

head(daily_usage)

daily_usage_percent <- daily_usage %>%
  count(usage_type) %>% 
  ungroup() %>%    
  mutate(
    #creating new columns for user type percentages
    total_percent = n / sum(n), 
    # transforming decimal into percentage
    labels = scales::percent(total_percent)
  )

head(daily_usage_percent)

# making the pie chart
ggplot(daily_usage_percent, aes(x = "", y = total_percent, fill = usage_type)) +
  geom_col(width = 11, color = "white") + 
  coord_polar(theta = "y") + 
  labs(title = "User Usage Type Percent", fill = "User usage type", x = NULL, y = NULL) + 
  theme_void() + 
  geom_text(aes(label = labels), 
            position = position_stack(vjust = 0.5), 
            size = 5, color = "black") + 
  scale_fill_manual(values = c("High Use" = "#391fea", "Moderate Use" = "#467df9", "Low Use" = "#d43b2d"))

# Number of steps
#making the chart
ggplot(data = daily_activity) +
  geom_histogram(mapping = aes(x = TotalSteps), binwidth = 4000, color = "#467df9", fill = "#467df9", alpha = 0.6) + 
  geom_vline(aes(xintercept = 4500, color = "sedentary", linetype = "sedentary"), linewidth = 0.8) +
  geom_vline(aes(xintercept = 10000, color = "recommended", linetype = "recommended"), linewidth = 0.8) +
  labs(x = "daily steps", y = "count", title = "Distribution of number of steps") +
  scale_color_manual(name = "Daily Recommendation (WHO)", 
                     values = c("sedentary" = "red",
                                "recommended" = "black")) +
  scale_linetype_manual(name = "Daily Recommendation (WHO)", 
                        values = c("sedentary" = "twodash",
                                   "recommended" = "dashed")) +
  theme_minimal() 

# Number of steps by user usage type
#merging activity and usage tables
daily_activity_usage <- daily_activity %>%
  left_join(daily_usage, by = "Id")

# Making the same previous segmented histogram
ggplot(data = daily_activity_usage) +
  geom_histogram(mapping = aes(x = TotalSteps), binwidth = 4000, color = "#467df9", fill = "#467df9", alpha = 0.6) +
  geom_vline(aes(xintercept = 4500, color = "sedentary", linetype = "sedentary"), linewidth = 0.8 ) +
  geom_vline(aes(xintercept = 10000, color = "recommended", linetype = "recommended"), linewidth = 0.8) +
  labs(x = "daily steps", y = "count", title = "Distribution of Daily Steps by User Usage Type") +
  scale_color_manual(name = "Daily Recommendation (WHO)",
                     values = c("sedentary" = "#d43b2d",
                                "recommended" = "black")) +
  scale_linetype_manual(name = "Daily Recommendation (WHO)",
                        values = c("sedentary" = "twodash",
                                   "recommended" = "dashed")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  theme(legend.position = "top") +
  facet_wrap(~ usage_type) 

#making a boxplot for deeper compreesion.
daily_activity_usage %>%
  ggplot(aes(x= , y= TotalSteps, fill= usage_type)) +
  geom_boxplot() +
  scale_y_continuous(limits=c(0,40000), breaks=seq(0,40000, by = 5000))+
  labs(title= ("Average Steps by User Usage Type"), x=" " , y="Total Steps")+
  theme(plot.title = element_text(size = 16),
        axis.text.y = element_text(size=10), 
        axis.text.x = element_blank(),axis.ticks.x=element_blank(),
        legend.title = element_text(size=12),
        legend.text = element_text(size=8))+
  facet_grid(~usage_type) +
  theme_minimal() +
  scale_fill_manual(name = "Usage Type",
                    values = c("High Use" = "#391fea", "Moderate Use" = "#467df9", "Low Use" = "#d43b2d")) 

# Grouping by number of steps
#finding out the average of all types of user activity, add them up and find out the total average.
active_average <- daily_activity %>%
  group_by(Id) %>%
  summarise (mean_daily_steps = mean(TotalSteps), mean_daily_calories = mean(Calories))

head(active_average)

# Segmenting users by total step count
user_active_type <- active_average %>%
  mutate(user_type = case_when(
    mean_daily_steps < 4500 ~ "sedentary",
    mean_daily_steps >= 4500 & mean_daily_steps < 7500 ~ "lightly active", 
    mean_daily_steps >= 7500 & mean_daily_steps < 10000 ~ "fairly active", 
    mean_daily_steps >= 10000 ~ "very active"
  ))

head(user_active_type)

#converting into percentages
user_active_type_percent <- user_active_type %>%
  count(user_type) %>%
  mutate(total_percent = n / sum(n),
         labels = scales::percent(total_percent)) %>%
  mutate(user_type = factor(user_type,
                            levels = c("very active", "fairly active", "lightly active", "sedentary")))

head(user_active_type_percent)

#making a pie chart
ggplot(user_active_type_percent, aes(x = "", y = total_percent, fill = user_type)) +
  geom_col(width = 11, color = "white") +  
  coord_polar(theta = "y") + 
  labs(title = "User Active Type Percentage", fill = "User Active Type", x = NULL, y = NULL) + 
  theme_void() + 
  geom_text(aes(label = labels), 
            position = position_stack(vjust = 0.5), 
            size = 5, color = "black") + 
  scale_fill_manual(values = c("very active" = "#391fea",
                               "fairly active" = "#467df9",
                               "lightly active" = "#ab70ff",
                               "sedentary" = "#d43b2d")) 

# Segmenting grouped number of steps by user usage type
#merging tables
daily_active_type_usage <- user_active_type %>%
  left_join(daily_usage, by = "Id")

#converting quantity to porcentage
daily_active_use_piechart <- daily_active_type_usage %>%
  group_by(usage_type, user_type) %>%
  summarise(n_users = n(), .groups = "drop_last") %>% 
  mutate(
    total_in_facet = sum(n_users), 
    total_percent = n_users / total_in_facet, 
    labels = scales::percent(total_percent)
  ) %>%
  ungroup()

head(daily_active_use_piechart)

#making a pie chart
ggplot(daily_active_use_piechart, aes(x = "", y = total_percent, fill = user_type)) +
  geom_col(width = 1, color = NA) + 
  coord_polar(theta = "y") + 
  labs(title = "User Active Type Distribution by Usage Group", fill = "Activity Level", x = NULL, y = NULL) + 
  theme_void() + 
  geom_text(aes(label = labels), 
            position = position_stack(vjust = 0.5), 
            size = 4, color = "black") +
  scale_fill_manual(values = c("very active" = "#391fea", "fairly active" = "#467df9", 
                               "lightly active" = "#ab70ff", "sedentary" = "#d43b2d")) +
  facet_wrap(~ usage_type) + 
  theme(plot.title = element_text(size = 14, hjust = 0.5, vjust = 4),
        legend.position = "bottom", 
        strip.text = element_text(size = 10, face = "bold")) 

# Total steps vs calories
#making the chart
ggplot(data = daily_activity) + aes(x = TotalSteps, y = Calories) +
  geom_point(color = "#391fea", alpha = 0.6) +
  geom_smooth(method = 'loess', formula = 'y ~ x', color = 'darkblue') +
  labs(title = "Total Steps vs Calories", y = 'calories', x = 'total steps')

# Relationship between steps and calories by time of day
#merging tables
hourly_calories_steps <- hourly_calories %>%
  left_join(hourly_steps, by = c("Id", "date"))

# Converting steps and calories values into averages by hour of the day
hourly_calories_steps_grouped <- hourly_calories_steps %>%
  mutate( hour_of_day = hour(date)) %>%
  group_by(hour_of_day) %>% 
  drop_na() %>% 
  summarize(
    average_steps = mean(StepTotal, na.rm = TRUE),
    average_calories = mean(Calories, na.rm = TRUE),
    .groups = "drop" 
  ) %>%
  arrange(hour_of_day)

head(hourly_calories_steps_grouped)

# making a bar chart
ggarrange(
  ggplot(data = hourly_calories_steps_grouped) +
    geom_col(aes(x = hour_of_day, y = average_steps, fill = average_steps)) + 
    labs(title = "Hourly Steps Throughout the Day", x="hour of the day", y="average steps") + 
    scale_fill_gradient(low = "#d43b2d", high = "#391fea")+
    scale_x_continuous(breaks = 0:23, 
                       labels = as.character(0:23)) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          legend.position = "bottom"), 
  ggplot(data = hourly_calories_steps_grouped) +
    geom_col(aes(x = hour_of_day, y = average_calories, fill = average_calories)) + 
    labs(title = "Hourly Calories Throughout the Day", x="hour of the day", y="average calories") + 
    scale_fill_gradient(low = "#d43b2d", high = "#391fea")+
    scale_x_continuous(breaks = 0:23, 
                       labels = as.character(0:23)) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          legend.position = "bottom") 
)

# Segmenting steps and calories by time of day and by user usage type
# Let's repeat the conversion of steps and calorie values into averages by hour of day, but also using Id
hourly_averages_by_id_and_hour <- hourly_calories_steps %>%
  mutate(hour_of_day = hour(date)) %>% 
  group_by(Id, hour_of_day) %>% 
  drop_na() %>% 
  summarize(
    average_steps = mean(StepTotal, na.rm = TRUE),
    average_calories = mean(Calories, na.rm = TRUE),
    .groups = "drop" 
  ) %>%
  arrange(Id, hour_of_day)

head(hourly_averages_by_id_and_hour)

# Merging with usage table.
hourly_averages_usage <- hourly_averages_by_id_and_hour %>%
  left_join(daily_usage, by = "Id")


# total steps and calories of users grouped by the hours of the day
hourly_usage_grouped <- hourly_averages_usage %>% 
  group_by(usage_type, hour_of_day) %>% 
  summarize(
    grouped_sum_steps = sum(average_steps),
    grouped_sum_calories = sum(average_calories),
    .groups = "drop"
  )

# making a bar chart of the steps by hour
ggplot(hourly_usage_grouped, aes(x = hour_of_day, y = grouped_sum_steps, fill = grouped_sum_steps)) +
  geom_col() +
  labs(title = "Average Steps by Hour of Day x Usage Type",
       x = "hour of day", y = "average steps") +
  scale_fill_gradient(low = "#d43b2d", high = "#391fea") +
  scale_x_continuous(breaks = 0:23, labels = as.character(0:23)) +
  facet_wrap(~ usage_type) + 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5, hjust = 1),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom")

# making a bar chart of the calories burned by hour
ggplot(hourly_usage_grouped, aes(x = hour_of_day, y = grouped_sum_calories, fill = grouped_sum_calories)) +
  geom_col() +
  labs(title = "Average Calories by Hour of Day x Usage Type",
       x = "hour of day", y = "average calories") +
  scale_fill_gradient(low = "#d43b2d", high = "#391fea") +
  scale_x_continuous(breaks = 0:23, labels = as.character(0:23)) +
  facet_wrap(~ usage_type) + 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5, hjust = 1),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom") 

# BMI levels
# finding the average BMI per user
weight_grouped <- weight %>%
  group_by(Id) %>%
  summarise(BMI = mean(BMI))

# categorizing BMI user type
weight_user_type <- weight_grouped %>%
  mutate(
    BMI = case_when(
      BMI < 18.5 ~ "underweight",
      BMI >= 18.5 &
        BMI < 25 ~ "healthy weight",
      BMI >= 25 &
        BMI < 30 ~ "overweight",
      BMI >= 30 ~ "obese"
    ))

head(weight_user_type)

# transforming floatings into percentage 
weight_user_type_percent <- weight_user_type %>%
  count(BMI) %>%
  mutate(total_percent = n / sum(n),
         labels = scales::percent(total_percent)) %>%
  mutate(BMI = factor(BMI, levels = c("underweight", "healthy weight", "overweight", "obese")))

head(weight_user_type_percent)

#making a pie chart
ggplot(weight_user_type_percent, aes(x = "", y = total_percent, fill = BMI)) +
  geom_col(width = 11, color = "white") +
  coord_polar(theta = "y") + 
  labs(title = "User Weight Type Percentage", fill = "User Weight Type", x = NULL, y = NULL) + 
  theme_void() + 
  geom_text(aes(label = labels), 
            position = position_stack(vjust = 0.5), 
            size = 5, color = "black") + 
  scale_fill_manual(values = c("healthy weight" = "#391fea",
                               "underweight" = "#467df9",
                               "overweight" = "#ab70ff",
                               "obese" = "#d43b2d"))

# BMI levels vs usage type
#merging tables
weight_user_type_usage <- weight_user_type %>%
  left_join(daily_usage, by = "Id") 

# transforming values into percentage
weight_user_type_usage_piechart <- weight_user_type_usage %>%
  group_by(usage_type, BMI) %>%
  summarise(n_users = n(), .groups = "drop_last") %>% 
  mutate(
    total_in_facet = sum(n_users), 
    total_percent = n_users / total_in_facet, 
    labels = scales::percent(total_percent)
  ) %>%
  ungroup()

# making a pie chart
ggplot(weight_user_type_usage_piechart, aes(x = "", y = total_percent, fill = BMI)) +
  geom_col(width = 1) + 
  coord_polar(theta = "y") + 
  labs(title = "User Active Type Distribution by Usage Group", fill = "Activity Level", x = NULL, y = NULL) + 
  theme_void() + 
  geom_text(aes(label = labels), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 4) + 
  scale_fill_manual(values = c("healthy weight" = "#391fea", "underweight" = "#467df9",
                               "overweight" = "#ab70ff", "obese" = "#d43b2d")) +
  facet_wrap(~ usage_type) + 
  theme(plot.title = element_text(size = 14, hjust = 0.5, vjust = 4),
        legend.position = "bottom", 
        strip.text = element_text(size = 10, face = "bold")) 

# BMI vs average steps
# merging weight and activity tables
activity_weight_type <- weight_user_type %>%
  left_join(daily_activity, by = "Id")

#finding mean steps per BMI type 
activity_weight_type_plot <- activity_weight_type %>%
  group_by(BMI) %>%
  summarise(
    mean_steps = mean(TotalSteps, na.rm = TRUE),
    n_users = n_distinct(Id)
  ) 

head(activity_weight_type_plot)

# making a bar chart
ggplot(activity_weight_type_plot, aes(x = BMI, y = mean_steps, fill = BMI)) + #BMI x steps
  geom_col(width = 0.6) +
  labs(title = "Mean Steps per BMI user type",
       x = "CMI user type", y = "mean steps") +
  scale_fill_manual(values = c("healthy weight" = "#391fea", "underweight" = "#467df9",
                               "overweight" = "#ab70ff", "obese" = "#d43b2d")) +
  theme_minimal() +
  theme(legend.position = "none")

# Exploring the sleeping time table 
# converting minutes to hours
sleep_hour_04 <- sleep_day_04 %>%
  mutate(
    TotalHoursAsleep = round(sleep_day_04$TotalMinutesAsleep / 60, 1),
    TotalHoursInBed = round(sleep_day_04$TotalTimeInBed / 60, 1)
  )

head(sleep_hour_04)

# making the chart 
ggplot(data = sleep_hour_04) +
  geom_point(mapping = aes(x = TotalHoursAsleep, y= TotalHoursInBed), alpha = 0.6, color = "#391fea") +
  geom_vline(aes(xintercept = 7), linetype = "solid") +
  labs(title = "Distribution of sleep records", x = 'hours asleep', y = 'hours in bed') +
  annotate("text", x = 8.2, y = 15, label = "7 hours of sleep", hjust = 0.4, vjust = 1, size = 4)

#grouping Id's averages
sleep_04_grouped <- sleep_hour_04 %>% 
  group_by(Id) %>%
  summarise(mean_hoursasleep = mean(TotalHoursAsleep), mean_hoursinbed = mean(TotalHoursInBed))

head(sleep_04_grouped)

#merging with usage table
sleep_usage_type_04 <- sleep_04_grouped %>% 
  left_join(daily_usage, by = "Id")

# making the new chart
ggplot(data = sleep_usage_type_04) + aes(x = mean_hoursasleep, y = mean_hoursinbed, , color = usage_type) +
  geom_point(alpha = 0.6, size = 7) +
  geom_vline(aes(xintercept = 7), linetype = "solid") +
  geom_smooth(method = 'loess', formula = 'y ~ x', color = 'darkblue', se = FALSE) +
  labs(title = "Distribution of sleep records", x = 'hours asleep', y = 'hours in bed') +
  annotate("text", x = 8.2, y = 15, label = "7 hours of sleep", hjust = 0.6, vjust = 1, size = 4) +
  theme(legend.position = "bottom")

# Sedentary time vs asleep time
daily_activity_sleep_04 <- daily_activity_04 %>%  
  merge(sleep_hour_04, by=c ("Id", "date"))

#making the chart
ggplot(data = daily_activity_sleep_04, aes(x = SedentaryMinutes, y = TotalMinutesAsleep)) +
  geom_point(color = "#391fea", alpha = 0.6) +
  geom_smooth(method = 'loess', formula = 'y ~ x', color = 'darkblue') +
  labs(title = "Sedentary Minutes vs Total Minutes Asleep", x = 'sedentary minutes', y = 'minutes asleep')

# How well users slept
# grouping data
sleep_user_type_04 <- sleep_04_grouped %>%
  mutate(
    mean_hoursasleep = case_when(
      mean_hoursasleep < 7 ~ "sleep little",
      mean_hoursasleep >= 7 &
        mean_hoursasleep < 9 ~ "sleep ideally",
      mean_hoursasleep >= 9 ~ "sleep a lot"
    ))

head(sleep_user_type_04)

# transforming values into percentage
sleep_user_type_04_percent <- sleep_user_type_04 %>%
  count(mean_hoursasleep) %>%
  mutate(total_percent = n / sum(n),
         labels = scales::percent(total_percent)) %>%
  mutate(mean_hoursasleep = factor(mean_hoursasleep, levels = c("sleep little", "sleep ideally", "sleep a lot")))

head(sleep_user_type_04_percent)

# making the chart
ggplot(sleep_user_type_04_percent, aes(x = "", y = total_percent, fill = mean_hoursasleep)) +
  geom_col(width = 11, color = "white") + 
  coord_polar(theta = "y") + 
  labs(title = "User Sleep Type Percentage", fill = "User Sleep Type", x = NULL, y = NULL) + 
  theme_void() + 
  geom_text(aes(label = labels), 
            position = position_stack(vjust = 0.5), 
            size = 5, color = "black") + 
  scale_fill_manual(values = c("sleep ideally" = "#391fea",
                               "sleep a lot" = "#467df9",
                               "sleep little" = "#d43b2d"))

# BMI levels vs time asleep
#merging tables
sleep_weight_type_04 <- weight_user_type %>%
  left_join(sleep_hour_04, by = "Id", "date") 

#finding mean minutes asleep per BMI type in 04 for chart
sleep_weight_type_04_plot <- sleep_weight_type_04 %>%
  group_by(BMI) %>%
  summarise(
    mean_sleep = mean(TotalHoursAsleep, na.rm = TRUE),
    n_users = n_distinct(Id)
  ) 

head(sleep_weight_type_04_plot)

# making a bar chart
ggplot(sleep_weight_type_04_plot, aes(x = BMI, y = mean_sleep, fill = BMI)) + #BMI x sleep
  geom_col(width = 0.6) +
  labs(title = "Mean Sleep Hours per BMI User Type",
       x = "BMI user type", y = "mean minutes asleep per night") +
  scale_fill_manual(values = c("healthy weight" = "#391fea", "underweight" = "#467df9",
                               "overweight" = "#ab70ff", "obese" = "#d43b2d")) +
  theme_minimal() +
  theme(legend.position = "none")

# Steps and time asleep per day of the week
#transforming date to day of the week
weekday_steps_sleep_04 <- daily_activity_sleep_04 %>%
  mutate(weekday = weekdays(date))

#sorting by day of week
weekday_steps_sleep_04$weekday <- ordered(weekday_steps_sleep_04$weekday, 
                                          levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# finding averages by day of the week
weekday_mean_steps_sleep_04 <- weekday_steps_sleep_04 %>%
  group_by(weekday) %>%
  summarize (daily_steps = mean(TotalSteps), daily_sleep = mean(TotalHoursAsleep))

head(weekday_mean_steps_sleep_04)

#making the chart to summarize data
ggarrange(
  ggplot(data = weekday_mean_steps_sleep_04) +
    geom_col(aes(weekday, daily_steps), fill = "#391fea") +
    geom_hline(yintercept = 7500) +
    labs(title = "Daily steps per weekday", x= "", y = "count of steps") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)), 
  ggplot(weekday_mean_steps_sleep_04, aes(weekday, daily_sleep)) +
    geom_col(fill = "#467df9") +
    geom_hline(yintercept = 7) +
    labs(title = "Hours asleep per weekday", x= "", y = "hours asleep") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
)
