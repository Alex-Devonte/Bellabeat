#Get summary of data
lapply(data_list, function(x) {
  select(x, (!ends_with('id') & !ends_with('date') & !ends_with('time'))) %>% 
    summary
})
#Remove scientific notation
options(scipen=999)


#change name of sleep date column for merging purposes
colnames(data_list$sleep)[5] <- 'activity_date'

#merge activity and sleep data frames
merged_activity_sleep <- merge(data_list$`daily_activity `,
                               data_list$sleep, by=c ('id', 'activity_date'))
glimpse(merged_activity_sleep)

#intensity and time
intensity_time <- data_list$hourly_intensity

#Change the display order for the x axis
intensity_time$activity_time <- factor(intensity_time$activity_time,
                                       c("01:00 AM", "02:00 AM",
                                         "03:00 AM", "04:00 AM",
                                         "05:00 AM", "06:00 AM",
                                         "07:00 AM", "08:00 AM",
                                         "09:00 AM", "10:00 AM",
                                         "11:00 AM", "12:00 PM",
                                         "01:00 PM", "02:00 PM",
                                         "03:00 PM", "04:00 PM",
                                         "05:00 PM", "06:00 PM",
                                         "07:00 PM", "08:00 PM",
                                         "09:00 PM", "10:00 PM",
                                         "11:00 PM", "12:00 AM"))
intensity_time <- intensity_time%>% 
  group_by(activity_time) %>% 
  summarise(intensity = sum(total_intensity))

ggplot(intensity_time, aes(x=activity_time, y=intensity, fill='#FE8F77')) +
  geom_col(show.legend = FALSE) +
  labs(title = 'Activity by the hour', x = 'Hour', y = 'Total intensity') +
  theme(axis.text.x = element_text(angle = 90))


#Calories and steps
steps_calories <- merged_activity_sleep %>% 
  group_by(total_steps) %>% 
  summarise(calories_burned = sum(calories))

ggplot(steps_calories, aes(x=total_steps, y=calories_burned)) +
  geom_smooth() +
  geom_point(stat = 'identity') +
  labs(title = 'Steps and calories correlation',
       x = 'Total steps', y = 'Calories')


#Daily steps
daily_steps <- data_list$daily_steps
daily_steps$weekday <- wday(mdy(daily_steps$activity_date), label = TRUE)
daily_steps$weekday <- factor(daily_steps$weekday, c("Mon", "Tue", "Wed", "Thu","Fri","Sat", "Sun"))

daily_steps <- daily_steps%>% 
  group_by(weekday) %>% 
  summarise(total = mean(step_total))

ggplot(daily_steps, aes(x=weekday, y=total,  fill='#FE8F77')) +
  geom_col(show.legend = FALSE) + geom_hline(yintercept = 8000) +
  labs(title = "Average steps per day", x= 'Weekday', y = 'Steps')