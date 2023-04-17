#Load necessary packages
library(tidyverse)
library(janitor)
library(skimr)
library(dplyr)
library(readr)

#Create data frames from csv files with clean names
daily_activity_df <- clean_names(read.csv('daily_activity_merged.csv'))
daily_calories_df <- clean_names(read.csv('daily_calories_merged.csv'))
hourly_intensity_df <- clean_names(read.csv('hourly_intensities_merged.csv'))
daily_intensity_df <- clean_names(read.csv('daily_intensities_merged.csv'))
daily_steps_df <- clean_names(read.csv('daily_steps_merged.csv'))
heartrate_df <- clean_names(read.csv('heartrate_seconds_merged.csv'))
sleep_df <- clean_names(read.csv('sleep_day_merged.csv'))
weight_df <- clean_names(read.csv('weight_log_info_merged.csv'))


#Create list to hold all data frames
data_list = list('daily_activity '= daily_activity_df,
                 'daily_calories' = daily_calories_df,
                 'hourly_intensity' = hourly_intensity_df,
                 'daily_intensity' = daily_intensity_df,
                 'daily_steps' = daily_steps_df,
                 'heartrate' = heartrate_df,
                 'sleep' = sleep_df,
                 'weight' = weight_df)

#View first 6 rows of all the data sets
lapply(data_list, function(x) {
  head(x)
})

#Get the distinct number of users in each data set
lapply(data_list, function(x) {
  print(n_distinct(x$id))
})

#Remove list elements
data_list <- within(data_list, rm('heartrate'))
data_list <- within(data_list, rm('weight'))
length(data_list)

#Check all rows for data validity
lapply(data_list, function(x) {
  lapply(x, function(y) {
    #Check for empty or NULL values
    if (is.null(y) | is_empty(y)) {
      print("NULL or empty values")
    }
    #Run checks on columns to ensure values are the correct data type
    else if (typeof(y) == "double" | typeof(y) == "integer") {
      if (is.numeric(y)) {
        print("All numerical data types are correct")
      } else {
        print ("Incorrect numerical data types")
      }
    }
    else if (typeof(y) == "character") {
      if (is.character(y)) {
        print("All character data types are correct")
      } else {
        print ("Incorrect character data types")
      }
    }
  })
})

#Check for duplicate values
sapply(data_list, function(x) sum(duplicated(x)))

#Remove duplicate values from sleep data set
data_list$sleep <- data_list$sleep[!duplicated(data_list$sleep), ]

#split date and time into their own respective columns
data_list$hourly_intensity$activity_date <- (format(as.Date(data_list$hourly_intensity$activity_hour, '%m/%d/%Y'), format ="%m/%d/%Y"))
data_list$hourly_intensity$activity_time <- ((format(as.POSIXct(data_list$hourly_intensity$activity_hour, '%m/%d/%Y %I:%M:%S %p', tz =''), format = "%I:%M %p")))
data_list$sleep$sleep_date <- (format(as.Date(data_list$sleep$sleep_day, '%m/%d/%Y'), format ="%m/%d/%Y"))

#Remove old date columns
data_list$hourly_intensity <- subset(data_list$hourly_intensity, select = -activity_hour)
data_list$sleep <- subset(data_list$sleep, select = -sleep_day)

head(data_list$hourly_intensity)
head(data_list$sleep)

#replace columns with suffix 'day' with 'date'
colnames(data_list$daily_calories) <- gsub(colnames(data_list$daily_calories), pattern = '_day', replacement = '_date')
colnames(data_list$daily_intensity) <- gsub(colnames(data_list$daily_intensity), pattern = '_day', replacement = '_date')
colnames(data_list$daily_steps) <- gsub(colnames(data_list$daily_steps), pattern = '_day', replacement = '_date')

#Format the other dates
data_list$`daily_activity `$activity_date <- format(mdy(data_list$`daily_activity `$activity_date), format = '%m/%d/%Y')
data_list$`daily_calories`$activity_date <- format(mdy(data_list$`daily_calories`$activity_date), format = '%m/%d/%Y')
data_list$`daily_intensity`$activity_date <- format(mdy(data_list$`daily_intensity`$activity_date), format = '%m/%d/%Y')
data_list$`daily_steps`$activity_date <- format(mdy(data_list$`daily_steps`$activity_date), format = '%m/%d/%Y')

#Convert the 0 values into NA
data_list$daily_calories[data_list$daily_calories == 0] <- NA
data_list$`daily_activity `$calories[data_list$daily_activity$calories == 0] <- NA

sapply(data_list, summary)
