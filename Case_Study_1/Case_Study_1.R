#Installing the packages
install.packages('tidyverse')
install.packages('janitor')
install.packages('lubridate')

#Loading the packages
library(tidyverse)
library(janitor)
library(lubridate)

#Adding a name <- Importing the csv(file_location)
Jan2023 <- read_csv("202301.csv")
Feb2023 <- read_csv("202302.csv")
Mar2023 <- read_csv("202303.csv")
Apr2023 <- read_csv("202304.csv")
May2023 <- read_csv("202305.csv")
Jun2023 <- read_csv("202306.csv")
Jul2023 <- read_csv("202307.csv")
Aug2023 <- read_csv("202308.csv")
Sep2023 <- read_csv("202309.csv")
Oct2023 <- read_csv("202310.csv")

#str(dataset_name)
str(Jan2023)
str(Feb2023)
str(Mar2023)
str(Apr2023)
str(May2023)
str(Jun2023)
str(Jul2023)
str(Aug2023)
str(Sep2023)
str(Oct2023)

#Creating new dataset name <- binding rows(all_your_datasets)
merged_df <- bind_rows(Jan2023, Feb2023, Mar2023, Apr2023, May2023, Jun2023, Jul2023, Aug2023, Sep2023, Oct2023)
                       
#Cleaning & removing any spaces, parentheses, etc.
merged_df <- clean_names(merged_df)

#removing_empty(dataset_name, by leaving c() empty, it selects rows & columns)
remove_empty(merged_df, which = c())

#In laymans term, wday() extracts the DAY from the OBJECT(usually SELECTED from a column in a dataframe) with the date format

#df_name$your_new_column_name <- wday(df_name$select_column, label = T/F, abbr = T/F)
merged_df$day_of_week <- wday(merged_df$started_at, label = T, abbr = T)

#In laymans term, POSIXct extracts a certain TIME HOUR FORMAT from an OBJECT (which is usually formatted as a date/time format)

#df_name$your_new_column_name <- format(as.POSIXct(df_name$select_column, '%time_format')
merged_df$starting_hour <- format(as.POSIXct(merged_df$started_at), '%H')


#In laymans term, format() is used to convert one datatype into another. Must be used with datatypes/date formats
#In laymans term, as.Date () extracts the DATE from the OBJECT (usually from a column in a dataframe) with the date format. Used in format()

#df_name$your_new_column_name <- format(as.Date(df_name$select_column), '%date_format')
merged_df$month <- format(as.Date(merged_df$started_at), '%m')


#In laymans term, difftime() calculates the time difference between column and another within a dataframe with a date format.

#df_name$your_new_column_name <- difftime(df_name$usually_end_time_column, df_name$usually_start_time_column, units = 'your_desired_unit')
merged_df$trip_duration <- difftime(merged_df$ended_at, merged_df$started_at, units ='sec')

#In laymans term, '!' means is not equals to
cleaned_df <- merged_df[!(merged_df$trip_duration<=0),]

write.csv(cleaned_df, file ='Cyclistic_df.csv')

#We use options(scipen =) to remove scientific values from our graphs (as the total number of trips will become very big)
#(position = 'dodge') in laymans term, will essentially inform the 2nd value (in this case member type) to 'dodge' and not stack ontop. Instead it will be displayed alongside the x- axis.

options(scipen = 999)
ggplot(data = cleaned_df) +
  aes(x = day_of_week, fill = member_casual) +
  geom_bar(position = 'dodge') +
  labs(x = 'Day of week', y = 'Number of rides', fill = 'Member type', title = 'Number of rides by member type')
ggsave("number_of_rides_by_member_type.png")

ggplot(data = cleaned_df) +
  aes(x = month, fill = member_casual) +
  geom_bar(position = 'dodge') +
  labs(x = 'Month', y = 'Number of rides', fill = 'Member type', title = 'Number of rides per month')
ggsave("number_of_rides_per_month.png")

#We remove 'dodge' as there are already so many charts due to facet_wrap
#Facet_wrap() esentially creates a long riben of panels
#element_text(size=) would allow us to reduce our text size to fit our charts
#dpi allows us to essentially save our chart into higher res, so it be viewed in a larger window more clearly

ggplot(data = cleaned_df) +
  aes(x = starting_hour, fill = member_casual) +
  facet_wrap(~day_of_week) +
  geom_bar() +
  labs(x = 'Starting hour', y = 'Number of rides', fill = 'Member type', title = 'Hourly use of bikes throughout the week') +
  theme(axis.text = element_text(size = 5))
ggsave("Hourly_use_of_bikes_throughout_the_week.png", dpi = 1000)

aggregate(cleaned_df$trip_duration ~ cleaned_df$member_casual + cleaned_df$day_of_week, FUN = mean)

#count will let you count, group & sort the unique values from a dataframe
#filter just filters out data that meets (==) or does not meet(!=) the requirement

count(filter(cleaned_df, member_casual=='member'), start_station_name, sort = T)
count(filter(cleaned_df, member_casual=='member'), end_station_name, sort = T)
            
count(filter(cleaned_df, member_casual=='casual'), start_station_name, sort = T)
count(filter(cleaned_df, member_casual=='casual'), end_station_name, sort = T)
                        
#OR
                        
annual_member_df <- filter(cleaned_df, member_casual=='member')
count(annual_member_df, start_station_name, sort = T)
count(annual_member_df, end_station_name, sort = T)
                        
casual_member_df <- filter(cleaned_df, member_casual=='casual')
count(casual_member_df, start_station_name, sort = T)
count(casual_member_df, end_station_name, sort = T)
                        
#The second method creates a dataframe, which can be much easier to read and understand as well.
