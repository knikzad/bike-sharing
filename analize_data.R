# # # # # # # # # # # # # # # # # # # # # # # 
# Install required packages
# tidyverse for data import and wrangling
# lubridate for date functions
# ggplot for visualization
# readr for reading data
# dplyr for stack multiple data frames into one big data frame
# # # # # # # # # # # # # # # # # # # # # # #  


# tidyverse for data import and wrangling
install.packages("tidyverse")

# lubridate for date functions
install.packages("lubridate")

# ggplot for visualization
install.packages("ggplot2")

#helps read data
install.packages("readr")

# dplyr for Stack individual quarter's data frames into one big data frame
install.packages("dplyr")



library(tidyverse)  
library(lubridate)  
library(ggplot2)  
library(dplyr)
library(readr)

#displays your working directory
getwd() 

#sets your working directory to simplify calls to data
work_dir <- "/cloud/project/Google_Data_Analytics/Member_vs_Casual_Riders/CSV"
setwd(work_dir)

#recheck your working directory
getwd() 

#=====================
# STEP 1: COLLECT DATA
#=====================
# Upload Divvy datasets (csv files) here
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")
q4_2019 <- read_csv("Divvy_Trips_2019_Q4.csv")
q3_2019 <- read_csv("Divvy_Trips_2019_Q3.csv")
q2_2019 <- read_csv("Divvy_Trips_2019_Q2.csv")
                    

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files
# While the names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file
colnames(q1_2020)
colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)

#remove extra columns from q1
q1_2020 <- q1_2020 %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))

#remove extra columns from q4
q4_2019 <- q4_2019 %>%  
  select(-c(birthyear, gender,"tripduration"))


#remove extra columns from q2
q3_2019 <- q3_2019 %>%  
  select(-c(birthyear, gender,"tripduration"))

#remove extra columns from q3
q2_2019 <- q2_2019 %>%  
  select(-c("01 - Rental Details Duration In Seconds Uncapped", "Member Gender","05 - Member Details Member Birthday Year"))


# Rename columns  to make them consistent with q1_2020 (as this will be the supposed going-forward table design for Divvy)
(q4_2019 <- rename(q4_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q3_2019 <- rename(q3_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q2_2019 <- rename(q2_2019
                   ,ride_id = "01 - Rental Details Rental ID"
                   ,rideable_type = "01 - Rental Details Bike ID" 
                   ,started_at = "01 - Rental Details Local Start Time"  
                   ,ended_at = "01 - Rental Details Local End Time"  
                   ,start_station_name = "03 - Rental Start Station Name" 
                   ,start_station_id = "03 - Rental Start Station ID"
                   ,end_station_name = "02 - Rental End Station Name" 
                   ,end_station_id = "02 - Rental End Station ID"
                   ,member_casual = "User Type"))


# Inspect the dataframes and look for incongruencies
str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)

# Convert ride_id and rideable_type to character so that they can stack correctly
q4_2019 <-  mutate(q4_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 

q3_2019 <-  mutate(q3_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 

q2_2019 <-  mutate(q2_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 

#Merge quarterly data frames into one big data frame
all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)


#remove quarterly data frames to clear up space in the environment there is memory allocation limitation in the RStudio free version
remove(q1_2020, q2_2019, q4_2019, q3_2019)




#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics

# There are a few problems we will need to fix:
# (1) In the "member_casual" column, there are two names for members ("member" and "Subscriber") and two names for casual riders ("Customer" and "casual"). We will need to consolidate that from four to two labels.
# (2) The data can only be aggregated at the ride-level, which is too granular. We will want to add some additional columns of data -- such as day, month, year -- that provide additional opportunities to aggregate the data.
# (3) We will want to add a calculated field for length of ride since the 2020Q1 data did not have the "tripduration" column. We will add "ride_length" to the entire dataframe for consistency.
# (4) There are some rides where tripduration shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to delete these rides.

# In the "member_casual" column, replace "Subscriber" with "member" and "Customer" with "casual"
# Before 2020, Divvy used different labels for these two types of riders ... we will want to make our dataframe consistent with their current nomenclature
# N.B.: "Level" is a special property of a column that is retained even if a subset does not contain any values from a specific level
# Begin by seeing how many observations fall under each usertype

table(all_trips$member_casual)

# Reassign to the desired values (we will go with the current 2020 labels)
all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

# Check to make sure the proper number of observations were reassigned
table(all_trips$member_casual)

# create columns date, month, day, and year of each ride and then derived them from started_at attribute.
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")


# Convert started_at and ended_at to datetime in order to later calculate the ride length
all_trips <-  mutate(all_trips, started_at = mdy_hm(started_at)
                   , ended_at = mdy_hm(ended_at)) 

# Create the column ride_length and calculate it by subtracting ended_at from started_at and then convert it to minutes
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at, units='mins')


# Inspect the structure of the columns
str(all_trips)

# clean the data

# Check number of rows before cleaning data
nrow(all_trips)  

#remove rows with NA values
all_trips <- na.omit(all_trips) 

#remove duplicate rows 
all_trips <- distinct(all_trips) 

#remove where ride_length is 0 or negative
all_trips <- all_trips[!(all_trips$ride_length <=0),] 

# Check number of rows after cleaning data
nrow(all_trips)  


# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips$ride_length) #straight average (total ride length / rides)
median(all_trips$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips$ride_length) #longest ride
min(all_trips$ride_length) #shortest ride


# You can condense the four lines above to one line using summary() on the specific attribute
summary(all_trips$ride_length)

#Begin by seeing how many observations fall under each usertype
table(all_trips$member_casual)

# Compare members and casual users
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = mean)
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = median)
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = max)
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips$ride_length ~ all_trips$member_casual + all_trips$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips$day_of_week <- ordered(all_trips$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips$ride_length ~ all_trips$member_casual + all_trips$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)	


# number of rides per month by user type
all_trips %>% 
  mutate(month = month.abb[as.numeric(month)]) %>% 
  mutate(month = factor(month, levels = month.abb)) %>% 
  group_by(member_casual, month) %>% 
  summarise(Number_of_Ride = n()) %>% 
  print(n = 24)


# sorts

# Let's visualize the number of rides by rider type
all_trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")


# Let's create a visualization for average duration
all_trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

# create a visualization for number of rides per month
all_trips %>% 
  mutate(month = month.abb[as.numeric(month)]) %>% 
  mutate(month = factor(month, levels = month.abb)) %>% 
  group_by(member_casual, month) %>% 
  summarise(Number_of_Ride = n()) %>% 
  ggplot(aes(x = month, y = Number_of_Ride, fill = member_casual)) +
  geom_col(position = "dodge")
#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
counts <- aggregate(all_trips$ride_length ~ all_trips$member_casual + all_trips$day_of_week, FUN = mean)
write.csv(counts, file = '/cloud/project/Google_Data_Analytics/Member_vs_Casual_Riders/CSV/avg_ride_length.csv')

#I'm done! Congratulations!

