# BikeCompany Customer behaviour data analysis

Analyzed beke company customer purchase pattern Trends with R

# Technologies Used
R
Tableau 

# Data visualization
![image](https://github.com/tav97/AnalyticsAV/assets/151886105/ee251c66-4598-47ca-90c5-d430fa806421)

 
# DataAnalysis in R
**Libraries Loaded**

library(tidyverse) #calculations

library(lubridate) #dates 

library(hms) #time

library(data.table) #exporting data frame

library(data.table)


**load original .csv files, a years worth of data from August 2020 to July 2021**
aug08_df <- divvy_202008_tripdata <- read.csv("C:/Users/apoor/Downloads/2020 Bikesales/2020 extracted/202008-divvy-tripdata.csv")
Sep09_df <- divvy_202009_tripdata <- read.csv("C:/Users/apoor/Downloads/2020 Bikesales/2020 extracted/202009-divvy-tripdata.csv")
Oct10_df <- divvy_202010_tripdata <- read.csv("C:/Users/apoor/Downloads/2020 Bikesales/2020 extracted/202010-divvy-tripdata.csv")
Nov11_df <- divvy_202011_tripdata <- read.csv("C:/Users/apoor/Downloads/2020 Bikesales/2020 extracted/202011-divvy-tripdata.csv")
Dec12_df <- divvy_202012_tripdata <- read.csv("C:/Users/apoor/Downloads/2020 Bikesales/2020 extracted/202012-divvy-tripdata.csv")

Jan01_df <- divvy_202101_tripdata <- read.csv("C:/Users/apoor/Downloads/2021 Bikesales/2021 sales/202101-divvy-tripdata.csv")
Feb02_df <- divvy_202102_tripdata <- read.csv("C:/Users/apoor/Downloads/2021 Bikesales/2021 sales/202102-divvy-tripdata.csv")
mar02_df <- divvy_202103_tripdata <- read.csv("C:/Users/apoor/Downloads/2021 Bikesales/2021 sales/202103-divvy-tripdata.csv")
Apr04_df <- divvy_202104_tripdata <- read.csv("C:/Users/apoor/Downloads/2021 Bikesales/2021 sales/202104-divvy-tripdata.csv")
May05_df <- divvy_202105_tripdata <- read.csv("C:/Users/apoor/Downloads/2021 Bikesales/2021 sales/202105-divvy-tripdata.csv")
jun06_df <- divvy_202106_tripdata <- read.csv("C:/Users/apoor/Downloads/2021 Bikesales/2021 sales/202106-divvy-tripdata.csv")
jul07_df <- divvy_202107_tripdata <- read.csv("C:/Users/apoor/Downloads/2021 Bikesales/2021 sales/202107-divvy-tripdata.csv")


**merge all of the data frames into one year view**
cyclistic_df <- rbind(aug08_df,Sep09_df,Oct10_df, Nov11_df, Dec12_df, Jan01_df, Feb02_df, mar02_df,  Apr04_df, May05_df, jun06_df,  jul07_df)

remove(aug08_df,Sep09_df,Oct10_df, Nov11_df, Dec12_df, Jan01_df, Feb02_df, mar02_df,  Apr04_df, May05_df, jun06_df,  jul07_df)

**assigning data to a new dataframe to make changes**
cyclistic_date <- cyclistic_df

**calculate ride length by subtracting ended_at time from started_at time and converted it to minutes**
cyclistic_date$ride_length <- difftime(cyclistic_df$ended_at, cyclistic_df$started_at, units = "mins")

**create columns for: day of week, month, day, year, time, hour**
cyclistic_date$date <- as.Date(cyclistic_date$started_at) #default format is yyyy-mm-dd, use start date

cyclistic_date$day_of_week <- wday(cyclistic_df$started_at) #calculate the day of the week 

cyclistic_date$day_of_week <- format(as.Date(cyclistic_date$date), "%A") #create column for day of week
cyclistic_date$month <- format(as.Date(cyclistic_date$date), "%m")#create column for month
cyclistic_date$day <- format(as.Date(cyclistic_date$date), "%d") #create column for day
cyclistic_date$year <- format(as.Date(cyclistic_date$date), "%Y") #create column for year

cyclistic_date$time <- format(as.Date(cyclistic_date$date), "%H:%M:%S") #format time as HH:MM:SS
cyclistic_date$time <- as_hms((cyclistic_df$started_at)) #create new column for time

**Assuming cyclistic_date$time is a character column with values like "18:08:14"**
cyclistic_date$hour <- as.numeric(substr(cyclistic_date$time, 1, 2))


**inspect date column**
head(cyclistic_date$time, n = 10)


**cyclistic_date$time <- as.POSIXct(cyclistic_date$time, format = "%H:%M:%S")**

library(hms)

cyclistic_date$time <- as_hms(cyclistic_date$started_at)

library(hms)

**Assuming cyclistic_date$started_at is a character column with values like "2020-08-20 18:08:14"**
cyclistic_date$started_at <- as.POSIXct(cyclistic_date$started_at, format = "%Y-%m-%d %H:%M:%S")
cyclistic_date$time <- as_hms(format(cyclistic_date$started_at, format = "%H:%M:%S"))


View(cyclistic_date)

cyclistic_date <-cyclistic_date %>% mutate(season = 
                                             case_when(month == "03" ~ "Spring",
                                                       month == "04" ~ "Spring",
                                                       month == "05" ~ "Spring",
                                                       month == "06"  ~ "Summer",
                                                       month == "07"  ~ "Summer",
                                                       month == "08"  ~ "Summer",
                                                       month == "09" ~ "Fall",
                                                       month == "10" ~ "Fall",
                                                       month == "11" ~ "Fall",
                                                       month == "12" ~ "Winter",
                                                       month == "01" ~ "Winter",
                                                       month == "02" ~ "Winter")
)

**create column for different time_of_day: Night, Morning, Afternoon, Evening**
cyclistic_date <-cyclistic_date %>% mutate(time = 
                                             case_when(hour == "0" ~ "Night",
                                                       hour == "1" ~ "Night",
                                                       hour == "2" ~ "Night",
                                                       hour == "3" ~ "Night",
                                                       hour == "4" ~ "Night",
                                                       hour == "5" ~ "Night",
                                                       hour == "6" ~ "Morning",
                                                       hour == "7" ~ "Morning",
                                                       hour == "8" ~ "Morning",
                                                       hour == "9" ~ "Morning",
                                                       hour == "10" ~ "Morning",
                                                       hour == "11" ~ "Morning",
                                                       hour == "12" ~ "Afternoon",
                                                       hour == "13" ~ "Afternoon",
                                                       hour == "14" ~ "Afternoon",
                                                       hour == "15" ~ "Afternoon",
                                                       hour == "16" ~ "Afternoon",
                                                       hour == "17" ~ "Afternoon",
                                                       hour == "18" ~ "Evening",
                                                       hour == "19" ~ "Evening",
                                                       hour == "20" ~ "Evening",
                                                       hour == "21" ~ "Evening",
                                                       hour == "22" ~ "Evening",
                                                       hour == "23" ~ "Evening")
)

#class(cyclistic_date$hour)

#names(cyclistic_date)

**clean the data**

cyclistic_date <- na.omit(cyclistic_date) #remove rows with NA values
cyclistic_date <- distinct(cyclistic_date) #remove duplicate rows 

cyclistic_date <- cyclistic_date[!(cyclistic_date$ride_length <=0),] #remove where ride_length is 0 or negative

cyclistic_date <- cyclistic_date %>%  #remove columns not needed: ride_id, start_station_id, end_station_id, start_lat, start_long, end_lat, end_lng
  select(-c(ride_id, start_station_id, end_station_id,start_lat,start_lng,end_lat,end_lng)) 

**view the final data**
View(cyclistic_date)

**total number of rides**
nrow(cyclistic_date)

cyclistic_date %>%
  group_by(member_casual) %>% 
  count(member_casual)

**Type of bike**

**total rides by member type**
cyclistic_date %>%
  group_by(member_casual, rideable_type) %>% 
  count(rideable_type)

**total rides**
cyclistic_date %>%
  group_by(rideable_type) %>% 
  count(rideable_type)

**total rides by member type**
cyclistic_date %>%
  group_by(member_casual) %>% 
  count(hour) %>% 
  print(n = 48) #lets you view the entire tibble

**total rides**
cyclistic_date %>%
  count(hour) %>% 
  print(n = 24, na.print = "")

**Tim of day**

**morning**
#total rides by member type 
cyclistic_date %>%
  group_by(member_casual) %>% 
  filter(time == "Morning") %>% 
  count(time)


**total rides**
cyclistic_date %>%
  filter(time == "Morning") %>% 
  count(time)

**afternoon**
**total rides by member type**
cyclistic_date %>%
  group_by(member_casual) %>% 
  filter(time == "Afternoon") %>% 
  count(time)

**total rides** 
cyclistic_date %>%
  filter(time == "Afternoon") %>% 
  count(time)


**evening**
#total rides by member type
cyclistic_date %>%
  group_by(member_casual) %>% 
  filter(time == "Evening") %>% 
  count(time)

**total rides**
cyclistic_date %>%
  filter(time == "Evening") %>% 
  count(time)

**night**
#number of rides by member type
cyclistic_date %>%
  group_by(member_casual) %>% 
  filter(time == "Night") %>% 
  count(time)

**number of rides** 
cyclistic_date %>%
  filter(time == "Night") %>% 
  count(time)


**all times of day**
#total rides by member type 
cyclistic_date %>%
  group_by(member_casual) %>% 
  count(time)

**number of rides**
cyclistic_date %>%
  group_by(time) %>% 
  count(time)

**DAY OF THE WEEK**

**total rides by member type**
cyclistic_date %>%
  group_by(member_casual) %>% 
  count(day_of_week)

**total rides** 
cyclistic_date %>%
  count(day_of_week)

**DAY OF THE MONTH**

**total rides by member type**
cyclistic_date %>%
  group_by(member_casual) %>% 
  count(day) %>% 
  print(n = 62) #lets you view the entire tibble

**total rides**
cyclistic_date %>%
  count(day) %>% 
  print(n = 31, na.print = "") #lets you view the entire tibble


cyclistic_date %>%
  group_by(member_casual) %>%
  count(month) %>%
  print(n = 24, na.print = "")

**total rides**
cyclistic_date %>%
  count(month)

cyclistic_date <-cyclistic_date %>% mutate(month = 
                                             case_when(month == "01" ~ "January",
                                                       month == "02" ~ "February",
                                                       month == "03" ~ "March",
                                                       month == "04" ~ "April",
                                                       month == "05" ~ "May",
                                                       month == "06" ~ "June",
                                                       month == "07" ~ "July",
                                                       month == "08" ~ "August",
                                                       month == "09" ~ "September",
                                                       month == "10" ~ "October",
                                                       month == "11" ~ "November",
                                                       month == "12" ~ "December"
                                             ))

**SEASON**
**spring**

**total rides by member type** 
cyclistic_date %>%
  group_by(member_casual) %>% 
  filter(season == "Spring") %>% 
  count(season)

**total rides**
cyclistic_date %>%
  filter(season == "Spring") %>% 
  count(season)

**summer**

**total rides by member type**
cyclistic_date %>%
  group_by(member_casual) %>% 
  filter(season == "Summer") %>% 
  count(season)

**total rides**
cyclistic_date %>%
  filter(season == "Summer") %>% 
  count(season)

**total rides by member type**
cyclistic_date %>%
  group_by(member_casual) %>% 
  filter(season == "Fall") %>% 
  count(season)

**total rides**
cyclistic_date %>%
  filter(season == "Fall") %>% 
  count(season)


**winter**

**total rides by member type**
cyclistic_date %>%
  group_by(member_casual) %>% 
  filter(season == "Winter") %>% 
  count(season)

**total rides** 
cyclistic_date %>%
  filter(season == "Winter") %>% 
  count(season)

**all seasons**
#total rides by member type
cyclistic_date %>%
   group_by(season, member_casual) %>%
   count(season)

**Average ride length**

**average of ride_length**
cyclistic_avgRide <- mean(cyclistic_date$ride_length)
print(cyclistic_avgRide)

**Member type**

**average ride_length**
cyclistic_date %>% group_by( member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

**Type of Bike**

**total rides by member type** 
cyclistic_date %>% group_by(member_casual, rideable_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

**average ride_length**
cyclistic_date %>% group_by(rideable_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))


**Hour**

**average ride_length by member type**
cyclistic_date %>% group_by(hour, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=48) #lets you view entire tibble

**average ride_length**
cyclistic_date %>% group_by(hour) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=24) #lets you view entire tibble

**Time of day**

**morning**

**average ride length by member type**
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(time == "Morning") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))


**average ride length**
cyclistic_date %>% 
  filter(time == "Morning") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))


**afternoon**

**average ride length by member type**
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(time == "Afternoon") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

**average ride length**
cyclistic_date %>% 
  filter(time == "Afternoon") %>%
summarise_at(vars(ride_length),list(time = mean))

**evening**
**average ride length by member type**

cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(time == "Evening") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

**average ride length**
cyclistic_date %>% 
  filter(time == "Evening") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))



**night**

**average ride length by member type** 
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(time == "Night") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

**average ride length**
cyclistic_date %>% 
  filter(time == "Night") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))


**all times of day**

**average ride length by member type**
cyclistic_date %>% 
  group_by(time, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

**average ride length**
cyclistic_date %>% 
  group_by(time) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))


**Day of week**

**average ride_length by member type**
cyclistic_date %>% group_by(member_casual, day_of_week) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

**average ride_length**
cyclistic_date %>% group_by(day_of_week) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))


**Day of the month**

**average ride_length by member type**
cyclistic_date %>% group_by(day, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=62)  #lets you view entire tibble

**average ride_length**
cyclistic_date %>% group_by(day) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=31)  #lets you view entire tibble

**Month**

**average ride_length by member type**
cyclistic_date %>% group_by(month, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=24)  #lets you view entire tibble

**average ride_length**
cyclistic_date %>% group_by(month) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

**Season**

**spring**

#average ride length by member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Spring") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_date %>% 
  filter(season == "Spring") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))



**Summer**

#average ride length by member type for summer 
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Summer") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length for summer 
cyclistic_date %>% 
  filter(season == "Summer") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))


**Fall**

#average ride length by member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Fall") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

**Average ride length**
cyclistic_date %>% 
  filter(season == "Fall") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

**Winter**

**Average ride lngth by member type**
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Winter") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))
**Average Ride Length**
cyclistic_date %>% 
  filter(season == "Winter") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))


**All Seasons**

#average ride length by member type
cyclistic_date %>% 
  group_by(season, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

**Average Ride Length** 
cyclistic_date %>% 
  group_by(season) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

**Created new dataframe in Tableau**
cyclistic_tableau <- cyclistic_date


**Clean teh data**
cyclistic_tableau <- cyclistic_tableau %>%  #remove columns not needed: start_station_name, end_station_name, time, started_at, ended_at
  select(-c(start_station_name, end_station_name, time, started_at, ended_at))

#download the new data as a .csv file
fwrite(cyclistic_tableau,"cyclistic_date.csv")

getwd()


View(cyclistic_tableau)
