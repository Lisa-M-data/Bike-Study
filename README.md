# Bike-Study R Programming lanuage

install.packages("tidyverse")
library(tidyverse)
library(lubridate)
library(ggplot2)

q2_2019 <- read_csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read_csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("Divvy_Trips_2019_Q4.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

colnames(q3_2019)
colnames(q4_2019)
colnames(q2_2019)
colnames(q1_2020)

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


str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)

q4_2019 <-  mutate(q4_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q3_2019 <-  mutate(q3_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q2_2019 <-  mutate(q2_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 


all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)


all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "01 - Rental Details Duration In Seconds Uncapped", "05 - Member Details Member Birthday Year", "Member Gender", "tripduration"))

colnames(all_trips)  
nrow(all_trips)  
summary(all_trips)

all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

table(all_trips$member_casual)


all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

str(all_trips)


all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)


all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]



mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

summary(all_trips_v2$ride_length)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)



all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

Examining data regarding a bike share company using R Programming language.

Introduction:
Bikeshare companies provide an affordable and convenient transportation option for urban commuters. Analyzing bikeshare data can offer valuable insights into usage patterns, customer behavior, and operational efficiency. In this summary, we'll explore how R programming language can be utilized to analyze and visualize bikeshare data for a hypothetical company.

Data Collection:
The first step in our analysis is to collect bikeshare data from the company's database or API. This dataset typically includes information such as trip duration, start and end stations, user demographics, and timestamps.

Data Preprocessing:
Once the data is collected, it needs to be preprocessed to ensure cleanliness and consistency. This involves tasks such as handling missing values, converting data types, and removing outliers. R provides various packages like dplyr and tidyr for efficient data manipulation.

Exploratory Data Analysis (EDA):
EDA is crucial for understanding the underlying patterns and trends in the bikeshare data. Using R's ggplot2 package, we can create visualizations such as histograms, bar charts, and scatter plots to explore variables like trip duration, user demographics, and popular routes.

Statistical Analysis:
R offers a wide range of statistical packages for conducting advanced analyses on bikeshare data. For example, we can perform hypothesis testing to compare trip durations between different user groups or conduct regression analysis to identify factors influencing ride frequency.

Predictive Modeling:
Predictive modeling can help bikeshare companies forecast future demand and optimize operational decisions. R provides powerful machine learning libraries like caret and ranger for building predictive models such as regression, decision trees, or neural networks.

Dashboard Creation:
Finally, we can use R's Shiny package to create interactive dashboards that allow stakeholders to explore bikeshare data in real-time. These dashboards can include features like filters, maps, and trend visualizations for better decision-making.

In conclusion, my analysis of bikeshare data using the R programming language has provided valuable insights into user behavior and usage patterns. One notable finding is that causal riders tend to ride for longer durations compared to membership holders. To incentivize causal riders to become members, we propose offering a price reduction to members based on ride durations. By researching the average market prices for bikeshare companies in the area and strategically setting a competitive price point, we can offer discounts to encourage membership sign-ups.

This approach not only benefits the bikeshare company by increasing membership and revenue but also provides added value to members by rewarding them for longer rides. By leveraging the analytical capabilities of R, bikeshare companies can make data-driven decisions to optimize pricing strategies and enhance customer satisfaction, ultimately contributing to the success and sustainability of their operations in the urban transportation landscape.
In conclusion, our analysis of bikeshare data using the R programming language has provided valuable insights into user behavior and usage patterns. One notable finding is that causal riders tend to ride for longer durations compared to membership holders. To incentivize causal riders to become members, we propose offering a price reduction to members based on ride durations. By researching the average market prices for bikeshare companies in the area and strategically setting a competitive price point, we can offer discounts to encourage membership sign-ups.

This approach not only benefits the bikeshare company by increasing membership and revenue but also provides added value to members by rewarding them for longer rides. By leveraging the analytical capabilities of R, bikeshare companies can make data-driven decisions to optimize pricing strategies and enhance customer satisfaction, ultimately contributing to the success and sustainability of their operations in the urban transportation landscape.



