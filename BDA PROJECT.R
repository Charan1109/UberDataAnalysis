library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(tidyverse) # metapackage of all tidyverse packages
library(DT)
library(scales)


colors = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")
colors

# Read the data for each month separately 
apr <- read.csv("C:/Users/Anandh/Desktop/BDA PROJEECT CSV FILES/uber-raw-data-apr14.csv")
may <- read.csv("C:/Users/Anandh/Desktop/BDA PROJEECT CSV FILES/uber-raw-data-may14.csv")
june <- read.csv("C:/Users/Anandh/Desktop/BDA PROJEECT CSV FILES/uber-raw-data-jun14.csv")
july <- read.csv("C:/Users/Anandh/Desktop/BDA PROJEECT CSV FILES/uber-raw-data-jul14.csv")
aug <- read.csv("C:/Users/Anandh/Desktop/BDA PROJEECT CSV FILES/uber-raw-data-aug14.csv")
sept <- read.csv("C:/Users/Anandh/Desktop/BDA PROJEECT CSV FILES/uber-raw-data-sep14.csv")

# Combine the data together 
data <- rbind(apr, may, june, july, aug, sept)
cat("The dimensions of the data are:", dim(data))


head(data)

#we will format the datetime into a more readable format using the Date Time conversion function.
data$Date.Time <- as.POSIXct(data$Date.Time, format="%m/%d/%Y %H:%M:%S")
data$Time <- format(as.POSIXct(data$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
data$Date.Time <- ymd_hms(data$Date.Time)


# Create individual columns for month day and year
data$day <- factor(day(data$Date.Time))
data$month <- factor(month(data$Date.Time, label=TRUE))
data$year <- factor(year(data$Date.Time))
data$dayofweek <- factor(wday(data$Date.Time, label=TRUE))


# Add Time variables as well 
data$second = factor(second(hms(data$Time)))
data$minute = factor(minute(hms(data$Time)))
data$hour = factor(hour(hms(data$Time)))

head(data)


hourly_data <- data %>% 
  group_by(hour) %>% 
  dplyr::summarize(Total = n())

# Plot the data by hour
ggplot(hourly_data, aes(hour, Total)) + 
  geom_bar(stat="identity", 
           fill="steelblue", 
           color="red") + 
  ggtitle("Trips Every Hour", subtitle = "aggregated today") + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) 


# Aggregate the data by month and hour
month_hour_data <- data %>% group_by(month, hour) %>%  dplyr::summarize(Total = n())

ggplot(month_hour_data, aes(hour, Total, fill=month)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Trips by Hour and Month") 


# Aggregate data by day of the month 
day_data <- data %>% group_by(day) %>% dplyr::summarize(Trips = n())
day_data


# Plot the data for the day
ggplot(day_data, aes(day, Trips)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Trips by day of the month") + 
  theme(legend.position = "none") 


# Collect data by day of the week and month
day_month_data <- data %>% group_by(dayofweek, month) %>% dplyr::summarize(Trips = n())
day_month_data


# Plot the above data
ggplot(day_month_data, aes(dayofweek, Trips, fill = month)) + 
  geom_bar(stat = "identity", aes(fill = month), position = "dodge") + 
  ggtitle("Trips by Day and Month") + 
  scale_fill_manual(values = colors)


#Number of Trips place during months in a year
month_data <- data %>% group_by(month) %>% dplyr::summarize(Total = n())

month_data


ggplot(month_data, aes(month, Total, fill = month)) + 
  geom_bar(stat = "Identity") + 
  ggtitle("Trips in a month") + 
  theme(legend.position = "none")
  scale_fill_manual(values = colors)
  
  
 # Heatmap by Hour and Day
  ggplot(day_hour_data, aes(day, hour, fill = Total)) + 
    geom_tile(color = "white") + 
    ggtitle("Heat Map by Hour and Day")
  
  
  # Collect data by month and day
  month_day_data <- data %>% group_by(month, day) %>% dplyr::summarize(Trips = n())
  month_day_data
  
  
  ggplot(month_day_data, aes(day, month, fill = Trips)) + 
    geom_tile(color = "white") + 
    ggtitle("Heat Map by Month and Day")
  
  
  # Plot a heatmap by day of the week and month
  ggplot(day_month_data, aes(dayofweek, month, fill = Trips)) + 
    geom_tile(color = "white") + 
    ggtitle("Heat Map by Month and Day")
  
  


