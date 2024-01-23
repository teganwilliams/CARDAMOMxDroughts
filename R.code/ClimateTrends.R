### Climate Trends and Anomalies
## By Tegan Williams January 2024
## Data for Göttingen weather station, supplied by Deutscher Wetterdienst

#### Step 1. Calculating daily averages for 1990 to 2020 (30 year trends) ####

# Libraries 
library(dplyr)
library(tidyverse)
library(ggplot2) 

# Load Data 
climate_data <- read_csv("Data/Göttingen_climate_trend_data.csv")
view(climate_data)

met_data <- read.csv("~/Desktop/Dissertation/Data/DE-Hai-2000-2020-weekly_timeseries_met.csv", header = TRUE)
obs_data <- read.csv("~/Desktop/Dissertation/Data/DE-Hai-2000-2020-weekly_timeseries_obs.csv", header = TRUE)
sm_data <- read.csv("~/Desktop/Dissertation/Data/DE-Hai-2000-2020-weekly_timeseries_metSM.csv", header= TRUE)


# Calculating Daily Trends

climate_data$Date <- as.Date(climate_data$Date)
climate_data$DayOfYear <- format(climate_data$Date, "%j")

daily_averages <- climate_data %>%
  group_by(DayOfYear) %>%
  summarize(AverageTemperature = mean(MeanT, na.rm = TRUE))

ggplot(daily_averages, aes(x = as.numeric(DayOfYear), y = AverageTemperature)) +
  geom_line() +
  labs(title = "Daily Climate Trends (1990-2020)",
       x = "Day of the Year",
       y = "Average Temperature")




