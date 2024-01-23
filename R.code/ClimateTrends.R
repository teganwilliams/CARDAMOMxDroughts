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
  geom_line(colour="red2") +
  labs(title = "Daily Climate Trends (1990-2020)",
       x = "Day of the Year",
       y = "Average Temperature") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5))

# ok now i can beautify this a bit, and then replicate it for precipitation and max T 
# once ive done that, i can extract the 'anomalies' from this weather dataset, so that i can attribute or define when the anomalies are in Hainich!

# lets try adding a year to this plot to compare values (e.g., 2003)

datafor2003 <- climate_data %>%
  filter(year(Date) == 2003)
datafor2018 <- climate_data %>%
  filter(year(Date) == 2018)
datafor2011 <- climate_data %>%
  filter(year(Date) == 2011)


combined_data1 <- merge(daily_averages, datafor2003, by = "DayOfYear", all.x = TRUE)
view(combined_data1)

ggplot() +
  geom_line(data = combined_data1, aes(x = as.numeric(DayOfYear), y = AverageTemperature), colour = "black") +
  geom_line(data = combined_data1, aes(x = as.numeric(DayOfYear), y = MeanT), colour = "red") +
  labs(title = "Daily Climate Trends vs 2003",
       x = "Day of the Year",
       y = "Air Temperature") +
  scale_color_manual(values = c("black", "red"), name = "Legend", labels = c("Average", "2003")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) +
  xlim(c(152, 243))+
  ylim(c(10,27))


combined_data2018 <- merge(daily_averages, datafor2018, by = "DayOfYear", all.x = TRUE)
view(combined_data2018)

ggplot() +
  geom_line(data = combined_data2018, aes(x = as.numeric(DayOfYear), y = AverageTemperature), colour = "black") +
  geom_line(data = combined_data2018, aes(x = as.numeric(DayOfYear), y = MeanT), colour = "red") +
  labs(title = "Daily Climate Trends vs 2018",
       x = "Day of the Year",
       y = "Air Temperature") +
  scale_color_manual(values = c("black", "red"), name = "Legend", labels = c("Average", "2018")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5))

combined_data2011 <- merge(daily_averages, datafor2011, by = "DayOfYear", all.x = TRUE)
view(combined_data2011)

ggplot() +
  geom_line(data = combined_data2011, aes(x = as.numeric(DayOfYear), y = AverageTemperature), colour = "black") +
  geom_line(data = combined_data2011, aes(x = as.numeric(DayOfYear), y = MeanT), colour = "red") +
  labs(title = "Daily Climate Trends vs 2011",
       x = "Day of the Year",
       y = "Air Temperature") +
  scale_color_manual(values = c("black", "red"), name = "Legend", labels = c("Average", "2011")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5))




