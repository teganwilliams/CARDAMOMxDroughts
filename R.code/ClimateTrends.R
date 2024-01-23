### Climate Trends and Anomalies
## By Tegan Williams January 2024
## Data for Göttingen weather station, supplied by Deutscher Wetterdienst

#### Step 1. Calculating daily Temperature Averages for 1989 to 2020 ####

# Libraries 
library(dplyr)
library(tidyverse)
library(ggplot2) 

# Load Data 
climate_data <- read_csv("Data/DE-Hai_FLUXNET2015_DD_1989-2020_met.csv")
View(climate_data)

met_data <- read.csv("~/Desktop/Dissertation/Data/DE-Hai-2000-2020-weekly_timeseries_met.csv", header = TRUE)
obs_data <- read.csv("~/Desktop/Dissertation/Data/DE-Hai-2000-2020-weekly_timeseries_obs.csv", header = TRUE)
sm_data <- read.csv("~/Desktop/Dissertation/Data/DE-Hai-2000-2020-weekly_timeseries_metSM.csv", header= TRUE)

# Editing the Hainich met dataset to include DOY, month and year columns

library(lubridate)

climate_data <- climate_data %>%
  mutate(TIMESTAMP = as.character(TIMESTAMP),
         year = year(as.Date(TIMESTAMP, format = "%Y%m%d")),
         month = month(as.Date(TIMESTAMP, format = "%Y%m%d")),
         doy = yday(as.Date(TIMESTAMP, format = "%Y%m%d")))

# Calculating Daily Trends for the whole year 

daily_averages <- climate_data %>%
  group_by(doy) %>%
  summarize(AverageTemperature = mean(MeanT, na.rm = TRUE))

ggplot(daily_averages, aes(x = as.numeric(doy), y = AverageTemperature)) +
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
  filter(year == 2003)
datafor2018 <- climate_data %>%
  filter(year == 2018)
datafor2011 <- climate_data %>%
  filter(year == 2011)
datafor2008 <- climate_data %>%
  filter(year == 2008)
datafor1989 <- climate_data %>%
  filter(year == 1989)

combined_data1 <- merge(daily_averages, datafor2003, by = "doy", all.x = TRUE)
view(combined_data1)

ggplot() +
  geom_line(data = combined_data1, aes(x = as.numeric(doy), y = AverageTemperature), colour = "black") +
  geom_line(data = combined_data1, aes(x = as.numeric(doy), y = MeanT), colour = "red") +
  labs(title = "Daily Climate Trends vs 2003",
       x = "Day of the Year",
       y = "Air Temperature") +
  scale_color_manual(values = c("black", "red"), name = "Legend", labels = c("Average", "2003")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) +
  xlim(c(152, 243))+
  ylim(c(8,27))


combined_data2018 <- merge(daily_averages, datafor2018, by = "doy", all.x = TRUE)
view(combined_data2018)

ggplot() +
  geom_line(data = combined_data2018, aes(x = as.numeric(doy), y = AverageTemperature), colour = "black") +
  geom_line(data = combined_data2018, aes(x = as.numeric(doy), y = MeanT), colour = "red") +
  labs(title = "Daily Climate Trends vs 2018",
       x = "Day of the Year",
       y = "Air Temperature") +
  scale_color_manual(values = c("black", "red"), name = "Legend", labels = c("Average", "2018")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5))+
  xlim(c(152, 243))+
  ylim(c(8,27))

combined_data2011 <- merge(daily_averages, datafor2011, by = "doy", all.x = TRUE)
view(combined_data2011)

ggplot() +
  geom_line(data = combined_data2011, aes(x = as.numeric(doy), y = AverageTemperature), colour = "black") +
  geom_line(data = combined_data2011, aes(x = as.numeric(doy), y = MeanT), colour = "red") +
  labs(title = "Daily Climate Trends vs 2011",
       x = "Day of the Year",
       y = "Air Temperature") +
  scale_color_manual(values = c("black", "red"), name = "Legend", labels = c("Average", "2011")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5))+
  xlim(c(152, 243))+
  ylim(c(8,27))

combined_data2008 <- merge(daily_averages, datafor2008, by = "doy", all.x = TRUE)
ggplot() +
  geom_line(data = combined_data2008, aes(x = as.numeric(doy), y = AverageTemperature), colour = "black") +
  geom_line(data = combined_data2008, aes(x = as.numeric(doy), y = MeanT), colour = "red") +
  labs(title = "Daily Climate Trends vs 2008",
       x = "Day of the Year",
       y = "Air Temperature") +
  scale_color_manual(values = c("black", "red"), name = "Legend", labels = c("Average", "2008")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5))+
  xlim(c(152, 243))+
  ylim(c(8,27))

combined_data1989 <- merge(daily_averages, datafor1989, by = "doy", all.x = TRUE)
ggplot() +
  geom_line(data = combined_data1989, aes(x = as.numeric(doy), y = AverageTemperature), colour = "black") +
  geom_line(data = combined_data1989, aes(x = as.numeric(doy), y = MeanT), colour = "red") +
  labs(title = "Daily Climate Trends vs 1989",
       x = "Day of the Year",
       y = "Air Temperature") +
  scale_color_manual(values = c("black", "red"), name = "Legend", labels = c("Average", "2008")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5))+
  xlim(c(152, 243))+
  ylim(c(8,27))


# now overlaying all the years of data + climate trend

# Merge with daily averages
library(lubridate)
combined_data_all_years <- merge(climate_data, daily_averages, by = "doy", all.x = TRUE)
View(combined_data_all_years)
combined_data_all_years$YearFactor <- factor(combined_data_all_years$year)

ggplot() +
  geom_line(data = combined_data_all_years, aes(x = as.numeric(doy), y = AverageTemperature), size = 1, colour = "black") +
  geom_line(data = combined_data_all_years, aes(x = as.numeric(doy), y = MeanT, group = year, colour =as.factor(year)), alpha = 0.5) +
  labs(title = "Summer Climate Trends - Individual Years and Average",
       x = "Day of the Year",
       y = "Temperature") +
  scale_color_manual(values = c("grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","pink", "#FAEF17","grey", "#FFD700","grey","grey","grey","#FFA500" ,"grey","grey","grey","grey","#FF8C00","grey","grey","#D9534F","#FF4640","#FF001A"))+
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5))+
  xlim(c(152, 243))+
  ylim(c(8,30))

#### Precipitation ####

# Calculating Daily Trends for the whole year 

daily_averages_P <- climate_data %>%
  group_by(doy) %>%
  summarize(MeanPrecip = mean(Precip, na.rm = TRUE))

ggplot(daily_averages_P, aes(x = as.numeric(doy), y = MeanPrecip)) +
  geom_line(colour="blue2") +
  labs(title = "Daily Climate Trends (1990-2020)",
       x = "Day of the Year",
       y = "Mean Precipitation") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5))

P2003 <- merge(daily_averages_P, datafor2003, by = "doy", all.x = TRUE)
View(P2003)

ggplot() +
  geom_line(data = P2003, aes(x = as.numeric(doy), y = MeanPrecip), colour = "black") +
  geom_line(data = P2003, aes(x = as.numeric(doy), y = Precip), colour = "blue2") +
  labs(title = "Daily Climate Trends vs 2003",
       x = "Day of the Year",
       y = "Precipitation") +
  scale_color_manual(values = c("black", "blue2"), name = "Legend", labels = c("Average", "2003")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) +
  xlim(c(152, 243)) +
  ylim(c(0,20))


P2018 <- merge(daily_averages_P, datafor2018, by = "doy", all.x = TRUE)

ggplot() +
  geom_line(data = P2018, aes(x = as.numeric(doy), y = MeanPrecip), colour = "black") +
  geom_line(data = P2018, aes(x = as.numeric(doy), y = Precip), colour = "blue2") +
  labs(title = "Daily Climate Trends vs 2018",
       x = "Day of the Year",
       y = "Precipitation") +
  scale_color_manual(values = c("black", "blue2"), name = "Legend", labels = c("Average", "2018")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) +
  xlim(c(152, 243)) +
  ylim(c(0,20))

P2011 <- merge(daily_averages_P, datafor2011, by = "doy", all.x = TRUE)

ggplot() +
  geom_line(data = P2011, aes(x = as.numeric(doy), y = MeanPrecip), colour = "black") +
  geom_line(data = P2011, aes(x = as.numeric(doy), y = Precip), colour = "blue2") +
  labs(title = "Daily Climate Trends vs 2011",
       x = "Day of the Year",
       y = "Precipitation") +
  scale_color_manual(values = c("black", "blue2"), name = "Legend", labels = c("Average", "2011")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) +
  xlim(c(152, 243)) +
  ylim(c(0,20))


P2008 <- merge(daily_averages_P, datafor2008, by = "doy", all.x = TRUE)
ggplot() +
  geom_line(data = P2008, aes(x = as.numeric(doy), y = MeanPrecip), colour = "black") +
  geom_line(data = P2008, aes(x = as.numeric(doy), y = Precip), colour = "blue2") +
  labs(title = "Daily Climate Trends vs 2008",
       x = "Day of the Year",
       y = "Precipitation") +
  scale_color_manual(values = c("black", "blue2"), name = "Legend", labels = c("Average", "2008")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) +
  xlim(c(152, 243)) +
  ylim(c(0,20))

P1989 <- merge(daily_averages_P, datafor1989, by = "doy", all.x = TRUE)
ggplot() +
  geom_line(data = P1989, aes(x = as.numeric(doy), y = MeanPrecip), colour = "black") +
  geom_line(data = P1989, aes(x = as.numeric(doy), y = Precip), colour = "blue2") +
  labs(title = "Daily Climate Trends vs 1989",
       x = "Day of the Year",
       y = "Precipitation") +
  scale_color_manual(values = c("black", "blue2"), name = "Legend", labels = c("Average", "2008")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) +
  xlim(c(152, 243)) +
  ylim(c(0,17))

ggplot() +
  geom_line(data = P2018, aes(x = as.numeric(doy), y = MeanPrecip), colour = "black") +
  geom_line(data = P2018, aes(x = as.numeric(doy), y = Precip), colour = "blue2") +
  labs(title = "Daily Climate Trends vs 2018",
       x = "Day of the Year",
       y = "Precipitation") +
  scale_color_manual(values = c("black", "blue2"), name = "Legend", labels = c("Average", "2018")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) +
  xlim(c(152, 243)) +
  ylim(c(0,17))



# now overlaying all the years of data + climate trend

# Merge with daily averages
library(lubridate)
combined_data_all_years_P <- merge(climate_data, daily_averages_P, by = "doy", all.x = TRUE)
View(combined_data_all_years)
combined_data_all_years$YearFactor <- factor(combined_data_all_years$year)

ggplot() +
  geom_line(data = combined_data_all_years_P, aes(x = as.numeric(doy), y = MeanPrecip), size = 1, colour = "black") +
  geom_line(data = combined_data_all_years_P, aes(x = as.numeric(doy), y = Precip, group = year, colour =as.factor(year)), alpha = 0.5) +
  labs(title = "Summer Climate Trends - Individual Years and Average",
       x = "Day of the Year",
       y = "Precip") +
  scale_color_manual(values = c("grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","blue", "cyan","grey", "darkblue","grey","grey","grey","green" ,"grey","grey","grey","grey", "darkgreen","grey","grey", "#337AFF", "#0098C2", "#4FD6FF")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5))+
  xlim(c(152, 243))+
  ylim(c(0,25))



#### Anomalies ####

# Choose a reference period 1990-2020
reference_period <- climate_data %>%
  filter(year >= 1989 & year <= 2020) %>%
  group_by(doy) %>%
  summarize(ReferenceAverage = mean(MeanT, na.rm = TRUE))

# Merge with the main data
merged_data <- merge(climate_data, reference_period, by = "doy", all.x = TRUE)

merged_data_hainich <- merge(met_data, reference_period, by = "doy", all.x = TRUE)

# Calculate temperature anomalies
merged_data$TemperatureAnomaly <- merged_data$MeanT - merged_data$ReferenceAverage

# Calculate the 90th percentile of temperature anomalies
percentile_90 <- quantile(merged_data$TemperatureAnomaly, 0.9, na.rm = TRUE)

# Filter only values in the upper 90th percentile
anomaly_data <- merged_data %>%
  filter(TemperatureAnomaly > percentile_90)

# Plotting anomalies over time

ggplot(anomaly_data, aes(x = doy, y = TemperatureAnomaly)) +
  geom_line() +
  labs(title = "Temperature Anomalies (Upper 90th Percentile)",
       x = "doy",
       y = "Temperature Anomaly")




#### Using the Göttingen data ####

# load dataset

climate_data <- read_csv("Data/Göttingen_climate_trend_data.csv")
view(climate_data)

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
datafor2008 <- climate_data %>%
  filter(year(Date) == 2008)


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
        plot.title=element_text(size=13, hjust=0.5))+
  xlim(c(152, 243))+
  ylim(c(10,27))

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
        plot.title=element_text(size=13, hjust=0.5))+
  xlim(c(152, 243))+
  ylim(c(10,27))

combined_data2008 <- merge(daily_averages, datafor2008, by = "DayOfYear", all.x = TRUE)
ggplot() +
  geom_line(data = combined_data2008, aes(x = as.numeric(DayOfYear), y = AverageTemperature), colour = "black") +
  geom_line(data = combined_data2008, aes(x = as.numeric(DayOfYear), y = MeanT), colour = "red") +
  labs(title = "Daily Climate Trends vs 2008",
       x = "Day of the Year",
       y = "Air Temperature") +
  scale_color_manual(values = c("black", "red"), name = "Legend", labels = c("Average", "2008")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5))+
  xlim(c(152, 243))+
  ylim(c(10,27))

# now overlaying all the years of data + climate trend

# Merge with daily averages
library(lubridate)
climate_data$Year <- year(climate_data$Date)
combined_data_all_years <- merge(climate_data, daily_averages, by = "DayOfYear", all.x = TRUE)
View(combined_data_all_years)
combined_data_all_years$YearFactor <- factor(combined_data_all_years$Year)
install.packages("lubridate")

ggplot() +
  geom_line(data = combined_data_all_years, aes(x = as.numeric(DayOfYear), y = AverageTemperature), size = 1, colour = "black") +
  geom_line(data = combined_data_all_years, aes(x = as.numeric(DayOfYear), y = MeanT, group = Year, colour =as.factor(Year)), alpha = 0.5) +
  labs(title = "Summer Climate Trends - Individual Years and Average",
       x = "Day of the Year",
       y = "Temperature") +
  scale_color_manual(values = c("grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","pink", "#FAEF17","grey", "#FFD700","grey","grey","grey","#FFA500" ,"grey","grey","grey","grey","#FF8C00","grey","grey","#D9534F","#FF4640","#FF001A"))+
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5))+
  xlim(c(152, 243))+
  ylim(c(8,30))

library(dplyr)
library(lubridate)


# Choose a reference period 1990-2020
reference_period <- climate_data %>%
  filter(Year >= 1990 & Year <= 2020) %>%
  group_by(DayOfYear) %>%
  summarize(ReferenceAverage = mean(MeanT, na.rm = TRUE))

# Merge with the main data
merged_data <- merge(climate_data, reference_period, by = "DayOfYear", all.x = TRUE)

merged_data_hainich <- merge(met_data, reference_period, by = "DayOfYear", all.x = TRUE)

# Calculate temperature anomalies
merged_data$TemperatureAnomaly <- merged_data$MeanT - merged_data$ReferenceAverage

# Calculate the 90th percentile of temperature anomalies
percentile_90 <- quantile(merged_data$TemperatureAnomaly, 0.9, na.rm = TRUE)

# Filter only values in the upper 90th percentile
anomaly_data <- merged_data %>%
  filter(TemperatureAnomaly > percentile_90)

# Plotting anomalies over time

ggplot(anomaly_data, aes(x = Date, y = TemperatureAnomaly)) +
  geom_line() +
  labs(title = "Temperature Anomalies (Upper 90th Percentile)",
       x = "Date",
       y = "Temperature Anomaly")





