####### Anomalies plotted #######
### Amended/final code script ###
### Tegan Williams March 2024 ###

#### Data loading & wrangling ####

# Libraries (required packages for data wrangling)
library(dplyr)
library(tidyverse)
library(ggplot2) 

# Load Meteorological Data
met_data <- read.csv("Data/DE-Hai-2000-2020-weekly_timeseries_met.csv", header = TRUE)
obs_data <- read.csv("Data/DE-Hai-2000-2020-weekly_timeseries_obs.csv", header = TRUE)
sm_data <- read.csv("Data/DE-Hai-2000-2020-weekly_timeseries_metSM.csv", header= TRUE)
climate_data <- read_csv("Data/DE-Hai_FLUXNET2015_DD_1989-2020_met.csv")

# Delete 2021 data from Met data set since only NA values (-9999)
rows_to_delete <- c(1093:1144)
met_data <- met_data[-rows_to_delete, ]

# Create new column for year, months and full dates 
rows_per_year <- 52
met_data$year <- rep(2000:2020, each = rows_per_year)
met_data$full_date <- as.Date(paste0(met_data$year, "-", met_data$doy), format = "%Y-%j")
met_data <- met_data %>%
  mutate(month = month(full_date, label = TRUE))

# Now filter datasets for fewer years
met_data$year <- as.numeric(met_data$year)
met2000to2005 <- met_data[met_data$year >= 2000 & met_data$year <= 2005, ]
met2015to2020 <- met_data[met_data$year >= 2015 & met_data$year <= 2020, ]

met2003 <- met_data[met_data$year >= 2003 & met_data$year <= 2003, ]
met2018 <- met_data[met_data$year >= 2018 & met_data$year <= 2018, ]

#### Plots of raw met data timeseries ####

# Air Temperature over Time for 2000 to 2005
ggplot(met2000to2005, aes(x = full_date, y = airt_C)) +
  geom_line(colour="red2") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(title="Temperature trends 2000-2005") + 
  theme(plot.title=element_text(size=13, hjust=0.5)) + # Title size and position
  xlab("Time [year]") + 
  ylab("Air Temperature [°C]")

# Max Air Temperature over Time for 2000-2005 --> EXACTLY THE SAME TRENDS
ggplot(met2000to2005, aes(x = full_date, y = maxt_C)) +
  geom_line(colour="red2") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(title="Maximum Temperature trends 2000-2005") + 
  theme(plot.title=element_text(size=13, hjust=0.5)) + 
  xlab("Time [year]") + 
  ylab("Max Temperature [°C]")


# plotting T over time for 5 consecutive years e.g., 2000-2005, to show the difference during drought years 
# plus add the 30-year mean temperature as well!
avg_monthly_temp <- met_data %>%
  group_by(year, month) %>%
  summarise(monthlyT = mean(airt_C, na.rm = TRUE))

avg_monthly_temp$year <- as.numeric(avg_monthly_temp$year)
avg_monthly_temp_05 <- avg_monthly_temp[avg_monthly_temp$year >= 2000 & avg_monthly_temp$year <= 2005, ]
avg_monthly_temp_20 <- avg_monthly_temp[avg_monthly_temp$year >= 2015 & avg_monthly_temp$year <= 2020, ]

met_data_with_avg_temp_05 <- left_join(met2000to2005, avg_monthly_temp_05, by = c("year", "month"))

ggplot(met_data_with_avg_temp_05, aes(x = month, y = monthlyT, colour = as.factor(year), group = year)) +
  geom_line(size = 0.7) +
  geom_smooth(method = 'loess', aes(group=1), colour = "black", size = 0.7) +
  scale_color_manual(values = c("#FAEF17", "#FFD700","#FFA500","red", "#BA8659", "#EDC279")) +
  labs(x = "Month", y = "Air Temperature (°C)", title = "Air temperature over a year with drought years highlighted", colour = "Year") +
  scale_x_discrete(limits = c("May", "Jun", "Jul", "Aug", "Sep")) +
  scale_y_continuous(limits = c(9, 22)) +
  theme(legend.position = "right", panel.background = element_blank(), axis.line = element_line(colour = "black"))

# Using weekly values without the mean monthly 
ggplot(met_data_with_avg_temp_05, aes(x = doy, y = airt_C, colour = as.factor(year), group = year)) +
  geom_line(size = 0.7) +
  geom_smooth(method = 'loess', aes(group=1), colour = "black", size = 0.7) +
  scale_color_manual(values = c("#FAEF17", "#FFD700","#FFA500","red", "#BA8659", "#EDC279")) +
  labs(x = "Month", y = "Air Temperature (°C)", title = "Air temperature over a year with drought years highlighted", colour = "Year") +
  scale_x_continuous(breaks = c(121, 154, 182, 213, 242), 
                     labels = c("May", "Jun", "Jul", "Aug", "Sep"),
                     expand = c(0, 0),
                     limits = c(119, 245)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(5,30)) +
  theme(legend.position = "right", panel.background = element_blank(), axis.line = element_line(colour = "black"))


# Precipitation over Time
ggplot(met2000to2005, aes(x = full_date, y = precip_kgm2s)) +
  geom_line(colour="blue2") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) + # Title size and position
  labs(title="Precipitation trends 2000-2005") + 
  xlab("Time [year]") + 
  ylab("Precipitation [kg/m^2/s]")


#### Plotting Temperature Anomalies ####

# Reference period 1989-2020
climate_data <- climate_data %>%
  mutate(TIMESTAMP = as.character(TIMESTAMP),
         year = year(as.Date(TIMESTAMP, format = "%Y%m%d")),
         month = month(as.Date(TIMESTAMP, format = "%Y%m%d")),
         doy = yday(as.Date(TIMESTAMP, format = "%Y%m%d")))

daily_averages <- climate_data %>%
  group_by(doy) %>%
  summarize(AverageTemperature = mean(MeanT, na.rm = TRUE))

datafor2003 <- climate_data %>%
  filter(year == 2003)
datafor2018 <- climate_data %>%
  filter(year == 2018)
dataforboth <- climate_data %>%
  filter(year %in% c(2003, 2018))

temp_reference_period <- climate_data %>%
  filter(year >= 1989 & year <= 2020) %>%
  group_by(doy) %>%
  summarize(tAverage = mean(MeanT, na.rm = TRUE))

# Merge climate data with reference period
temp_merged_data <- merge(climate_data, temp_reference_period, by = "doy", all.x = TRUE)

# Calculate temperature anomalies
temp_merged_data$tempAnomaly <- temp_merged_data$MeanT - temp_merged_data$tAverage

# Calculate the 90th percentile of temperature anomalies
temp_percentile_90 <- quantile(temp_merged_data$tempAnomaly, 0.9, na.rm = TRUE)
temp_percentile_0 <- quantile(temp_merged_data$tempAnomaly, 0, na.rm = TRUE)

# Filter only values in the chosen percentile
temp_anomaly_data <- temp_merged_data %>%
  filter(tempAnomaly > temp_percentile_0)
temp_anomaly_data90 <- temp_merged_data %>%
  filter(tempAnomaly > temp_percentile_90)

min(temp_anomaly_data90$tempAnomaly)
max(temp_anomaly_data90$tempAnomaly)

temp_anomaly_data$year <- as.factor(temp_anomaly_data$year)
temp_anomaly_data$doy <- as.factor(temp_anomaly_data$doy)

temp_anomaly_data_filtered <- temp_anomaly_data[c("year", "doy", "tAverage", "tempAnomaly")]
temp_anomaly_data_filtered$doy <- as.numeric(temp_anomaly_data_filtered$doy)

# Plotting Temperature Anomalies over Time

# 1) using daily values
ggplot(temp_anomaly_data_filtered, aes(x = doy, y = tempAnomaly, colour = year)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
  geom_hline(yintercept = 4.63, linetype = "dashed", colour = "darkorange") +
  geom_text(aes(x = 235, y = -11, label = "90th Percentile"), colour = "darkorange") + 
  geom_text(aes(x = 242, y = 1, label = "Norm"), colour = "black") + 
  labs(title = "Summer Temperature Anomalies compared to 30 year average",
       x = "",
       y = "Daily Temp Anomaly (degrees C)") +
  scale_colour_manual(values = c("grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","deeppink","grey", "grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey", "grey","grey", "red2", "grey", "grey")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) +
  scale_x_continuous(breaks = c(154, 182, 213, 242), 
                     labels = c("Jun", "Jul", "Aug", "Sep"),
                     expand = c(0, 0),
                     limits = c(152, 245)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0,11))


# 2) using weekly temperature means to calculate anomalies
weekly_average_temp_data <- climate_data %>%
  group_by(week = ceiling(as.numeric(doy)/7)) %>%
  summarise(AverageWeeklyMeanT = mean(MeanT, na.rm = TRUE))

weekly_temp_data <- climate_data %>%
  group_by(year, week = ceiling(as.numeric(doy)/7)) %>%
  summarise(WeeklyMeanT = mean(MeanT, na.rm = TRUE))

# now find the anomaly values
weekly_temp_merged_data <- merge(weekly_average_temp_data, weekly_temp_data, by = "week", all.x = TRUE)
weekly_temp_merged_data$tempAnomaly <- weekly_temp_merged_data$WeeklyMeanT - weekly_temp_merged_data$AverageWeeklyMeanT
weekly_temp_percentile_90 <- quantile(weekly_temp_merged_data$tempAnomaly, 0.9, na.rm = TRUE)
weekly_temp_percentile_0 <- quantile(weekly_temp_merged_data$tempAnomaly, 0, na.rm = TRUE)

weekly_temp_anomaly_data <- weekly_temp_merged_data %>%
  filter(tempAnomaly > weekly_temp_percentile_0)
weekly_temp_anomaly_data90 <- weekly_temp_merged_data %>%
  filter(tempAnomaly > weekly_temp_percentile_90)

min(weekly_temp_anomaly_data90$tempAnomaly)
max(weekly_temp_anomaly_data90$tempAnomaly)

temp_anomaly_data$year <- as.factor(temp_anomaly_data$year)
temp_anomaly_data$doy <- as.factor(temp_anomaly_data$doy)

weekly_temp_anomaly_data$year <- as.factor(weekly_temp_anomaly_data$year)
weekly_temp_anomaly_data$week <- as.numeric(weekly_temp_anomaly_data$week)

ggplot(weekly_temp_anomaly_data, aes(x = week, y = tempAnomaly, colour = year)) +
  # geom_rect(aes(xmin = 119, xmax = 245, 
  #          ymin = -15, ymax = -6.98), 
  #    fill = "orange", alpha = 0.5) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
  geom_hline(yintercept = 3.63, linetype = "dashed", colour = "darkorange") +
  geom_text(aes(x = 242, y = 1, label = "Norm"), colour = "black") + 
  geom_text(aes(x = 235, y = -11, label = "90th Percentile"), colour = "darkorange") + 
  labs(title = "Summer Weekly Temperature Anomalies compared to 30 year average",
       x = "",
       y = "Temperature Anomaly (degrees Celsius)") +
  scale_colour_manual(values = c("grey","grey","grey","grey","grey", "blue","grey","grey","grey","grey","grey","grey","grey","grey","deeppink","grey","grey","grey","grey","grey","grey","grey","grey","grey", "grey", "grey","grey","grey","grey","red2", "grey", "grey")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) +
  scale_x_continuous(breaks = c(18, 22, 27, 32, 36), 
                     labels = c("May", "Jun", "Jul", "Aug", "Sep"),
                     expand = c(0, 0),
                     limits = c(18, 36)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-5,8))

View(weekly_temp_anomaly_data)









