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

#### Plots of raw Meteorological data time series ####

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


#### Temperature Anomalies ####
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
weekly_temp_percentile_95 <- quantile(weekly_temp_merged_data$tempAnomaly, 0.95, na.rm = TRUE)

weekly_temp_anomaly_data <- weekly_temp_merged_data %>%
  filter(tempAnomaly > weekly_temp_percentile_0)
weekly_temp_anomaly_data90 <- weekly_temp_merged_data %>%
  filter(tempAnomaly > weekly_temp_percentile_90)
weekly_temp_anomaly_data95 <- weekly_temp_merged_data %>%
  filter(tempAnomaly > weekly_temp_percentile_95)

min(weekly_temp_anomaly_data95$tempAnomaly)
max(weekly_temp_anomaly_data95$tempAnomaly)

temp_anomaly_data$year <- as.factor(temp_anomaly_data$year)
temp_anomaly_data$doy <- as.factor(temp_anomaly_data$doy)

weekly_temp_anomaly_data$year <- as.factor(weekly_temp_anomaly_data$year)
weekly_temp_anomaly_data$week <- as.numeric(weekly_temp_anomaly_data$week)


# creating a new column to group into drought vs non-drought years 
# based on their max value (e.g., >6.5 temp anomaly)

# Create new column 'drought_status' and initialise it with 'non-drought'

temp_anomaly_data_summer <- weekly_temp_anomaly_data[weekly_temp_anomaly_data$week >= 18 & weekly_temp_anomaly_data$week <= 36, ]
drought_years <- temp_anomaly_data_summer %>%
  filter(tempAnomaly > 6.5) %>%
  pull(year) %>%
  unique()
temp_anomaly_data_summer['Drought_Status'] = '1989-2020'
temp_anomaly_data_summer$Drought_Status[temp_anomaly_data_summer$year %in% drought_years] <- as.character(temp_anomaly_data_summer$year[temp_anomaly_data_summer$year %in% drought_years])
temp_anomaly_data_summer$Drought_Status <- as.factor(temp_anomaly_data_summer$Drought_Status)

min(temp_anomaly_data_summer$tempAnomaly)

ggplot(temp_anomaly_data_summer, aes(x = week, y = tempAnomaly, colour = Drought_Status, group = year)) +
  geom_line(size = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.7, colour = "black") +
  geom_hline(yintercept = 4.69, linetype = "dashed", size = 0.7, colour = "darkorange") +
  geom_text(aes(x = 35.2, y = -0.3, label = "Norm"), colour = "black") + 
  geom_text(aes(x = 34.5, y = 5.2, label = "95th percentile"), colour = "darkorange") + 
  labs(title = "Summer weekly temperature anomalies compared to 30 year average",
       x = "Summer months",
       y = "Temperature Anomaly (degrees Celsius)",
       colour = "Year:") +
  scale_colour_manual(values = c("#D4D4D4C4", "#B51717", "#0F5596", "#178A86")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=14, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 14, face = "bold", ),
        legend.text = element_text(size = 12)) +
  scale_x_continuous(breaks = c(18, 22, 27, 32, 36), 
                     labels = c("May", "Jun", "Jul", "Aug", "Sep"),
                     expand = c(0, 0),
                     limits = c(18, 36)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-6,8))







#### Deep Soil Moisture Anomalies ####

# Reference period 2000-2020
sm_reference_period <- sm_data %>%
  filter(year >= 2000 & year <= 2020) %>%
  group_by(doy) %>%
  summarize(smAverage = mean(SWC_1, na.rm = TRUE))

# Merge with the main data
sm_merged_data <- merge(sm_data, sm_reference_period, by = "doy", all.x = TRUE)

# Calculate soil moisture anomalies
sm_merged_data$smAnomaly <- sm_merged_data$SWC_1 - sm_merged_data$smAverage

# Calculate the 90th percentile of sm anomalies
sm_percentile_95 <- quantile(sm_merged_data$smAnomaly, 0.95, na.rm = TRUE)
sm_percentile_90 <- quantile(sm_merged_data$smAnomaly, 0.9, na.rm = TRUE)
sm_percentile_0 <- quantile(sm_merged_data$smAnomaly, 0, na.rm = TRUE)
sm_percentile_10 <- quantile(sm_merged_data$smAnomaly, 0.1, na.rm = TRUE)

# Filter only values in the upper 90th percentile
sm_anomaly_data <- sm_merged_data %>%
  filter(smAnomaly > sm_percentile_0)

sm_anomaly_data90 <- sm_merged_data %>%
  filter(smAnomaly > sm_percentile_90)

sm_anomaly_data95 <- sm_merged_data %>%
  filter(smAnomaly > sm_percentile_95)

sm_anomaly_data10 <- sm_merged_data %>%
  filter(smAnomaly > sm_percentile_10)

min(sm_anomaly_data95$smAnomaly)

sm_anomaly_data$year <- as.factor(sm_anomaly_data$year)
sm_anomaly_data$doy <- as.factor(sm_anomaly_data$doy)

sm_anomaly_data_filtered <- sm_anomaly_data[c("year", "doy", "smAverage", "smAnomaly")]
sm_anomaly_data_filtered$year <- as.factor(sm_anomaly_data_filtered$year)
sm_anomaly_data_filtered$doy <- as.numeric(sm_anomaly_data_filtered$doy)

# Create new column 'drought_status' and initialise it with 'non-drought'
sm_anomaly_data_summer <- sm_anomaly_data_filtered %>%
  filter(doy %in% c(18:36))

sm_drought_years <- sm_anomaly_data_summer%>%
  filter(year %in% c(2003,2010,2018)) %>%
  pull(year) %>%
  unique()

sm_anomaly_data_summer['Drought_Status'] = '2000-2020'

sm_anomaly_data_summer <- sm_anomaly_data_summer %>%
  mutate(Drought_Status = ifelse(year %in% sm_drought_years, as.character(year), Drought_Status))

sm_anomaly_data_summer$Drought_Status <- as.factor(sm_anomaly_data_summer$Drought_Status)
sm_anomaly_data_summer$year <- as.factor(sm_anomaly_data_summer$year)
sm_anomaly_data_summer$doy <- as.numeric(sm_anomaly_data_summer$doy)
sm_anomaly_data_summer$smAnomaly <- as.numeric(sm_anomaly_data_summer$smAnomaly)

sm_anomaly_data_filtered10 <- sm_anomaly_data_summer %>%
  filter(smAnomaly > sm_percentile_10)

deficit_data <- sm_anomaly_data_summer %>%
  filter(smAnomaly < 0)
drought_threshold <- quantile(deficit_data$smAnomaly, 0.05)
minimum_deficit <- deficit_data %>%
  mutate(Drought_Status = ifelse(smAnomaly < drought_threshold, "Drought", "No Drought"))

drought_threshold95 <- minimum_deficit %>%
  filter(Drought_Status == "Drought")

max(drought_threshold95$smAnomaly)

# threshold for 80th percentile is -8.06
# threshold for 90th percentile is -9.43
# threshold for 95th percentile is -9.93

# Plotting soil moisture anomalies
ggplot(sm_anomaly_data_summer, aes(x = doy, y = smAnomaly, group = year, colour = Drought_Status)) +
  geom_line(size = 0.7) +
  geom_hline(yintercept = 0, size = 0.3, colour = "black") +
  geom_hline(yintercept = -8.06, linetype = "dashed", size = 0.7, colour = "darkorange") +
  geom_text(aes(x = 35.2, y = 1, label = "Norm"), colour = "black") + 
  geom_text(aes(x = 34.2, y = -10, label = "80th percentile"), colour = "darkorange") +  
  labs(title = "Summer weekly deep soil moisture anomalies compared to 20 year average",
       x = "Summer months",
       y = "Deep Soil Moisture Anomaly (mm)",
       colour = "Year:") +
  scale_colour_manual(values = c("#D4D4D4C4", "#B51717", "#0F5596", "#178A86")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 14, face = "bold", ),
        legend.text = element_text(size = 12)) +
  scale_x_continuous(breaks = c(18, 22, 27, 32, 36), 
                     labels = c("May", "Jun", "Jul", "Aug", "Sep"),
                     expand = c(0, 0),
                     limits = c(18, 36)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-15,15))




